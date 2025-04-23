if not ffi then
   gui.notify:add(
        gui.notification(
            'WARNING!', 
            'TURN ON ALLOW INSECURE IN LUA AND RELOAD SCRIPT!', 
            draw.textures['icon_close']
    ))
    error("enable ffi for lua to work")
end

local GetModuleHandleA = ffi.cast('uint64_t(__stdcall*)(const char*)', utils.find_export('kernel32.dll', 'GetModuleHandleA'));

local client = GetModuleHandleA('client.dll');

--[[
    IF SCRIPT DOES NOT WORK, UPDATE OFFSETS IN CLIENT_DLL FROM SPECIFIED LINKS
]]

local client_dll = {
    dwEntityList = 0x1A1F670, -- https://github.com/a2x/cs2-dumper/blob/main/output/offsets.hpp
    m_fireCount = 0x16D8, -- https://github.com/a2x/cs2-dumper/blob/main/output/client_dll.hpp
    m_firePositions = 0x0D98 -- https://github.com/a2x/cs2-dumper/blob/main/output/client_dll.hpp
}

ffi.cdef([[
    typedef struct {
        float x, y, z;
    } Vec3;
]])

local enabled = gui.checkbox(gui.control_id('3d-molotov-area-esp'));
local fill_colour = gui.color_picker(gui.control_id('3d-molotov-area-esp-fill'));
local outline_colour = gui.color_picker(gui.control_id('3d-molotov-area-esp-out'));
local rounding = gui.slider(gui.control_id('3d-molotov-smoothing-slider'), 0, 4, {'%.0f'}, 1);

local row = gui.make_control('Enable Molotov ESP', enabled);
local rounding_row = gui.make_control('Rounding', rounding);
row:add(fill_colour)
row:add(outline_colour)

local group = gui.ctx:find('lua>elements a');

group:add(row);
group:add(rounding_row);
group:reset();

local function orientation(p, q, r)
    return (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y)
end

local function distance_sq(p1, p2)
    local dx = p1.x - p2.x
    local dy = p1.y - p2.y
    return dx * dx + dy * dy
end

local function gift_wrap_hull(points)
    local n = #points
    if n < 3 then return points end

    local hull = {}

    local l = 1
    for i = 2, n do
        if points[i].x < points[l].x or (points[i].x == points[l].x and points[i].y < points[l].y) then
            l = i
        end
    end

    local p = l
    repeat
        table.insert(hull, points[p])
        local q = (p % n) + 1
        for i = 1, n do
            local orient = orientation(points[p], points[i], points[q])
            if orient < 0 then
                q = i
            elseif orient == 0 then
                if distance_sq(points[p], points[i]) > distance_sq(points[p], points[q]) then
                    q = i
                end
            end
        end
        p = q
    until p == l

    return hull
end

local render = {} render.__index = render

render.point_in_triangle = function(vertex_1, vertex_2, vertex_3, point)
    local a = ((vertex_2.y - vertex_3.y) * (point.x - vertex_3.x) + (vertex_3.x - vertex_2.x) * (point.y - vertex_3.y)) /
            ((vertex_2.y - vertex_3.y) * (vertex_1.x - vertex_3.x) + (vertex_3.x - vertex_2.x) * (vertex_1.y - vertex_3.y))

    local b = ((vertex_3.y - vertex_1.y) * (point.x - vertex_3.x) + (vertex_1.x - vertex_3.x) * (point.y - vertex_3.y)) /
            ((vertex_2.y - vertex_3.y) * (vertex_1.x - vertex_3.x) + (vertex_3.x - vertex_2.x) * (vertex_1.y - vertex_3.y))

    local c = 1 - a - b
    return a >= 0 and b >= 0 and c >= 0
end

render.is_poly_ear = function(vertices, i, n)
    local prev = vertices[i == 1 and n or i - 1]
    local curr = vertices[i]
    local next = vertices[i % n + 1]
    
    if ((curr.x - prev.x) * (next.y - curr.y) - 
        (curr.y - prev.y) * (next.x - curr.x)) < 0 then
        return false
    end
    
    for j = 1, n do
        if (j ~= i and j ~= (i % n + 1) and j ~= (i == 1 and n or i - 1)) then
            if (render.point_in_triangle(prev, curr, next, vertices[j])) then
                return false
            end
        end
    end
    
    return true
end

render.triangulate_convex = function(vertices)
    if (#vertices < 3) then return {} end
    
    local triangles = {}
    local first_vertex = vertices[1]
    
    for i = 2, #vertices - 1 do
        table.insert(triangles, {
            draw.vec2(first_vertex.x, first_vertex.y),
            draw.vec2(vertices[i].x, vertices[i].y),
            draw.vec2(vertices[i + 1].x, vertices[i + 1].y)
        })
    end
    
    return triangles
end

render.triangulate = function(vertices)
    if (#vertices < 3) then return {} end
    
    local is_convex = true
    for i = 1, #vertices do
        local prev = vertices[(i == 1) and #vertices or i - 1]
        local curr, next = vertices[i], vertices[i % #vertices + 1]
        if ((curr.x - prev.x) * (next.y - curr.y) - 
            (curr.y - prev.y) * (next.x - curr.x)) < 0 then
            is_convex = false
            break
        end
    end
    
    if (is_convex) then
        return render.triangulate_convex(vertices)
    end
    
    local remaining = {}
    for i = 1, #vertices do
        remaining[i] = i
    end
    
    local iterations, triangles = 0, {}
    
    while (#remaining > 3 and iterations < #vertices) do
        iterations = iterations + 1
        for i = 1, #remaining do
            if (render.is_poly_ear(vertices, remaining[i], #remaining)) then
                local prev = vertices[remaining[(i == 1) and #remaining or i - 1]]
                local curr = vertices[remaining[i]]
                local next = vertices[remaining[i % #remaining + 1]]
                
                table.insert(triangles, {
                    draw.vec2(prev.x, prev.y),
                    draw.vec2(curr.x, curr.y),
                    draw.vec2(next.x, next.y)
                })
                
                table.remove(remaining, i)
                break
            end
        end
    end
    
    if (#remaining == 3) then
        table.insert(triangles, {
            draw.vec2(vertices[remaining[1]].x, vertices[remaining[1]].y),
            draw.vec2(vertices[remaining[2]].x, vertices[remaining[2]].y),
            draw.vec2(vertices[remaining[3]].x, vertices[remaining[3]].y)
        })
    end
    
    return triangles
end

render.polygon = function(color, filled, ...)
    local vertex_table = { ... }
    local layer = draw.surface

    local triangles = render.triangulate(vertex_table)

    for _, triangle in ipairs(triangles) do
        if (filled) then
            layer:add_triangle_filled(triangle[1], triangle[2], triangle[3], color)
        else
            layer:add_triangle(triangle[1], triangle[2], triangle[3], color)
        end
    end
end

render.polygon_line = function(color, ...)
    render.polygon(color, false, ...)
end

render.polygon_filled = function(color, ...)
    render.polygon(color, true, ...)
end

local function distance_sq(p1, p2)
    local dx = p1.x - p2.x
    local dy = p1.y - p2.y
    local dz = p1.z - p2.z
    return dx * dx + dy * dy + dz * dz
end

local function clean_points_3d(points, threshold)
    threshold = threshold or 1.0
    local cleaned = {}
    for _, p in ipairs(points) do
        local too_close = false
        for _, cp in ipairs(cleaned) do
            if distance_sq(p, cp) < threshold * threshold then
                too_close = true
                break
            end
        end
        if not too_close then
            table.insert(cleaned, p)
        end
    end
    return cleaned
end

local function compute_centroid_3d(points)
    local cx, cy, cz = 0, 0, 0
    for _, p in ipairs(points) do
        cx = cx + p.x
        cy = cy + p.y
        cz = cz + p.z
    end
    local n = #points
    return { x = cx / n, y = cy / n, z = cz / n }
end

local function normalize(v)
    local len = math.sqrt(v.x * v.x + v.y * v.y + v.z * v.z)
    if len == 0 then return {x = 0, y = 0, z = 0} end
    return { x = v.x / len, y = v.y / len, z = v.z / len }
end

local function expand_points_3d(points, amount)
    local center = compute_centroid_3d(points)
    local expanded = {}
    for _, p in ipairs(points) do
        local dir = normalize({
            x = p.x - center.x,
            y = p.y - center.y,
            z = p.z - center.z
        })
        table.insert(expanded, {
            x = p.x + dir.x * amount,
            y = p.y + dir.y * amount,
            z = p.z + dir.z * amount
        })
    end
    return expanded
end

local function draw_smoothed_inferno(fire_points, fire_count, layer)
    if fire_points == nil then return end
    if #fire_points < 3 then return end

    local function clean_points(points, threshold)
        threshold = threshold or 1.0
        local cleaned = {}
        for _, p in ipairs(points) do
            local too_close = false
            for _, cp in ipairs(cleaned) do
                local dx = p.x - cp.x
                local dy = p.y - cp.y
                if dx * dx + dy * dy < threshold * threshold then
                    too_close = true
                    break
                end
            end
            if not too_close then
                table.insert(cleaned, p)
            end
        end
        return cleaned
    end

    local function chaikin_smooth(points, iterations)
        for _ = 1, iterations or 1 do
            local new_points = {}
            for i = 1, #points do
                local p1 = points[i]
                local p2 = points[(i % #points) + 1]
                local q = {
                    x = 0.75 * p1.x + 0.25 * p2.x,
                    y = 0.75 * p1.y + 0.25 * p2.y
                }
                local r = {
                    x = 0.25 * p1.x + 0.75 * p2.x,
                    y = 0.25 * p1.y + 0.75 * p2.y
                }
                table.insert(new_points, q)
                table.insert(new_points, r)
            end
            points = new_points
        end
        return points
    end

    local cleaned = clean_points_3d(fire_points, 2.0)
    local expanded = expand_points_3d(cleaned, 75)
    local final_points_2d = {}

    for _, v in pairs(expanded) do
        local vec = vector(v.x, v.y, v.z)
        local screen_vec = math.world_to_screen(vec)

        table.insert(final_points_2d, {x = screen_vec.x, y = screen_vec.y})
    end
    local cleaned = clean_points(final_points_2d, 2)
    local convex_hull = gift_wrap_hull(cleaned)
    local smoothed = chaikin_smooth(convex_hull, rounding:get_value():get())
    local final_points = gift_wrap_hull(smoothed)

    local old = nil

    for _, v in pairs(final_points) do
        if old == nil then 
            old = v
            goto continue 
        end

        layer:add_line(
            draw.vec2(old.x, old.y),
            draw.vec2(v.x, v.y),
            outline_colour:get_value():get()
        )

        old = v

        ::continue::
    end

    layer:add_line(
            draw.vec2(old.x, old.y),
            draw.vec2(final_points[1].x, final_points[1].y),
            draw.color(255, 0, 0)
        )

    render.polygon_filled(fill_colour:get_value():get(), unpack(final_points))
end

local eIDs = {}
local lastLoop = game.global_vars.real_time

local function is_valid_ptr(ptr)
    local addr = tonumber(ffi.cast("uintptr_t", ptr))
    return addr ~= nil and addr > 0x10000 and addr < 0x7FFFFFFFFFFF
end

local function safe_cast(ptr, ctype)
    if not is_valid_ptr(ptr) then return nil end
    return ffi.cast(ctype, ptr)
end

local function get_fire_points(eID)
    if eID == nil then return nil end

    local dwEntityList_ptr = safe_cast(client + client_dll.dwEntityList, "uintptr_t*")[0]
    if not dwEntityList_ptr then
        print("Invalid entity list base")
        eIDs[eID] = nil
        return nil
    end

    local list_entry_index = 8 * bit32.rshift(bit32.band(eID, 0x7FFF), 9) + 16
    local list_entry_ptr = safe_cast(dwEntityList_ptr + list_entry_index, "uintptr_t*")[0]
    if not list_entry_ptr then
        print("Invalid list entry")
        eIDs[eID] = nil
        return nil
    end

    local base_entity_offset = 0x78 * bit32.band(eID, 0x1FF)
    local C_BaseEntity = safe_cast(list_entry_ptr + base_entity_offset, "uintptr_t*")[0]
    if not C_BaseEntity then
        print("Invalid base entity")
        eIDs[eID] = nil
        return nil
    end

    local fire_count_ptr = ffi.cast("int*", C_BaseEntity + client_dll.m_fireCount)
    if not is_valid_ptr(fire_count_ptr) then
        print("Invalid fire count pointer")
        eIDs[eID] = nil
        return nil
    end

    local fire_count = fire_count_ptr[0]
    if fire_count == 0 then
        print("Fire count is zero")
        eIDs[eID] = nil
        return nil
    end

    local fire_positions_ptr = ffi.cast("Vec3*", C_BaseEntity + client_dll.m_firePositions)
    if not is_valid_ptr(fire_positions_ptr) then
        print("Invalid fire position pointer")
        eIDs[eID] = nil
        return nil
    end

    local fire_points = {}

    for i = 0, fire_count - 1 do
        local pos = fire_positions_ptr[i]
        local vec = vector(pos.x, pos.y, pos.z)
        table.insert(fire_points, vec)
    end

    eIDs[eID] = {fire_points, fire_count}
end

local function on_present_queue()
    local layer = draw.surface
    local should_update = game.global_vars.real_time - lastLoop > 0.2

    if should_update then
        lastLoop = game.global_vars.real_time
        for i, _ in pairs(eIDs) do print(i) end
    end

    for eID, points in pairs(eIDs) do
        if should_update then
            get_fire_points(eID)
        end

        local data = eIDs[eID]
        if data and data[1] and data[2] then
            draw_smoothed_inferno(data[1], data[2], layer)
        end
    end
end

events.present_queue:add(on_present_queue)
mods.events:add_listener('inferno_startburn')
mods.events:add_listener('inferno_extinguish')
mods.events:add_listener('inferno_expire')

events.event:add(function(event)
    if event:get_name() == "game_newmap" then
        client = GetModuleHandleA('client.dll');
    end
    if event:get_name() == "inferno_startburn" then
        local entityID = event:get_int('entityid')
        eIDs[entityID] = {}
        lastLoop = 0
    end
    if event:get_name() == "inferno_extinguish" then
        local entityID = event:get_int('entityid')
        eIDs[entityID] = nil
    end
    if event:get_name() == "inferno_expire" then
        local entityID = event:get_int('entityid')
        eIDs[entityID] = nil
    end
end)


if fill_colour:get_value():get() == draw.color(0, 0, 0, 0) then
    fill_colour:get_value():set(draw.color(255, 0, 0, 50))
end
if outline_colour:get_value():get() == draw.color(0, 0, 0, 0) then
    outline_colour:get_value():set(draw.color(255, 0, 0, 255))
end
if rounding:get_value():get() == 0 then
    rounding:get_value():set(3)
end 
