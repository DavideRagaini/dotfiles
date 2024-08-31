-- Source: https://github.com/wis/mpvSockets
local utils = require 'mp.utils'

local function get_temp_path()
    local directory_seperator = package.config:match("([^\n]*)\n?")
    local example_temp_file_path = os.tmpname()

    -- remove generated temp file
    pcall(os.remove, example_temp_file_path)

    local seperator_idx = example_temp_file_path:reverse():find(directory_seperator)
    local temp_path_length = #example_temp_file_path - seperator_idx

    return example_temp_file_path:sub(1, temp_path_length)
end

local tempDir = get_temp_path()

local function join_paths(...)
    local arg={...}
    local path = ""
    for i,v in ipairs(arg) do
        path = utils.join_path(path, tostring(v))
    end
    return path;
end

local pid = utils.getpid()
local socket_dir = "mpv"
os.execute("mkdir -p " .. join_paths(tempDir, socket_dir) .. " 2>/dev/null")
mp.set_property("options/input-ipc-server", join_paths(tempDir, socket_dir, pid))

local function shutdown_handler()
    os.remove(join_paths(tempDir, socket_dir, pid))
end
mp.register_event("shutdown", shutdown_handler)
