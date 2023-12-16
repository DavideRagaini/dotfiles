----- string

function is_empty(input)
    if input == nil or input == "" then
        return true
    end
end

function trim(input)
    if is_empty(input) then
        return ""
    end

    return input:match "^%s*(.-)%s*$"
end

function contains(input, find)
    if not is_empty(input) and not is_empty(find) then
        return input:find(find, 1, true)
    end
end

function replace(str, what, with)
    if is_empty(str) then return "" end
    if is_empty(what) then return str end
    if with == nil then with = "" end
    what = string.gsub(what, "[%(%)%.%+%-%*%?%[%]%^%$%%]", "%%%1")
    with = string.gsub(with, "[%%]", "%%%%")
    return string.gsub(str, what, with)
end

function split(input, sep)
    local tbl = {}

    if not is_empty(input) then
        for str in string.gmatch(input, "([^" .. sep .. "]+)") do
            table.insert(tbl, str)
        end
    end

    return tbl
end

function pad_left(input, len, char)
    if input == nil then
        input = ""
    end

    if char == nil then
        char = ' '
    end

    return string.rep(char, len - #input) .. input
end

----- file

function file_exists(path)
    local f = io.open(path, "r")

    if f ~= nil then
        io.close(f)
        return true
    end
end

function file_append(path, content)
    local h = assert(io.open(path, 'r'))
    local lines = {}
    for line in h:lines() do
        table.insert(lines, line)
    end
    h:close()

    table.insert(lines, 1, content)

    local path_new = path .. ".tmp"
    local h = io.open(path_new, 'w')
    for _, line in ipairs(lines) do
        h:write(line .. "\n")
    end
    h:close()

    os.remove(path)
    os.rename(path_new, path)
end

----- TODO check if already in file, if positive then skip
-----       ignore date (first tab)
-----       check if exist
----- TODO url ( first column ) as index
function file_append2(path, content)
    local h = assert(io.open(path, 'r'))
    local tbl = {}
    for line in h:tbl() do
        local i = 1 -- first column
        for value in (string.gmatch(line, "[^%s]+")) do  -- tab separated values
    --  for value in (string.gmatch(line, '%d[%d.]*')) do -- comma separated values
            tbl[i]=tbl[i]or{} -- if not column then create new one
            tbl[i][#tbl[i]+1]=tonumber(value) -- adding row value
            i=i+1 -- column iterator
        end
    end
    h:close()

    -- TODO dont pass date
    table.find(tlb, content)
    -- table.insert(table, 1, content)

    -- local path_new = path .. ".tmp"
    -- local h = io.open(path_new, 'w')
    -- for _, line in ipairs(tbl) do
    --     h:write(line .. "\n")
    -- end
    -- h:close()

    -- os.remove(path)
    -- os.rename(path_new, path)
end

----- history

path = ""

local o = {
    exclude = "",
    storage_path = "~~/history.log",
}

opt = require "mp.options"
opt.read_options(o)

o.storage_path = mp.command_native({"expand-path", o.storage_path})

function discard()
    for _, v in pairs(split(o.exclude, ";")) do
        local p = replace(path, "/", "\\")
        v = replace(trim(v), "/", "\\")

        if contains(p, v) then
            return true
        end
    end
end

function history()
    path = mp.get_property("path")
    local uploader = mp.get_property("metadata/by-key/uploader")
    local title = mp.get_property("media-title")
    local uploader = ''

    if contains(path, "://") then
        uploader = mp.get_property("metadata/by-key/uploader")
    else
        local path_words = split(path, '/')
        uploader = path_words[#path_words-3] .. '/' ..  path_words[#path_words-2]
    end

    if not is_empty(path) and not discard() then
        local line = string.format("%-14s\t«%-25s»: %-70s\t%s", os.date("%x %X"), uploader, title, path)
        file_append(o.storage_path, line)
    end
end

mp.register_event("file-loaded", history)
mp.register_event("shutdown", history)
