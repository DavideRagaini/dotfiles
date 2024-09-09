----- string

local function is_empty(input)
    if input == nil or input == "" then
        return true
    end
end

local function trim(input)
    if is_empty(input) then
        return ""
    end

    return input:match "^%s*(.-)%s*$"
end

local function contains(input, find)
    if not is_empty(input) and not is_empty(find) then
        return input:find(find, 1, true)
    end
end

local function replace(str, what, with)
    if is_empty(str) then return "" end
    if is_empty(what) then return str end
    if with == nil then with = "" end
    what = string.gsub(what, "[%(%)%.%+%-%*%?%[%]%^%$%%]", "%%%1")
    with = string.gsub(with, "[%%]", "%%%%")
    return string.gsub(str, what, with)
end

local function split(input, sep)
    local tbl = {}

    if not is_empty(input) then
        for str in string.gmatch(input, "([^" .. sep .. "]+)") do
            table.insert(tbl, str)
        end
    end

    return tbl
end

----- file

local function file_exists(path)
    local f = io.open(path, "r")

    if f ~= nil then
        io.close(f)
        return true
    end
end

-- function file_exists(file)
--   -- some error codes:
--   -- 13 : EACCES - Permission denied
--   -- 17 : EEXIST - File exists
--   -- 20	: ENOTDIR - Not a directory
--   -- 21	: EISDIR - Is a directory
--   --
--   local isok, errstr, errcode = os.rename(file, file)
--   if isok == nil then
--      if errcode == 13 then
--         -- Permission denied, but it exists
--         return true
--      end
--      return false
--   end
--   return true
-- end

-- local function dir_exists(path)
--   return file_exists(path .. "/")
-- end

local function file_append(path, content)
    local hr,err = assert(io.open(path, 'r'))
    local lines = {}
    if hr == nil then
        print("Couldn't open file: "..err)
    else
        for line in hr:lines() do
            table.insert(lines, line)
        end
        hr:close()
    end

    table.insert(lines, 1, content)

    local path_new = path .. ".tmp"
    local hw, errw = io.open(path_new, 'w')
    if hw == nil then
        print("Couldn't open file: "..errw)
    else
        for _, line in ipairs(lines) do
            hw:write(line .. "\n")
        end
        hw:close()
    end


    os.remove(path)
    os.rename(path_new, path)
end

----- history

local path = ""

local o = {
    exclude = "",
    storage_path = "~~/history.log",
}

local opt = require "mp.options"
opt.read_options(o)

o.storage_path = mp.command_native({"expand-path", o.storage_path})

local function discard()
    for _, v in pairs(split(o.exclude, ";")) do
        local p = replace(path, "/", "\\")
        v = replace(trim(v), "/", "\\")

        if contains(p, v) then
            return true
        end
    end
end

local function grep(key, f)
    local h = assert(io.open(f, 'r'))
    local i = 1
    for line in h:lines() do
        local j = 1
        for value in (string.gmatch(line, "[^\t]+")) do
          if j == 4 then
              if value == key then
                h:close()
                return false
              end
          end
          j = j+1
        end
        i = i+1
    end
    h:close()
    return true
end

local function history()
    path = mp.get_property("path")
    if grep(path, o.storage_path) then
        local title = mp.get_property("media-title")
        local uploader = ""

        if file_exists(path) then
            local path_words = split(path, '/')
            uploader = string.format(
                "%s/%s/%s",
                path_words[#path_words-4],
                path_words[#path_words-3],
                path_words[#path_words-2])
            -- path = string.gsub(path, os.getenv("HOME"), '~')
        else
            uploader = mp.get_property("metadata/by-key/uploader")
        end

        if not is_empty(path) and not discard() then
            local line = string.format(
                "%-14s\t%-30s\t%-90s\t%s",
                os.date("%Y-%m-%d %H:%M:%S"),
                string.gsub(uploader,"\n",""),
                title,
                path)
            file_append(o.storage_path, line)
        end
    end
end

mp.register_event("file-loaded", history)
mp.register_event("shutdown", history)
