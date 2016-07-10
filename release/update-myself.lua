#!/usr/bin/lua

local my_error = error
error = function(msg, level)
   print(msg)
   print("Update failed! Press Enter to exit... ")
   io.stdin:read("*l")
   my_error(msg, level)
end

local wrench, system, debug
do
   wrench = require"wrench"
   system = wrench.system
   debug = wrench.debug
end

local md5file = io.open("wrench.md5")
if not md5file then
   error("Can't open local md5 file")
end

local md5s_here = {}
local urls_here = {}
for line in md5file:lines() do
   local file, url, md5 = line:match("^(%S+)%s+(%S+)%s+(%S+)%s*$")
   md5s_here[file] = md5
   urls_here[file] = url
end
md5file:close()

local mv, rm = "mv", "rm"
if urls_here.myself:match("-windows/") then
   mv, rm = "move", "del"
end


local _s = system{"./download", urls_here.myself, "wrench.md5.up"}

local md5s_remote = {}
local urls_remote = {}
local remote_files_md5 = ""

local remote_md5file = io.open("wrench.md5.up", "rb")
if not remote_md5file then
   error("Can't open wrench.md5.up, download failed?")
end

local md5lib = require"md5"

for line in remote_md5file:lines() do
   local file, url, md5 = line:match("^(%S+)%s+(%S+)%s+(%S+)%s*$")
   if file ~= "myself" then
      remote_files_md5 = remote_files_md5 .. line .. "\n";
   end
   md5s_remote[file] = md5
   urls_remote[file] = url
end

remote_md5file:close()

if md5lib.sumhexa(remote_files_md5) ~= md5s_remote["myself"] then
   error ("Invalid wrench.md5 md5, download invalid data? " .. md5lib.sumhexa(remote_files_md5) .. " : " .. md5s_remote["myself"])
end

for file, md5 in pairs(md5s_remote) do
   if file == "myself" then
      goto continue
   end

   if true then
      local stream = io.open(file, "rb")
      if stream then
         local data = stream:read("*a")
         stream:close()
         if md5lib.sumhexa(data) == md5s_remote[file] then
            debug("file %s already updated, continue", file)
            goto continue
         end
      end

      system{"./download", urls_remote[file], file .. ".up"}

      local stream_up = io.open(file .. ".up", "rb")
      if not stream_up then
         error("can't open " .. file .. ", download failed?")
      end

      local data = stream_up:read("*a")
      stream_up:close()

      local stream, _, errno = io.open(file, "wb")
      if not stream then
         error("Can't open '" .. file .. "': " .. _)
      end


      if md5lib.sumhexa(data) ~= md5s_remote[file] and file ~= "myself" then
         error("md5 mismatch for " .. file .. ", download failed?")
      end

      stream:write(data)
      stream:close()
      system{rm, file .. ".up"}
   end
   ::continue::
end

system{rm, "wrench.md5"}
system{mv, "wrench.md5.up", "wrench.md5"}
print("Update OK!, press Enter to exit... ")
io.stdin:read("*l")
