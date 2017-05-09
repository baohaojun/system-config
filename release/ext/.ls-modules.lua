local M = {}
M.description_to_filename = {}
M.description_to_loaded_func = {}
M.descriptions = {}

M.descriptions[1] = "你想执行什么扩展功能?"

local isUser = {}

current_files = ls_files{"ext", "*.lua"}
data_ext_files = ls_files{configDirFile("ext"), "*.lua"}
for _, f in pairs(data_ext_files) do
   current_files[#current_files + 1] = f
   isUser[f] = true
end

for _, f in pairs(current_files) do

   local fio = io.open(f)
   if fio then
      local data = fio:read("*a")
      local lines = split("\n", data)
      local description = ""
      for _, l in pairs(lines) do
         l, n = l:gsub("^%s*--%s+(.*)", "%1")
         if n == 1 then
            description = l
            break
         end
      end
      local base = f:gsub(".*" .. package.config:sub(1, 1), "")
      local key = description .. " : " .. base
      if isUser[f] then
         key = key .. " (user)"
      end
      M.description_to_filename[key] = f
      M.descriptions[#M.descriptions + 1] = key
   end
end

return M
