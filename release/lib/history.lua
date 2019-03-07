M.load_history = function()
   local dofile_res, history_map = pcall(dofile, M.configDirFile("history.lua"))
   if dofile_res then
      M.history_map = history_map
   else
      M.history_map = {}
   end
   M.history_loaded = true
end

M.save_history = function(history_name_to_save)
   local history_file = io.open(M.configDirFile("history.lua"), "w")
   local val
   history_file:write("local map = {}\n")
   for history_name, history_array in spairs(M.history_map) do
      if history_name ~= "" then
         history_file:write(("\nmap[ %s ] = {\n"):format(M.quote_string(history_name)))
         local saved_vals_array = {}
         local length = 0
         local val_is_saved = {}
         local start_to_save = 1

         if history_name == history_name_to_save then
            start_to_save = 0
         end

         for s_i = start_to_save, #history_array do
            val = history_array[s_i]
            if not val_is_saved[val] and length < 100 then
               val_is_saved[val] = 1
               length = length + 1
               saved_vals_array[length] = val
            end
         end

         for s_i = 1, #saved_vals_array do
            val = saved_vals_array[s_i]
            history_file:write(("    %s,\n"):format(M.quote_string(val)))
         end

         M.history_map[history_name] = saved_vals_array
         history_file:write(("}\n"))
      end
   end
   history_file:write("return map\n")
   history_file:close()
end

M.select_args_with_history = function(history_name, prompt, init_args, ...)
   if not M.history_loaded then
      M.load_history()
   end

   local args= {prompt, init_args, ...}

   if type(history_name) == 'table' and prompt == nil then
      history_name, args = history_name[1], table.slice(history_name, 2)
   end

   if not M.history_map[history_name] then
      M.history_map[history_name] = {}
   end

   for i = 1, #M.history_map[history_name] do
      if not M.member(M.history_map[history_name][i], args) then
         args[#args + 1] = M.history_map[history_name][i]
      end
   end

   local ret = M.select_args(args)

   M.history_map[history_name][0] = ret
   M.save_history(history_name)
   return ret
end
