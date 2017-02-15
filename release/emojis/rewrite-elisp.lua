#!/usr/bin/lua

dofile_res, emojis_map = pcall(dofile, "../emojis.lua")
if dofile_res then
   print "(setq emojis-string-list '("
   for i = 1, #emojis_map do
      emoji_def = emojis_map[i]
      emoji_char = emoji_def[1]
      emoji_what = emoji_def[2]
      emoji_png = emoji_def[3]
      if emoji_def[2]:match("unicode$") then
         print (
            (
               [[ ("%s" "~/src/github/Wrench/release/%s" "%s") ]]
            ):format(emoji_char:gsub('"', '\\"'),
                     emoji_png:gsub('"', '\\"'),
                     emoji_what:gsub('"', '\\"')))
      end
   end
end
print"))"
