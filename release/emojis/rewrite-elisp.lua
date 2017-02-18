#!/usr/bin/lua

dofile_res, emojis_map = pcall(dofile, "../emojis.lua")

emoji_image_map = {}


if dofile_res then
   print "(setq emojis-string-list '("
   for i = 1, #emojis_map do
      emoji_def = emojis_map[i]
      emoji_char = emoji_def[1]
      emoji_what = emoji_def[2]
      emoji_png = emoji_def[3]
      if emoji_def[2]:match("unicode$") then
         emoji_image_map[emoji_char] = emoji_png
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

require 'json'

jfile = io.open("/home/bhj/.emacs.d/elpa/emojify-20161124.940/data/emoji.json")
jstr = jfile:read("*a")
j = json.decode(jstr)

for emoji, val in pairs(j) do

   if val['style'] == 'unicode' then
      if not emoji_image_map[emoji] then
         -- print("emoji is " .. emoji)
      else
         print("relative-link -f ../" .. emoji_image_map[emoji] .." emojione-v2.2.6-22/" .. val.image)
      end
   end
end
