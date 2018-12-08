#!/usr/bin/lua

Wrench = require"wrench"

dofile_res, emojis_map = pcall(dofile, "../emojis.lua")

emoji_image_map = {}

emojis_el = io.open("emojis.el.x", "w")


if dofile_res then
   emojis_el:write("(setq emojis-string-list '(" .. "\n")
   for i = 1, #emojis_map do
      emoji_def = emojis_map[i]
      emoji_char = emoji_def[1]
      emoji_what = emoji_def[2]
      emoji_png = emoji_def[3]
      if true then
         emoji_image_map[emoji_char] = emoji_png
         emojis_el:write (
            (
               [[ ("%s" "~/src/github/Wrench/release/%s" "%s") ]]
            ):format(emoji_char:gsub('"', '\\"'):gsub("%[", "［"):gsub("%]", "］"),
                     emoji_png:gsub('"', '\\"'),
                     emoji_what:gsub('"', '\\"')) .. "\n")
      end
   end
end

Wrench.dataDir = os.getenv("HOME") .. "/src/github/private-config/Wrench-cache/"
if Wrench.file_exists(Wrench.dataDirFile("apps.info")) then
   apps_file = io.open(Wrench.dataDirFile("apps.info"))
   local apps_txt = apps_file:read("*a")
   local apps = Wrench.split("\n", apps_txt)
   local app_table = {}
   apps[#apps + 1] = "smartisan=smartisan=锤子科技 smartisan"
   for i = 1, #apps do
      fields = Wrench.split("=", apps[i])
      local activity, class_, name  = fields[1], fields[2], fields[3]
      local pinyin = Wrench.qx(("pinyin? -1 '%s'"):format(name))
      io.stderr:write("working with " .. name .. "\n")
      emojis_el:write(
         (
            [[ ("［%s］" "~/.local-config/Wrench-cache/%s.png" "apps %s %s")]] .. "\n"
         ):format(
            name,
            activity,
            name,
            pinyin
                 )
      )
   end
end

emojis_el:write(")) ; end of emojis.el" .. "\n")
emojis_el:close()

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
