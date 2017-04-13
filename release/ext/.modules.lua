local M = {}
M.description_to_filename = {}
M.description_to_loaded_func = {}
M.descriptions = {}

M.descriptions[1] = "What do you want?"
M.description_to_filename[ [==[在微博上关注小扳手作者包昊军（不一定成功）]==] ] = 'follow-me.lua'
M.descriptions[2] = [==[在微博上关注小扳手作者包昊军（不一定成功）]==]
M.description_to_filename[ [==[通过微博、微信朋友圈传播小扳手（不一定成功）]==] ] = 'spread-it.lua'
M.descriptions[3] = [==[通过微博、微信朋友圈传播小扳手（不一定成功）]==]
return M
