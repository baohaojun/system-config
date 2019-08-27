M.log3 = log

local current_files = ls_files{"lib", "*.lua"}

for _, f in pairs(current_files) do
   wrench_run(f)
end

current_files = ls_files{os.getenv("HOME") .. "/.local-config/Wrench-config/lib", "*.lua"}

for _, f in pairs(current_files) do
   wrench_run(f)
end
