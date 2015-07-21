Import-Module .\appveyorHelp.psm1 -Force

INIT @("ninja", "extra-cmake-modules")

mkdir $env:APPVEYOR_BUILD_FOLDER\work\build\snorenotify
cd $env:APPVEYOR_BUILD_FOLDER\work\build\snorenotify
cmake -G"Ninja" $env:APPVEYOR_BUILD_FOLDER -DCMAKE_BUILD_TYPE=Release -DWITH_SNORE_DAEMON=ON -DWITH_FRONTENDS=ON -DCMAKE_INSTALL_PREFIX="$CMAKE_INSTALL_ROOT"
CMAKE-IMAGE-INSTALL "$env:APPVEYOR_BUILD_FOLDER\work\image"