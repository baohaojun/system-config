$ErrorActionPreference="Stop"

Import-Module $env:APPVEYOR_BUILD_FOLDER\ci\appveyorHelp.psm1 -Force -Verbose

Init @("ninja", "extra-cmake-modules", "png2ico")

mkdir -Force $env:APPVEYOR_BUILD_FOLDER\work\build\snorenotify
cd $env:APPVEYOR_BUILD_FOLDER\work\build\snorenotify
LogExec cmake -G"Ninja" $env:APPVEYOR_BUILD_FOLDER -DCMAKE_BUILD_TYPE=Release -DWITH_SNORE_DAEMON=ON -DWITH_FRONTENDS=ON -DCMAKE_INSTALL_PREFIX="$CMAKE_INSTALL_ROOT"
CmakeImageInstall "$env:APPVEYOR_BUILD_FOLDER\work\image"

SetupSnoreSend  "$env:APPVEYOR_BUILD_FOLDER\work\image\bin" @{
    "global" = @{ "Pushover-SECONDARY_BACKEND/UserKey.v1" = $env:SNORE_PUSHOVER_KEY;
                  "Toasty-SECONDARY_BACKEND/DeviceID.v1" = $env:SNORE_TOASTY_ID };
    "snore-send" = @{"Toasty-SECONDARY_BACKEND/Enabled.v1" = "true";
                     "Pushover-SECONDARY_BACKEND/Enabled.v1" = "true"}}


SendSnoreNotification "Build complete!" "Build of SnoreNotify succeeded"