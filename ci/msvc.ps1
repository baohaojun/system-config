Import-Module $env:APPVEYOR_BUILD_FOLDER\ci\ci.psm1

batCall "C:\Qt\5.5\msvc2013_64\bin\qtenv2.bat"
batCall "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" amd64

$CMAKE_INSTALL_ROOT=$env:APPVEYOR_BUILD_FOLDER -replace "\\", "/"

$CMAKE_INSTALL_ROOT=$CMAKE_INSTALL_ROOT/work/install
mkdir $env:APPVEYOR_BUILD_FOLDER\work\install
mkdir $env:APPVEYOR_BUILD_FOLDER\work\git
mkdir $env:APPVEYOR_BUILD_FOLDER\work\build
mkdir $env:APPVEYOR_BUILD_FOLDER\work\build\extra-cmake-modules
mkdir $env:APPVEYOR_BUILD_FOLDER\work\build\snorenotify
If ( -Not Test-Path $env:APPVEYOR_BUILD_FOLDER\work\git){
    cd $env:APPVEYOR_BUILD_FOLDER\work\git
    git clone git://anongit.kde.org/extra-cmake-modules.git
}

cd $env:APPVEYOR_BUILD_FOLDER\work\build\extra-cmake-modules
cmake -G"NMake Makefiles" $env:APPVEYOR_BUILD_FOLDER\work\git\extra-cmake-modules -DCMAKE_INSTALL_PREFIX="$(CMAKE_INSTALL_ROOT)"
nmake install

cd $env:APPVEYOR_BUILD_FOLDER\work\build\snorenotify
cmake -G"NMake Makefiles" %APPVEYOR_BUILD_FOLDER% -DWITH_SNORE_DAEMON=ON -DWITH_FRONTENDS=ON -DCMAKE_INSTALL_PREFIX="$(CMAKE_INSTALL_ROOT)"
nmake