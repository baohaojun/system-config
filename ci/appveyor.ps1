#Set environment variables for Visual Studio Command Prompt
#http://stackoverflow.com/questions/2124753/how-i-can-use-powershell-with-the-visual-studio-command-prompt
function batCall([string] $path, [string] $arg)
{
Write-Host "Calling $path $arg"
cmd /c "'$path' '$arg' & set" |
foreach {
  if ($_ -match "=") {
    #Write-Host "ENV:\$($v[0])=$($v[1])"
    $v = $_.split("="); set-item -force -path "ENV:\$($v[0])"  -value "$($v[1])"
  }
}
}

$CMAKE_INSTALL_ROOT=$env:APPVEYOR_BUILD_FOLDER -replace "\\", "/"
$CMAKE_INSTALL_ROOT="$CMAKE_INSTALL_ROOT/work/install"


mkdir $env:APPVEYOR_BUILD_FOLDER\work\install
mkdir $env:APPVEYOR_BUILD_FOLDER\work\build
mkdir $env:APPVEYOR_BUILD_FOLDER\work\build\extra-cmake-modules
mkdir $env:APPVEYOR_BUILD_FOLDER\work\build\snorenotify


If ( !(Test-Path "$env:APPVEYOR_BUILD_FOLDER\work\git")){
    mkdir $env:APPVEYOR_BUILD_FOLDER\work\git
    cd $env:APPVEYOR_BUILD_FOLDER\work\git
    git clone git://anongit.kde.org/extra-cmake-modules.git
}


if( $env:COMPILER -eq "MINGW" )
{
batCall "C:\Qt\5.5\mingw492_32\bin\qtenv2.bat"
#remove sh.exe from path
$env:PATH=$env:PATH -replace "C:\\Program Files \(x86\)\\Git\\bin", ""
$CMAKE_GENERATOR="MinGW Makefiles"
}
elseif( $env:COMPILER -eq "MSVC" )
{
batCall "C:\Qt\5.5\msvc2013_64\bin\qtenv2.bat"
batCall "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" amd64
$CMAKE_GENERATOR="NMake Makefiles"
}

cd $env:APPVEYOR_BUILD_FOLDER\work\build\extra-cmake-modules
cmake -G $CMAKE_GENERATOR $env:APPVEYOR_BUILD_FOLDER\work\git\extra-cmake-modules -DCMAKE_INSTALL_PREFIX="$CMAKE_INSTALL_ROOT"
nmake install

cd $env:APPVEYOR_BUILD_FOLDER\work\build\snorenotify
cmake -G $CMAKE_GENERATOR $env:APPVEYOR_BUILD_FOLDER -DWITH_SNORE_DAEMON=ON -DWITH_FRONTENDS=ON -DCMAKE_INSTALL_PREFIX="$CMAKE_INSTALL_ROOT"
nmake