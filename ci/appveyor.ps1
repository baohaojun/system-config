#Set environment variables for Visual Studio Command Prompt
#http://stackoverflow.com/questions/2124753/how-i-can-use-powershell-with-the-visual-studio-command-prompt
function batCall([string] $path, [string] $arg)
{
    Write-Host "Calling `"$path`" `"$arg`""
    cmd /c  "$path" "$arg" `& set |
    foreach {
      if ($_ -match "=") {
        $v = $_.split("=")
        #Write-Host "ENV:\$($v[0])=$($v[1])"
        set-item -force -path "ENV:\$($v[0])"  -value "$($v[1])"
      }
    }
}
function fixCmakeDestDir([string] $prefix, [string] $destDir)
{
    $prefix=$prefix -replace "/", "\\"
    $destDir=$destDir -replace "/", "\\"
    if( $prefix.substring(1,1) -eq ":")
    {
        $prefix=$prefix.substring(3)
    }
    Write-Host "move $destDir\$prefix to $destDir"
    mv "$destDir\$prefix\*" "$destDir"
    $rootLeftOver = $prefix.substring(0, $prefix.indexOf("\\"))
    Write-Host "rm $destDir\$rootLeftOver"
    rm -Recurse "$destDir\$rootLeftOver"

}
$CMAKE_INSTALL_ROOT="$env:APPVEYOR_BUILD_FOLDER/work/install" -replace "\\", "/"

if ( $env:COMPILER -eq "MINGW" )
{
    batCall "C:\Qt\5.5\mingw492_32\bin\qtenv2.bat"
    #remove sh.exe from path
    $env:PATH=$env:PATH -replace "C:\\Program Files \(x86\)\\Git\\bin", ""
    $CMAKE_GENERATOR="MinGW Makefiles"
    $MAKE = "mingw32-make"
}
elseif ( $env:COMPILER -eq "MSVC" )
{
    batCall "C:\Qt\5.5\msvc2013_64\bin\qtenv2.bat"
    batCall "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" amd64
    $CMAKE_GENERATOR="NMake Makefiles"
    $MAKE="nmake"
}



mkdir $env:APPVEYOR_BUILD_FOLDER\work\image
mkdir $env:APPVEYOR_BUILD_FOLDER\work\build
mkdir $env:APPVEYOR_BUILD_FOLDER\work\build\extra-cmake-modules
mkdir $env:APPVEYOR_BUILD_FOLDER\work\build\snorenotify


if ( !(Test-Path "$env:APPVEYOR_BUILD_FOLDER\work\install" ) )
{
    mkdir $env:APPVEYOR_BUILD_FOLDER\work\install
    mkdir $env:APPVEYOR_BUILD_FOLDER\work\git
    
    cd $env:APPVEYOR_BUILD_FOLDER\work\git
    git clone git://anongit.kde.org/extra-cmake-modules.git
    
    cd $env:APPVEYOR_BUILD_FOLDER\work\build\extra-cmake-modules
    cmake -G $CMAKE_GENERATOR $env:APPVEYOR_BUILD_FOLDER\work\git\extra-cmake-modules -DCMAKE_INSTALL_PREFIX="$CMAKE_INSTALL_ROOT"
    & $MAKE install
}


cd $env:APPVEYOR_BUILD_FOLDER\work\build\snorenotify
cmake -G $CMAKE_GENERATOR $env:APPVEYOR_BUILD_FOLDER -DCMAKE_BUILD_TYPE=Release -DWITH_SNORE_DAEMON=ON -DWITH_FRONTENDS=ON -DCMAKE_INSTALL_PREFIX="$CMAKE_INSTALL_ROOT"
& $MAKE install DESTDIR="$env:APPVEYOR_BUILD_FOLDER\work\image"
fixCmakeDestDir $CMAKE_INSTALL_ROOT "$env:APPVEYOR_BUILD_FOLDER\work\image"