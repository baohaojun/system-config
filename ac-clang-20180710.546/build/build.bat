@echo off


del /Q CMakeCache.txt
del /Q cmake_install.cmake
rmdir /Q /S CMakeFiles
rmdir /Q /S clang-server-x86_64.dir


set BUILD_OPTIONS=%~f0.ini
echo F | xcopy /D /Y %BUILD_OPTIONS%.template %BUILD_OPTIONS%


@rem parse for build option
setlocal enabledelayedexpansion


if exist "%BUILD_OPTIONS%" (
   set LINE_PREFIX=
   set INI_SECTION=

   for /f "eol=; delims== tokens=1,2" %%a in ( %BUILD_OPTIONS% ) do (
      set LINE_VALUE=%%a
      set LINE_PREFIX=!LINE_VALUE:~0,1!!LINE_VALUE:~-1,1!
      set INI_SECTION=!LINE_VALUE:~1,-1!

      if not "!LINE_PREFIX!"=="[]" (
         set !LINE_VALUE!=%%b
      )
   )
)


@rem ini vars are overwrite by arguments
if not "%1" == "" (
   set HOST_VS_VERSION=%1
)

if not "%2" == "" (
   set TARGET_LLVM_VERSION=%2
)

if not "%3" == "" (
   set TARGET_ARCH=%3
)

if not "%4" == "" (
   set TARGET_CONFIG=%4
)

if not "%5" == "" (
   set INSTALL_PREFIX=%5
)


set CMAKE_GENERATOR=Visual Studio
if "%HOST_VS_VERSION%" == "2017" (
   set CMAKE_GENERATOR=%CMAKE_GENERATOR% 15 2017
) else if "%HOST_VS_VERSION%" == "2015" (
   set CMAKE_GENERATOR=%CMAKE_GENERATOR% 14 2015
) else if "%HOST_VS_VERSION%" == "2013" (
   set CMAKE_GENERATOR=%CMAKE_GENERATOR% 12 2013
) else if "%HOST_VS_VERSION%" == "2012" (
   set CMAKE_GENERATOR=%CMAKE_GENERATOR% 11 2012
) else (
  echo unsupported Visual Studio Version!
  exit /B 1
)


set CMAKE_ADDITIONAL_OPTIONS=


if "%TARGET_ARCH%" == "64" (
   set CMAKE_GENERATOR=%CMAKE_GENERATOR% Win64
   set CMAKE_ADDITIONAL_OPTIONS=-Thost=x64
)


set PATH=%CMAKE_PATH%;%PATH%

set LLVM_LIBRARY_PATH="../clang-server/library/x86_%TARGET_ARCH%/%TARGET_CONFIG%/"
if not "%LLVM_BUILD_SHELLS_PATH%" == "" (
   set LLVM_LIBRARY_PATH="%LLVM_BUILD_SHELLS_PATH%/ps1/llvm-%TARGET_LLVM_VERSION%/build/msvc%HOST_VS_VERSION%-%TARGET_ARCH%/%TARGET_CONFIG%/"
)


@echo on

@rem goto :end

cmake -G "%CMAKE_GENERATOR%" ../clang-server -DLIBRARY_PATHS=%LLVM_LIBRARY_PATH% -DCMAKE_INSTALL_PREFIX=%INSTALL_PREFIX% %CMAKE_ADDITIONAL_OPTIONS%

@rem @pause

@rem cmake --build . [--config <config>] [--target <visual studio project name>] [-- -i]
cmake --build . --config %TARGET_CONFIG% --target %CMAKE_TARGET%

@rem :end
@rem set
@pause

