Snorenotify
===========

Snorenotify is a multi platform Qt notification framework. 
Using a plugin system it is possible to create notifications with many different notification systems on Windows, Unix and Mac.

For a detailed description for some of our backends see our [Wiki](https://github.com/Snorenotify/Snorenotify/wiki).

## Supported Backends ##
- [Windows Toast notifications](https://github.com/Snorenotify/Snorenotify/wiki/Windows-Toast-Notification) 

- OSX Notification Center

- Free Desktop Notifications

- [Growl for Windows](http://www.growlforwindows.com/)

- [Growl](http://growl.info/)

- [Snarl](http://snarl.fullphat.net/)


- [Integrated notifications](https://github.com/Snorenotify/Snorenotify/wiki/Integrated-Notification-Backend)

- System Tray

## Supported secondary Backends ##

- [Windows Phone - Toasty](http://supertoasty.com/)
- [Android - Notify my Android](https://www.notifymyandroid.com/) 
- [Android and IOS - Pushover] (https://pushover.net/)
- Playback of sound filles



## Projects using Snorenotify ##
- [Quassel IRC](http://www.quassel-irc.org/)
- [Tomahawk](http://www.tomahawk-player.org/)

## How to integrate Snorenotify in your project ##
### CMake ###

    project( MyApp )
    cmake_minimum_required( VERSION 2.8.12 )
    find_package(LibsnoreQt5 0.5.91 REQUIRED)
  
    add_executable( my_app main.cpp)
    target_link_libraries( my_app Snore::Libsnore)
  
### Qmake ###

    QT += LibsnoreQt5
  

### API Doc ###
[A documentation of the API can be found here](http://patrick.von-reth.de/other/snore/latest/doc/html/index.html)


## Dependencies ##
Required dependencies:

- [CMake](http://www.cmake.org/)
- [Extra CMake Modules](https://projects.kde.org/projects/kdesupport/extra-cmake-modules)
- [Qt5](http://qt-project.org/)


## Third party libraries that we ship with our source ##
- [Snarl C++](http://sourceforge.net/p/snarlwin/code/HEAD/tree/trunk/hdr/C++/SnarlInterface_v42/) used for Snarl

