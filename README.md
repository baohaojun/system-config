Snorenotify
===========

Snorenotify is a multi platform Qt notification framework. 
Using a plugin system it is possible to create notifications with many different notification systems on Windows, Unix and Mac.

For a detailed description for some of our backends see our [Wiki](https://github.com/Snorenotify/Snorenotify/wiki).

## Support
If you need support on how to use Snorenotify you can reach out to the developers using IRC joining [#Snorenotify](irc://irc.freenode.net/snorenotify) on freenode or using the [Web chat](http://webchat.freenode.net/?channels=snorenotify) .

## Supported Backends

- [Windows Toast notifications](https://github.com/Snorenotify/Snorenotify/wiki/Windows-Toast-Notification) 

- OSX Notification Center

- Free Desktop Notifications

- [Growl for Windows](http://www.growlforwindows.com/)

- [Growl](http://growl.info/)

- [Snarl](http://snarl.fullphat.net/)


- [Integrated notifications](https://github.com/Snorenotify/Snorenotify/wiki/Integrated-Notification-Backend)

- System Tray

## Supported secondary Backends

Secondary backends are those that allow you to send notifications to your phone, a website, play a sound.
In difference to backends they don't offer interactions with te notification  therefore any number of secondary backends might be enabled. 

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

## Build status
- [Windows build status on appveyor.com](https://ci.appveyor.com/project/TheOneRing/snorenotify/)
- [Linux and Mac OSX build status on travis.org](https://travis-ci.org/Snorenotify/Snorenotify)


## Third party libraries that we ship with our source ##
- [Snarl C++](http://sourceforge.net/p/snarlwin/code/HEAD/tree/trunk/hdr/C++/SnarlInterface_v42/) used for Snarl

