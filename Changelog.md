# Changes since 0.5.3 #
- Moved to KDE
- Added support for settings.
- New secondary backend [Toasty](http://supertoasty.com/).
- New secondary backend [Notify my Android](https://www.notifymyandroid.com/).
- New secondary backend Sound playback.
- New secondary backend [Pushover](https://pushover.net/) .
- New frontend  [Pushover](https://pushover.net/).
- OSX Notification Center now supports callbacks, thanks to [jendas1](https://github.com/jendas1).
- Install a .pri file, it is now quite easy to use Snorenotify in a qmake project.
- Improved implementation of build in Snore backend.
- Dropped Qt4
- Use c++11
- Lots of important Api changes

# Changes since 0.5.2 #
- Updated SnoreToast

# Changes since 0.5.1 #
- Updated the shiped version of SnoreToast
	- SnoreToast reported succes even if notifications failed because they where disabled by the system. This lead to a crash in snorenotify.

# Changes since 0.5.0 #
- Ship  a staticaly linked version of Snoretoast
- Fix window comming to front on mac osx on snore notifications
- Fixed creation of tmp dir on qt4

# Changes since 0.5.0rc3 #
- Readded the missing close button for the snore backend
- Cleaned up snore backend use a QPropertyAnimation instead of a custom timer code.
- Cleaned up qml in snore backend.
- Some buildsystem fixes.

# Changes since 0.5.0rc2 #
- Fixed a crash related to a QTimer deleted in the wrong thread.

# Changes since 0.5.0rc1 #
- Cleanedup build system using new Cmake magic
- Dropped KDE stuff
- Dropeed dependencie to boost and cryptopp by switching to a different Growl implementation. [A fork of gntp-send](https://github.com/Snorenotify/SnoreGrowl).


# Changes since 0.5.0beta4 #
- Merged mac osx backend
- Fixed a possible crash, if register was called directly before notify
- Don't register snorenotify with Growl

# Changes since 0.5.0beta3 #
- Only use QTemporaryDir  in qt5

# Changes since 0.5.0beta2 #
- Use  QTemporaryDir for the image cache
- Reduce calls to QFile::exists

# Changes since 0.5.0beta1 #
- Removed plugin caching, it was overkill and caused problems if the plugins where updated.

# Changes since 0.5-alpha1 #
- Better scaling for the snore backend.
- Fixed a crash.

# Changes since 0.4 #
- Support for separated installs for qt4 and qt5.
- Improved logging system.
- Extended doxygen doc.
- Refactoring.
- Added a default qml notification backend.
- Improved plugin loading.
- The Windows 8 backend now supports closing of notifications.
....
