#configuration
CONFIG +=  shared qpa release qt_no_framework
host_build {
    QT_ARCH = x86_64
    QT_TARGET_ARCH = x86_64
} else {
    QT_ARCH = x86_64
}
QT_CONFIG +=  minimal-config small-config medium-config large-config full-config gtk3 fontconfig libudev evdev xkbcommon-evdev xlib xrender xcb-plugin xcb-render xcb-glx xcb-xlib xcb-sm accessibility-atspi-bridge gbm linuxfb kms c++11 accessibility egl egl_x11 eglfs eglfs_gbm openvg opengl shared qpa reduce_exports reduce_relocations clock-gettime clock-monotonic posix_fallocate mremap getaddrinfo ipv6ifname getifaddrs inotify eventfd threadsafe-cloexec poll_poll system-jpeg system-png png doubleconversion system-freetype harfbuzz system-zlib cups iconv glib dbus dbus-linked openssl xcb xinput2 rpath alsa pulseaudio gstreamer-0.10 icu concurrent audio-backend release

#versioning
QT_VERSION = 5.7.1
QT_MAJOR_VERSION = 5
QT_MINOR_VERSION = 7
QT_PATCH_VERSION = 1

#namespaces
QT_LIBINFIX = 
QT_NAMESPACE = 

QT_EDITION = OpenSource

QT_COMPILER_STDCXX = 199711
QT_GCC_MAJOR_VERSION = 4
QT_GCC_MINOR_VERSION = 8
QT_GCC_PATCH_VERSION = 0
