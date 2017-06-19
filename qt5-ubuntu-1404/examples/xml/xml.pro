TEMPLATE      = subdirs
SUBDIRS       = xmlstreamlint

qtHaveModule(widgets) {
    SUBDIRS +=  dombookmarks \
                saxbookmarks \
                streambookmarks

    qtHaveModule(network): SUBDIRS += \
                rsslisting
}
