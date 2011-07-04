import PyQt4.QtGui
# Overload just the setSource member of QTextBrowser
# Should only be necessary with Qt < 4.3 (missing Qt4.3's setOpenLinks(False))
class QTextBrowser2(PyQt4.QtGui.QTextBrowser):
    def setSource(*args):
            pass
