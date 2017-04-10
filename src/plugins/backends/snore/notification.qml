import QtQuick 2.7
import QtQuick.Window 2.2

Window {
  id: window
  property int snoreBaseSize: body.font.pixelSize
  property int animationStart


  width:  snoreBaseSize * 30
  height: snoreBaseSize * 6
  color: notifyWidget.color

  onVisibleChanged: {
    if(visible){
      animation.from = animationStart
      animation.restart()
      show()
    }
  }

  NumberAnimation on x{
    id: animation
    duration: 200
  }

  Rectangle{
    id: root

    // don't anchor as we need he xChanged signal
    // a drag event directly on the window is not supported
    height: window.height
    width: window.width
    color: window.color

    Drag.active: mouseAreaAccept.drag.active

    function updatePosition()
    {
      var corner = Qt.BottomRightCorner

      if (corner === Qt.TopLeftCorner || corner === Qt.BottomLeftCorner) {
        animationStart = mouseAreaAccept.drag.minimumX = animation.from = -window.width
        mouseAreaAccept.drag.maximumX = animation.to = 0
      } else {
        animationStart = animation.from = Screen.desktopAvailableWidth
        animation.to = Screen.desktopAvailableWidth - window.width
        mouseAreaAccept.drag.minimumX =  0
        mouseAreaAccept.drag.maximumX = window.width
      }
      var space = (notifyWidget.id + 1) * window.height * 0.025

      if (corner === Qt.TopRightCorner || corner === Qt.TopLeftCorner) {
        window.y = space + (space + window.height) * notifyWidget.id
      } else {
        window.y = Screen.desktopAvailableHeight - (space + (space + height) * (notifyWidget.id + 1))
      }
    }

    Screen.onDesktopAvailableHeightChanged: root.updatePosition()
    Screen.onDesktopAvailableWidthChanged: root.updatePosition()

    Connections{
      target: notifyWidget
      onPositionChanged: root.updatePosition()
    }

    onXChanged: {
      // There is a Qt bug which will not stop the mouse tracking if the
      // item is hid during a drag event.
      if(Drag.active){
        window.x += x
        if(Math.abs(window.x - Math.max(animation.from, animation.to) +  mouseAreaAccept.mouseX) <= width * 0.05){
          Drag.cancel()
          notifyWidget.dismissed()
        }
      }
      x = 0
    }

    MouseArea {
      id: mouseAreaAccept
      anchors.fill: parent
      z: 90
      onClicked: {
        notifyWidget.invoked()
      }
      drag.target: root
      drag.axis: Drag.XAxis
      drag.smoothed: true
      onPressed: {
        animation.stop()
      }
      onReleased: {
        animation.from = window.x
        animation.start()
      }
    }


    Text {
      id: title
      color: notifyWidget.textColor
      text: notifyWidget.title
      font.pointSize: body.font.pointSize * 1.5
      font.bold: true
      anchors.top: parent.top
      anchors.margins: snoreBaseSize
      anchors.topMargin: snoreBaseSize / 2
      anchors.left: image.right
      anchors.right: closeButton.left
      textFormat: Text.StyledText
      font.family: notifyWidget.fontFamily
      elide: Text.ElideRight
    }

    Text {
      id: body
      color: notifyWidget.textColor
      text: notifyWidget.body
      font.pointSize: 10
      anchors.right: parent.right
      anchors.top: title.bottom
      anchors.bottom: parent.bottom
      anchors.left: image.right
      anchors.margins: snoreBaseSize
      anchors.topMargin: snoreBaseSize / 2
      anchors.bottomMargin: snoreBaseSize / 2
      wrapMode: Text.WrapAtWordBoundaryOrAnywhere
      onLinkActivated: Qt.openUrlExternally(link)
      textFormat: Text.StyledText
      font.family: notifyWidget.fontFamily
      elide: Text.ElideRight
    }

    Image {
      id: image
      fillMode: Image.PreserveAspectFit
      height: Math.max(root.height * 0.40, 48)
      width: Math.max(root.height * 0.40, 48)
      smooth: true
      anchors.left: parent.left
      anchors.margins: snoreBaseSize
      anchors.top: parent.top
      z: 4
      onWidthChanged: notifyWidget.imageSize = width
      source: notifyWidget.image
    }
  }
}
