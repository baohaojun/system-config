import QtQuick 2.3
import QtQuick.Window 2.2

Rectangle {
    id: root

    property int snoreBaseSize: body.font.pixelSize

    width: snoreBaseSize * 30
    height: snoreBaseSize * 9
    color: window.color

    Connections{
        target: window
        onVisibleChanged: {
            if(visible){
                x = 0
                animation.from = window.animationFrom
                animation.start()
            }
        }
    }


    onXChanged: {
        window.x += x

        var visibleWidth = window.isOrientatedLeft?
                    window.x - window.animationTo:
                    window.x - window.animationFrom
        if(window.visible && Math.abs(visibleWidth + mouseAreaAccept.mouseX) <= width * 0.05){
            window.visible = false
            window.dismissed()
        }
    }

    NumberAnimation{
        id: animation
        properties: "x"
        duration: 500
        target: window
        from: window.animationFrom
        to: window.animationTo
    }

    MouseArea {
        id: mouseAreaAccept
        anchors.fill: parent
        z: 90
        onClicked: {
            console.log("invoked")
            window.invoked()
        }
        drag.target: parent
        drag.axis: Drag.XAxis
        drag.maximumX: window.dragMaxX
        drag.minimumX: window.dragMinX
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
        color: window.textColor
        text: window.title
        font.pointSize: body.font.pointSize * 1.5
        font.bold: true
        anchors.top: parent.top
        anchors.margins: snoreBaseSize
        anchors.topMargin: snoreBaseSize / 2
        anchors.left: image.right
        anchors.right: closeButton.left
        textFormat: Text.StyledText
        font.family: window.fontFamily
        elide: Text.ElideRight
    }

    Text {
        id: body
        color: window.textColor
        text: window.body
        font.pointSize: 10
        anchors.right: appIcon.left
        anchors.top: title.bottom
        anchors.bottom: parent.bottom
        anchors.left: image.right
        anchors.margins: snoreBaseSize
        anchors.topMargin: snoreBaseSize / 2
        wrapMode: Text.WrapAtWordBoundaryOrAnywhere
        onLinkActivated: Qt.openUrlExternally(link)
        textFormat: Text.StyledText
        font.family: window.fontFamily
        elide: Text.ElideRight
    }

    Image {
        id: image
        fillMode: Image.PreserveAspectFit
        width: height
        smooth: true
        anchors.left: parent.left
        anchors.margins: snoreBaseSize
        anchors.bottom: parent.bottom
        anchors.top: parent.top
        z: 4
        onWidthChanged: window.imageSize = width
        source: window.image
    }

    Image {
        id: appIcon
        fillMode: Image.PreserveAspectFit
        height: root.height * 0.30
        width: root.height * 0.30
        smooth: true
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        anchors.margins: snoreBaseSize
        onWidthChanged: window.appIconSize = width
        source: window.appIcon
    }

    Canvas{
        id: closeButton
        height: root.height * 0.25
        width: root.height * 0.25
        anchors.top: parent.top
        anchors.margins: snoreBaseSize
        anchors.right: parent.right
        z: 91

        onPaint: {
            var context = getContext("2d");
            context.lineWidth = snoreBaseSize * 0.25;

            context.beginPath();
            context.clearRect(0, 0, width, height);
            context.fill();

            if(mouseAreaCloseButton.containsMouse)
            {
                context.beginPath();
                context.strokeStyle = root.color
                context.fillStyle = body.color
                context.globalAlpha = 0.5
                context.moveTo(width/2 + width/2 , height/2);
                context.arc(width/2, height/2, width/2 , 0, 2*Math.PI, true)
                context.fill();
                context.stroke();
            }

            var margin = snoreBaseSize * 0.5
            context.beginPath();
            context.globalAlpha = 1
            context.strokeStyle = body.color
            context.moveTo(margin, margin);
            context.lineTo(width - margin, height - margin);
            context.moveTo(width - margin, 0 + margin);
            context.lineTo(margin, height - margin);
            context.stroke();
        }

        MouseArea {
            id: mouseAreaCloseButton
            anchors.fill: parent
            onClicked: window.dismissed()
            hoverEnabled: true
            onEntered: parent.requestPaint()
            onExited: parent.requestPaint()
        }
    }
}
