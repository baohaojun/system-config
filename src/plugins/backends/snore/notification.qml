import QtQuick 2.3
import QtQuick.Window 2.2

Rectangle {
    id: root

    signal dismissed()
    signal invoked()

    property int snoreBaseSize: body.font.pixelSize

    width: snoreBaseSize * 30
    height: snoreBaseSize * 9


    NumberAnimation{
        id: animation
        objectName: "animation"
        properties: "x"
        duration: 500
        target: window
    }

    MouseArea {
        id: mouseArea2
        anchors.fill: parent
        z: 90
        onClicked: root.invoked()
    }

    Text {
        id: title
        objectName: "title"
        color: "#000000"
        text: "Title"
        font.pointSize: body.font.pointSize * 1.5
        font.bold: true
        anchors.top: parent.top
        anchors.margins: snoreBaseSize
        anchors.topMargin: snoreBaseSize / 2
        anchors.left: image.right
        anchors.right: closeButton.left
        textFormat: Text.StyledText
        font.family: snoreFont
        elide: Text.ElideRight
    }

    Text {
        id: body
        objectName: "body"
        color: "#000000"
        text: "Body"
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
        font.family: snoreFont
        elide: Text.ElideRight
    }

    Image {
        id: image
        objectName: "image"
        fillMode: Image.PreserveAspectFit
        width: height
        smooth: true
        anchors.left: parent.left
        anchors.margins: snoreBaseSize
        anchors.bottom: parent.bottom
        anchors.top: parent.top
        z: 4
    }

    Image {
        id: appIcon
        objectName: "appIcon"
        fillMode: Image.PreserveAspectFit
        height: root.height * 0.30
        width: root.height * 0.30
        smooth: true
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        anchors.margins: snoreBaseSize
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
            onClicked: root.dismissed()
            hoverEnabled: true
            onEntered: parent.requestPaint()
            onExited: parent.requestPaint()
        }
    }
}
