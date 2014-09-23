import QtQuick 2.3
import QtQuick.Window 2.2

Rectangle {
    id: root

    width: body.font.pixelSize * 30
    height: body.font.pixelSize * 9

    signal dismissed()

    signal invoked()

    function update(nTitle, bBody, nImage, nAppIcon, color, textColor, isUpdate)
    {
        title.text = nTitle
        title.color = textColor
        body.text = bBody
        body.color = textColor
        appIcon.source = nAppIcon
        image.source = nImage
        root.color = color


        if (!isUpdate) {
            var id = window.id
            var space = (id + 1) * height * 0.025

            window.y = space + (space + height) * id
            animation.target = window
            animation.from = Screen.desktopAvailableWidth
            animation.to = Screen.desktopAvailableWidth - width
            animation.start()
        }
    }

    NumberAnimation{
        id: animation
        properties: "x"
        duration: 500
    }

    MouseArea {
        id: mouseArea2
        anchors.fill: parent
        z: -1
        onClicked: root.invoked()
        hoverEnabled: true
        onEntered: closeButton.visible = true
        onExited: closeButton.visible = false
    }

    Text {
        id: title
        height: 14
        color: "#000000"

        text: "Title"
        font.pointSize: 10
        font.bold: true
        anchors.right: closeButton.left
        anchors.rightMargin: 22
        anchors.top: parent.top
        anchors.topMargin: 5
        anchors.left: image.right
        anchors.leftMargin: 5
    }

    Text {
        id: body
        color: "#000000"
        text: "Body"
        font.pointSize: 10
        anchors.right: appIcon.left
        anchors.rightMargin: 5
        anchors.top: title.bottom
        anchors.topMargin: 5
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 5
        anchors.left: image.right
        anchors.leftMargin: 5
        wrapMode: Text.WordWrap
        maximumLineCount: height / font.pixelSize - 1
        onLinkActivated: Qt.openUrlExternally(link)

    }


    Image {
        id: image
        width: height
        smooth: true
        anchors.left: parent.left
        anchors.leftMargin: 5
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 5
        anchors.top: parent.top
        anchors.topMargin: 5
        z: 4
    }

    Image {
        id: appIcon
        height: root.height * 0.30
        width: root.height * 0.30
        smooth: true
        anchors.right: parent.right
        anchors.rightMargin: 5
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 5
    }

    Image {
        id: closeButton
        height: root.height * 0.20
        width: root.height * 0.20
        anchors.top: parent.top
        anchors.topMargin: 5
        anchors.right: parent.right
        anchors.rightMargin: 5
        z: 3
        source: "resources/close.png"
        visible: false
        smooth: true

        MouseArea {
            id: mouseArea1
            anchors.bottom: parent.bottom
            anchors.left: parent.left
            onClicked: root.dismissed()
        }
    }

}
