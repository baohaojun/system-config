import QtQuick 1.1



Rectangle {
    objectName: "qmlNotification"
    id: root
    width: 365
    height: 100
    clip: false

    signal dismissed()

    signal invoked()





    function update(nTitle, bBody, nImage, nAppIcon, color, textColor)
    {
        title.text = nTitle
        title.color = textColor
        body.text = bBody
        body.color = textColor
        appIcon.source = nAppIcon
        image.source = nImage
        root.color = color
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

        text: qsTr("Title")
        font.pointSize: 10
        font.bold: true
        anchors.right: closeButton.left
        anchors.rightMargin: 22
        anchors.top: parent.top
        anchors.topMargin: 3
        anchors.left: image.right
        anchors.leftMargin: 6
        wrapMode: Text.WordWrap
    }

    Text {
        id: body
        color: "#000000"
        text: qsTr("Body")
        font.pointSize: 10
        anchors.right: appIcon.left
        anchors.rightMargin: 6
        anchors.top: title.bottom
        anchors.topMargin: 6
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 8
        anchors.left: image.right
        anchors.leftMargin: 6
        wrapMode: Text.WordWrap
        onLinkActivated: Qt.openUrlExternally(link)

    }


    Image {
        id: image
        width: 100
        anchors.left: parent.left
        anchors.leftMargin: 0
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 0
        anchors.top: parent.top
        anchors.topMargin: 0
        z: 4
        fillMode: Image.PreserveAspectFit
        source: "../../../../data/freedesktop-dbus.png"
    }

    Image {
        id: appIcon
        x: 329
        y: 62
        width: 30
        height: 30
        anchors.right: parent.right
        anchors.rightMargin: 6
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 8
        fillMode: Image.PreserveAspectFit
        source: "../../../../data/snore.png"
    }

    Image {
        id: closeButton
        x: 345
        width: 20
        height: 20
        anchors.top: parent.top
        anchors.topMargin: 0
        anchors.right: parent.right
        anchors.rightMargin: 0
        fillMode: Image.PreserveAspectFit
        z: 3
        source: "resources/close.png"
        visible: false

        MouseArea {
            id: mouseArea1
            anchors.bottom: parent.bottom
            anchors.left: parent.left
            onClicked: root.dismissed()
        }
    }






}
