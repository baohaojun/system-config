import QtQuick 1.1




Rectangle {
    objectName: "qmlNotification"
    id: root
    width: 365
    height: 100
    clip: true

    signal dismissed()

    signal linkClicked(string link)

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
        x: 106
        y: 3
        width: 217
        height: 14
        color: "#ffffff"

        text: qsTr("Title")
        wrapMode: Text.WordWrap
    }

    Text {
        id: body
        x: 106
        y: 23
        width: 217
        height: 69
        color: "#ffffff"
        text: qsTr("Body")
        wrapMode: Text.WordWrap
        onLinkActivated: root.linkClicked(link)

    }


    Image {
        id: image
        width: 100
        height: 100
        z: 4
        fillMode: Image.PreserveAspectFit
        source: "qrc:/root/images/freedesktop-dbus.png"
    }

    Image {
        id: appIcon
        x: 329
        y: 62
        width: 30
        height: 30
        fillMode: Image.PreserveAspectFit
        source: "qrc:/root/snore.png"
    }

    Image {
        id: closeButton
        x: 345
        y: 0
        width: 20
        height: 20
        fillMode: Image.PreserveAspectFit
        z: 3
        source: "qrc:/resources/close.png"
        visible: false

        MouseArea {
            id: mouseArea1
            anchors.fill: parent
            onClicked: root.dismissed()
        }
    }






}
