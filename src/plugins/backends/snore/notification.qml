import QtQuick 2.3
import QtQuick.Window 2.2

Rectangle {
    id: root

    signal dismissed()
    signal invoked()

    property int snoreMargin: body.font.pixelSize

    width: snoreMargin * 30
    height: snoreMargin * 9


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
            var corner = window.corner
            var id = window.id
            var space = (id + 1) * height * 0.025

            animation.target = window

            window.y = space + (space + height) * id
            if (corner == Qt.TopRightCorner || corner == Qt.BottomRightCorner) {
                animation.from = Screen.desktopAvailableWidth
                animation.to = Screen.desktopAvailableWidth - width
            } else {
                animation.from = -width
                animation.to = 0
            }
            if (corner == Qt.TopRightCorner || corner == Qt.TopLeftCorner) {
                window.y = space + (space + height) * id
            } else {
                window.y = Screen.desktopAvailableHeight - (space + (space + height) * (id + 1))
            }

            animation.start()
            window.visible = true
            utils.raiseWindowToFront(window.wid)
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
        z: 90
        onClicked: root.invoked()
    }

    Text {
        id: title
        color: "#000000"
        text: "Title"
        font.pointSize: body.font.pointSize * 1.5
        font.bold: true
        anchors.top: parent.top
        anchors.margins: snoreMargin
        anchors.topMargin: snoreMargin / 2
        anchors.left: image.right
        anchors.right: closeButton.left
        textFormat: Text.StyledText
        font.family: snoreFont
        elide: Text.ElideRight
    }

    Text {
        id: body
        color: "#000000"
        text: "Body"
        font.pointSize: 10
        anchors.right: appIcon.left
        anchors.top: title.bottom
        anchors.bottom: parent.bottom
        anchors.left: image.right
        anchors.margins: snoreMargin
        anchors.topMargin: snoreMargin / 2
        wrapMode: Text.WrapAtWordBoundaryOrAnywhere
        onLinkActivated: Qt.openUrlExternally(link)
        textFormat: Text.StyledText
        font.family: snoreFont
        elide: Text.ElideRight
    }


    Image {
        id: image
        width: height
        smooth: true
        anchors.left: parent.left
        anchors.margins: snoreMargin
        anchors.bottom: parent.bottom
        anchors.top: parent.top
        z: 4
    }

    Image {
        id: appIcon
        height: root.height * 0.30
        width: root.height * 0.30
        smooth: true
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        anchors.margins: snoreMargin
    }

    Image {
        id: closeButton
        height: root.height * 0.20
        width: root.height * 0.20
        anchors.top: parent.top
        anchors.margins: snoreMargin
        anchors.right: parent.right
        z: 3
        source: "resources/close.png"
        smooth: true

        MouseArea {
            id: mouseAreaCloseButton
            anchors.fill: parent
            onClicked: root.dismissed()
        }
    }
}
