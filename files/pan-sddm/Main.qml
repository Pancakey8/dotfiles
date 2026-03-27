// Slop
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

Rectangle {
    id: container
    width: 1920
    height: 1080
    color: "#fcfcfc"

    Image {
        anchors.fill: parent
        source: "./bg.png"
        fillMode: Image.PreserveAspectCrop
        smooth: true
    }

    Rectangle {
        anchors.fill: parent
        color: "#80202020"
    }

    // Internal variables
    property string savedUsername: ""
    // sessionModel.lastIndex is the SDDM default; 0 is the fallback
    property int sessionIndex: sessionModel.lastIndex >= 0 ? sessionModel.lastIndex : 0

    // --- Main Login Box ---
    Rectangle {
        id: loginBox
        anchors.centerIn: parent
        width: 550
        height: 480
        color: "#fcfcfc"
        border.color: "#202020"
        border.width: 2
        radius: 0

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: 40
            spacing: 15

            Text {
                id: greeting
                text: "Welcome"
                color: "#202020"
                font.family: "Inter"
                font.pointSize: 22
                font.bold: true
                Layout.alignment: Qt.AlignHCenter
                elide: Text.ElideRight
            }

            TextField {
                id: inputField
                Layout.fillWidth: true
                placeholderText: "Username"
                font.family: "Inter"
                color: "#202020"
                font.pointSize: 14
                topPadding: 16
                bottomPadding: 16
                horizontalAlignment: Text.AlignHCenter
                focus: true
                
                background: Rectangle {
                    color: "#fcfcfc"
                    border.color: "#202020"
                    border.width: 2
                    radius: 0
                }

                onAccepted: {
                    if (container.state === "") proceedToPassword()
                    else attemptLogin()
                }
            }

            RowLayout {
                Layout.alignment: Qt.AlignHCenter
                spacing: 10

                Button {
                    id: backButton
                    text: "Back"
                    visible: container.state === "showPassword"
                    topPadding: 16
                    bottomPadding: 16
                    contentItem: Text {
                        text: ""
                        color: "#202020"
                        font.bold: true
                        font.pointSize: 14
                        font.family: "FontAwesome"
                        horizontalAlignment: Text.AlignHCenter
                        verticalAlignment: Text.AlignVCenter
                        elide: Text.ElideRight
                    }
                    background: Rectangle {
                        implicitWidth: 80
                        implicitHeight: 40
                        color: backButton.hovered ? "#99c5ff" : "#fcfcfc"
                        border.color: "#202020"
                        border.width: 2 
                        radius: 0
                    }
                    onClicked: {
                        container.state = ""
                        inputField.text = container.savedUsername
                        inputField.echoMode = TextInput.Normal
                        inputField.forceActiveFocus()
                    }
                }

                Button {
                    id: actionButton
                    text: ""
                    topPadding: 16
                    bottomPadding: 16
                    contentItem: Text {
                        text: actionButton.text
                        color: "#202020"
                        font.pointSize: 14
                        horizontalAlignment: Text.AlignHCenter
                        verticalAlignment: Text.AlignVCenter
                        elide: Text.ElideRight
                        font.family: "FontAwesome"
                    }
                    background: Rectangle {
                        implicitWidth: 80
                        implicitHeight: 40
                        color: actionButton.hovered ? "#99c5ff" : "#fcfcfc"
                        border.color: "#202020"
                        border.width: 2
                        radius: 0
                    }
                    onClicked: {
                        if (container.state === "") proceedToPassword()
                        else attemptLogin()
                    }
                }
            }
        }
    }

    // --- Session Selector (Bottom Left) ---
    ComboBox {
        id: sessionPicker
        anchors.bottom: parent.bottom
        anchors.left: parent.left
        anchors.margins: 30
        width: 200
        height: 40
        
        model: sessionModel
        textRole: "name"
        currentIndex: container.sessionIndex

        onActivated: container.sessionIndex = index

        popup.y: -popup.height - 10

        popup.background: Rectangle {
            border.color: "#202020"
            border.width: 2
            color: "#fcfcfc"
            radius: 0
        }

        // Style for the "clutter" element
        delegate: ItemDelegate {
            width: sessionPicker.width
            hoverEnabled: true
            contentItem: Text {
                text: model.name
                color: "#202020"
                font.bold: true
                font.family: "Inter"
                horizontalAlignment: Text.AlignHCenter
                verticalAlignment: Text.AlignVCenter
                elide: Text.ElideRight
            }
            background: Rectangle {
                implicitHeight: 40
                color: hovered ? "#cacaca" : "#fcfcfc"
            }
        }

        background: Rectangle {
            color: "#fcfcfc"
            border.color: "#202020"
            border.width: 2
            radius: 0
        }

        contentItem: Text {
            text: sessionPicker.displayText
            color: "#202020"
            font.bold: true
            font.family: "Inter"
            leftPadding: 10
            horizontalAlignment: Text.AlignHCenter
            verticalAlignment: Text.AlignVCenter
            elide: Text.ElideRight
        }
    }

    // --- Power Controls (Top Right) ---
    RowLayout {
        anchors.top: parent.top
        anchors.right: parent.right
        anchors.margins: 30
        spacing: 15

        // Restart Button
        Button {
            id: rebootButton
            hoverEnabled: true
            onClicked: sddm.reboot()
            
            contentItem: Text {
                text: "" // FontAwesome sync/reboot icon
                color: "#202020"
                font.family: "FontAwesome"
                font.pointSize: 16
                horizontalAlignment: Text.AlignHCenter
                verticalAlignment: Text.AlignVCenter
            }

            background: Rectangle {
                implicitWidth: 50
                implicitHeight: 50
                color: rebootButton.hovered ? "#eadb56" : "#fcfcfc"
                border.color: "#202020"
                border.width: 2
            }
        }

        // Shutdown Button
        Button {
            id: shutdownButton
            hoverEnabled: true
            onClicked: sddm.powerOff()
            
            contentItem: Text {
                text: "" // FontAwesome power icon
                color: "#202020"
                font.family: "FontAwesome"
                font.pointSize: 16
                horizontalAlignment: Text.AlignHCenter
                verticalAlignment: Text.AlignVCenter
            }

            background: Rectangle {
                implicitWidth: 50
                implicitHeight: 50
                color: shutdownButton.hovered ? "#d8463e" : "#fcfcfc"
                border.color: "#202020"
                border.width: 2
            }
        }
    }

    // Logic Functions
    function proceedToPassword() {
        if (inputField.text !== "") {
            container.savedUsername = inputField.text
            container.state = "showPassword"
            inputField.text = ""
            inputField.echoMode = TextInput.Password
            inputField.forceActiveFocus()
        }
    }

    function attemptLogin() {
        sddm.login(container.savedUsername, inputField.text, container.sessionIndex)
    }

    states: [
        State {
            name: "showPassword"
            PropertyChanges { target: greeting; text: "Hello, " + container.savedUsername }
            PropertyChanges { target: inputField; placeholderText: "Password" }
            PropertyChanges { target: actionButton; text: "" }
        }
    ]

    Component.onCompleted: inputField.forceActiveFocus()
}