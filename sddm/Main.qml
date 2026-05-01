import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

Rectangle {
    id: container
    width: 1600
    height: 900
    color: config.background

    property int sessionIndex: sessionBar.currentIndex

    Rectangle {
        id: topBar
        anchors.top: parent.top
        width: parent.width
        height: 50
        color: config.bgDark
        z: 2

	Text {
	    anchors.centerIn: parent
	    id: dateDisplay
	    color: config.foreground
	    font.pixelSize: 16
	    font.family: config.font
	    text: Qt.formatDateTime(new Date(), "ddd, d MMM yyyy ⋅ HH:MM") 
	    Layout.alignment: Qt.AlignHCenter
	}

        RowLayout {
            anchors.fill: parent
            anchors.leftMargin: 15
            anchors.rightMargin: 0

            ComboBox {
                id: sessionBar
                Layout.preferredWidth: 200
		Layout.preferredHeight: 40
                model: sessionModel
                textRole: "name"
                currentIndex: sessionModel.lastIndex

                popup.y: -popup.height - 10

		popup.background: Rectangle {
		    border.color: config.accent
		    border.width: 2
		    color: config.foreground
		    radius: 0
		}
		
		delegate: ItemDelegate {
		    width: sessionBar.width
		    hoverEnabled: true
                    contentItem: Text {
		        font.family: config.font
                        text: sessionBar.displayText
                        color: config.foreground
                        verticalAlignment: Text.AlignVCenter
                        leftPadding: 10
                    }
		    background: Rectangle {
		        implicitHeight: 40
			color: hovered ? config.bgAccent : config.background
		    }
		}

                contentItem: Text {
		    font.family: config.font
                    text: sessionBar.displayText
                    color: config.foreground
                    verticalAlignment: Text.AlignVCenter
                    leftPadding: 10
                }

                background: Rectangle {
                    color: config.bgAccent
                    radius: 8
                }
            }

            Item { Layout.fillWidth: true }

	    RowLayout {
                spacing: 0

                Button {
                    id: restartButton
                    text: ""
                    onClicked: sddm.reboot()
		    Layout.fillHeight: true
		    Layout.preferredWidth: 60
                    
                    contentItem: Text {
		        font.family: "Font Awesome"
                        text: restartButton.text
			font.pointSize: 16
                        color: config.foreground
                        horizontalAlignment: Text.AlignHCenter
                        verticalAlignment: Text.AlignVCenter
			elide: Text.ElideLeft
                    }
                    background: Rectangle {
                        color: config.yellow
			topLeftRadius: 8
			bottomLeftRadius: 8
                    }
                }

                Button {
                    id: shutdownButton
                    text: " "
                    onClicked: sddm.powerOff()
		    Layout.fillHeight: true
		    Layout.preferredWidth: 60
                    
                    contentItem: Text {
                        text: shutdownButton.text
			font.family: "Font Awesome"
			font.pointSize: 16
                        color: config.foreground
                        horizontalAlignment: Text.AlignHCenter
                        verticalAlignment: Text.AlignVCenter
			elide: Text.ElideLeft
                    }
                    background: Rectangle {
                        color: config.red
                        radius: 0
                    }
                }
            }
        }
    }

    ColumnLayout {
        anchors.centerIn: parent
        spacing: 30
        width: 450

        Text {
	    font.family: config.fontTitle
            text: "Welcome"
            color: config.foreground
            font.pointSize: 36
            Layout.alignment: Qt.AlignHCenter
            renderType: Text.NativeRendering
        }

        TextField {
            id: usernameField
            Layout.fillWidth: true
	    font.family: config.font
	    font.pointSize: 14
	    leftPadding: 20
	    rightPadding: 20
	    Layout.preferredHeight: 45
            placeholderText: "Username"
            focus: true
            color: config.foreground
            
            background: Rectangle {
                color: config.bgAccent
                radius: 8
                border.color: passwordField.activeFocus ? config.accent : "transparent"
            }
        }

        TextField {
            id: passwordField
            Layout.fillWidth: true
	    Layout.preferredHeight: 45
	    font.pointSize: 14
	    leftPadding: 20
	    rightPadding: 20
	    font.family: config.font
            placeholderText: "Password"
            echoMode: TextInput.Password
            focus: true
            color: config.foreground
            
            background: Rectangle {
                color: config.bgAccent
                radius: 8
                border.color: passwordField.activeFocus ? config.accent : "transparent"
            }

            Keys.onEnterPressed: sddm.login(usernameField.text, passwordField.text, sessionIndex)
            Keys.onReturnPressed: sddm.login(usernameField.text, passwordField.text, sessionIndex)
        }

        Button {
            id: loginButton
            text: ""
            Layout.fillWidth: true
	    Layout.preferredHeight: 55
            
            contentItem: Text {
	        font.family: "Font Awesome"
		font.pointSize: 16
                text: loginButton.text
                color: config.foreground
                horizontalAlignment: Text.AlignHCenter
                verticalAlignment: Text.AlignVCenter
            }

            background: Rectangle {
                color: config.accent
                radius: 8
            }

            onClicked: sddm.login(usernameField.text, passwordField.text, sessionIndex)
        }

        Text {
            id: errorMessage
            color: config.red
            text: ""
	    font.family: config.font
            font.pixelSize: 14
            Layout.alignment: Qt.AlignHCenter
            Layout.preferredHeight: 20
            horizontalAlignment: Text.AlignHCenter
            verticalAlignment: Text.AlignVCenter
        }
    }

    Connections {
        target: sddm
        function onLoginSucceeded() {
            errorMessage.text = ""
        }
        function onLoginFailed() {
            errorMessage.text = "Invalid username/password"
            passwordField.text = ""
            passwordField.focus = true
        }
    }
}