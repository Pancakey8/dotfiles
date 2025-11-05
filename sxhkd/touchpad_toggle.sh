#!/usr/bin/env bash

# replace with your touchpad name
TOUCHPAD="ELAN1203:00 04F3:307A Touchpad"

# check current state
STATE=$(xinput list-props "$TOUCHPAD" | grep "Device Enabled" | awk '{print $4}')

if [ "$STATE" -eq 1 ]; then
    xinput disable "$TOUCHPAD"
    notify-send "Touchpad Disabled" "Touchpad is now disabled."
else
    xinput enable "$TOUCHPAD"
    notify-send "Touchpad Enabled" "Touchpad is now enabled."
fi
