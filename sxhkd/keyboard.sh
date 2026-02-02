#!/bin/bash
current=$(setxkbmap -query | grep layout | awk '{print $2}')

if [ "$current" = "tr" ]; then
    setxkbmap -layout us
    notify-send "Keyboard: English Q"
else
    setxkbmap -layout tr
    notify-send "Keyboard: Turkish Q"
fi
