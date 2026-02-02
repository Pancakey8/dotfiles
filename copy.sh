#!/usr/bin/env bash
cp -r ~/.config/bspwm/ .
cp -r ~/.config/sxhkd/ .
cp -r ~/.config/polybar/ .
cp -r ~/.config/rofi/ .
cp -r ~/.config/picom/ .
cp -r ~/.config/kitty/ .
cp -r ~/.config/dunst/ .
cp -r ~/.config/nvim/ .
cp -r ~/.config/wallpaper.png .
mkdir ./.emacs.d
mkdir ./.emacs.d/acsl
cp ~/.emacs.d/init.el ./.emacs.d/init.el
cp ~/.emacs.d/acsl/acsl.el ./.emacs.d/acsl/acsl.el
mkdir ./.emacs.d.old
cp ~/.emacs.d.old/init.el ./.emacs.d.old/init.el
