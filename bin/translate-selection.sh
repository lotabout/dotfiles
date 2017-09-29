#!/bin/bash

translation=$(xclip -o -selection primary| trans -show-original n -show-prompt-message n -show-languages n -no-ansi :zh )
notify-send -t 10 -u low "translation" "$translation"
