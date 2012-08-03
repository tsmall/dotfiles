#!/bin/bash
set -e
set -u

DOTFILES_PATH="$HOME/etc/dotfiles/sublime"
SUBLIME_PATH="$HOME/.config/sublime-text-2/Packages/User"

function create_symlink {
    filename="$1"
    destination=$SUBLIME_PATH/$filename

    if [[ -e $filename ]]
    then
        echo "File already exists: $destination"
    else
        ln -s $DOTFILES_PATH/$filename $destination
        echo "Created symlink: $destination"
    fi
}

create_symlink Preferences.sublime-settings
create_symlink Python.sublime-settings
