# -*- mode:sh -*-

bashdir=$HOME/etc/dotfiles/bash

source $bashdir/config.sh
source $bashdir/plan9.sh

# ------------------------------------------------------------------------------
# Environment Variables
# ------------------------------------------------------------------------------

export EDITOR="emacsclient"
export HISTCONTROL=ignoredups  # don't save duplicates in history
export LC_CTYPE=en_US.UTF-8    # required for svn to handle non-ASCII filenames
export MANWIDTH=80

# ------------------------------------------------------------------------------
# Aliases
# ------------------------------------------------------------------------------

alias dirs='dirs -v'
alias ll='ls -l'
alias ssh='ssh -o ServerAliveInterval=60'
alias t='todo -a'

# Emacs
alias ec='emacsclient'

# Top
alias tu='top -o cpu'           # cpu
alias tm='top -o vsize'         # memory

# ------------------------------------------------------------------------------
# Settings
# ------------------------------------------------------------------------------

shopt -s extglob                # Turn on extended pattern matching

# ------------------------------------------------------------------------------
# Prompt
# ------------------------------------------------------------------------------

# PS1="\n$GREEN\u$NO_COLOR:$CYAN\w$NO_COLOR\n-> "
PS1="\n\u:\w\n-> "

# ------------------------------------------------------------------------------
# Local Configuration
# ------------------------------------------------------------------------------

# Do this last, since I may want to override some of the above settings in the
# local file.

localfile=$bashdir/local.sh
if [ -f $localfile ]
then
    source $localfile
fi
