# ------------------------------------------------------------------------------
# Plan 9 from User Space
# ------------------------------------------------------------------------------

export PLAN9=/usr/local/plan9
export PATH=$PATH:$PLAN9/bin

# Set up the bash shell to work correctly inside Acme's win environment.
function winenv {
    alias ls="9 ls"
    alias g="find . -iregex '.*\.\(css\|html\|js\|tpl\|py\)' | xargs grep -n -e"
    alias git="git --no-pager"
    alias ack="ack-grep --nogroup --no-color"

    export EDITOR=E
    export PS1="$ "

    function cd {
	builtin cd $* && awd win
    }

    # Find all files under the current directory that start with the first
    # argument.
    function ff {
	find . -iname $1*
    }

    echo "You're all set. Enjoy"'!'
    echo
}

function 9tenv {
    alias ls="$PLAN9/bin/ls"
    alias man="man -P cat"

    export EDITOR=E

    echo "You're all set. Enjoy"'!'
}
