# Initialization ---------------------------------------------------------------

# Make the Caps Lock key a Control key.
# setxkbmap -option ctrl:nocaps

# Environment variables --------------------------------------------------------

set -x EDITOR "emacsclient"
set -x MANWIDTH 80
set -x PYTHONPATH $HOME/Projects/Offerpop/Engine $HOME/Projects/Offerpop/Lib

# These are all required for the AWS command-line client programs.
set -x AWS_CREDENTIAL_FILE $HOME/.aws/credentials                             
set -x AWS_ELB_HOME $HOME/bin/aws/elb                                         
set -x AWS_IAM_HOME $HOME/bin/aws/iam                                         
set -x AWS_RDS_HOME $HOME/bin/aws/rds                                         
set -x EC2_KEYPAIR ec2                                                        
set -x EC2_URL https://ec2.us-east-1.amazonaws.com                            
set -x EC2_PRIVATE_KEY $HOME/.aws/ec2/pk-WLHO6L7VYNE7WMWQV4KGZIZSG6YWLJGR.pem 
set -x EC2_CERT $HOME/.aws/ec2/cert-WLHO6L7VYNE7WMWQV4KGZIZSG6YWLJGR.pem      

# Configuration ----------------------------------------------------------------

function fish_prompt
    printf "%s:%s%s %s> " (whoami) (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
end

# "Aliases" --------------------------------------------------------------------

function c
    switch $argv[1]
        case "op"
            cd $HOME/Projects/Offerpop
        case "down"
            cd $HOME/Downloads
    end
end

complete -c c -f                # Don't include files in completion
complete -c c -a op -d "Offerpop Project Root"
complete -c c -a down -d "Downloads Folder"

function ec
    emacsclient $argv
end

# Functions --------------------------------------------------------------------

function play
    set -l playlist_path $HOME/Music/Playlists
    set -l playlist_name $argv
    mplayer -playlist $playlist_path/$playlist_name.pls
end

# Completions ------------------------------------------------------------------

function __get_playlists
    for playlist in $HOME/Music/Playlists/*.pls;
        basename $playlist | sed 's/\.pls$//'
    end;
end

complete -f -c play -a '(__get_playlists)'