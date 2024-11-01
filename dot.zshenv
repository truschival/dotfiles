# -*- mode: shell-script; -*-
# `.zshenv' is sourced on *all* invocations of the shell,
# unless the -f option is set.
# It should contain commands to set the command search path, plus other
# important environment variables.
# `.zshenv' should not contain commands that produce output or assume the
# shell is attached to a tty.

#Charset & language
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
#export LC_TIME=de_DE.UTF-8
#export LC_NUMERIC=de_DE.UTF-8
# export LC_ALL=
export WORKON_HOME=$HOME/.virtualenvs

HOME_LOCAL_BIN=$HOME/.local/bin
echo $PATH | grep  -q $HOME_LOCAL_BIN
ON_PATH=$?

if [[ $ON_PATH -ne 0 && -d $HOME_LOCAL_BIN ]]  ; then
    PATH="$HOME_LOCAL_BIN:$PATH"
fi

if [[ $XDG_SESSION_TYPE == "wayland" ]];
then
   export MOZ_ENABLE_WAYLAND=1
   export QT_QPA_PLATFORM=wayland-egl
#   export QT_WAYLAND_FORCE_DPI=physical
fi

# Source additional workstation dependend settings
source ~/.zshenv.local
source /usr/share/virtualenvwrapper/virtualenvwrapper.sh
export SSH_AUTH_SOCK=${XDG_RUNTIME_DIR}/ssh-agent.socket
. "$HOME/.cargo/env"
