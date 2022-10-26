# `.zshenv' is sourced on *all* invocations of the shell,
# unless the -f option is set.
# It should contain commands to set the command search path, plus other
# important environment variables.
# `.zshenv' should not contain commands that produce output or assume the
# shell is attached to a tty.

#Charset & language
export LANG=en_US.UTF-8
export LANGUAGE=
export LC_TIME=en_DE.UTF-8
export LC_NUMERIC=en_DE.UTF-8
export LC_ALL=

if [[ $XDG_SESSION_TYPE == "wayland" ]];
then
   export MOZ_ENABLE_WAYLAND=1
   export QT_QPA_PLATFORM=wayland-egl
#   export QT_WAYLAND_FORCE_DPI=physical
fi

# Source additional workstation dependend settings
source ~/.zshenv.local
