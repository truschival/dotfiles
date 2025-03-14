#!/bin/bash

# set -x
# set -e

VERBOSITY=7
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
# use NEW_HOME for testing purposes usually it points to HOME
NEW_HOME=$HOME
DRYRUN=''

# Source util functions
[ -e ./setupfuncs.sh ] && . ./setupfuncs.sh

# Install packages
log_notice "Install required packages"
read -p  " install? [N|y] ?" -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    sudo apt update &&
	sudo apt install -y wget gnupg2 gnupg-agent \
	     dirmngr cryptsetup scdaemon pcscd secure-delete \
	     yubikey-personalization \
	     libsecret-tools \
	     i3 i3-wm dex suckless-tools feh pulseaudio-utils i3lock xautolock \
	     sway sway-backgrounds swaybg swayidle swaylock sway-notification-center \
	     tofi waybar slurp grim wayland-protocols \
	     qlipper imagemagick x11-utils udiskie unclutter-xfixes xdg-utils \
	     mu4e isync gnutls-bin \
	     fonts-hack fonts-symbola \
		 pipenv virtualenv virtualenvwrapper \
		 git-flow zsh
fi

################################################################################
function cliphist_setup(){
    log_notice "setup cliphist"
	CLIPHIST_URL="https://github.com/sentriz/cliphist/releases/download/v0.6.1/v0.6.1-linux-amd64"
	wget -O $NEW_HOME/.local/bin/cliphist $CLIPHIST_URL
	chmod 755 $NEW_HOME/.local/bin/cliphist 
}


################################################################################
function gnupg_setup(){
    log_notice "setup gnupg config"
    read -p  " skip? [N|y] ?" -n 1 -r
    echo # new line
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
	return
    fi

    mkdir -p $NEW_HOME/.gnupg
    setup_links_in_subdir dot.gnupg $NEW_HOME

    chmod 700 $NEW_HOME/.gnupg
    chmod 600 $NEW_HOME/.gnupg/*

    # Setup systemd user config
    systemctl --user enable gpg-agent.socket
    systemctl --user enable dirmngr.socket

    gpg --import $SCRIPT_DIR/0x0A22736A4AB577ED.pub.asc
    gpg --import $SCRIPT_DIR/0x801A13F19F7ACBB9.pub.asc
}

################################################################################
function mail_setup(){
    mkdir -p $NEW_HOME/Maildir/ruschival.de
    log_notice "setup local mail"
    read -p  " skip? [N|y] ?" -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
	return
    fi

    log_info "Creating $NEW_HOME/.mailpass.gpg for encrypted IMAP password"
    read -p "enter IMAP password followed by ENTER" -s -r
    echo $REPLY > $NEW_HOME/.mailpass
    unset $REPLY
    gpg -e -r 0x801A13F19F7ACBB9  $NEW_HOME/.mailpass
    shred -u $NEW_HOME/.mailpass

    log_info "synching mail"
    [ -e /usr/bin/mbsync ] && /usr/bin/mbsync -a -V
    log_info "Disabling systemd user services mbsync.timer mbsync.service"
    systemctl --user disable mbsync.timer
    systemctl --user disable mbsync.service

    mu init --my-address=thomas@ruschival.de --my-address=t.ruschival@gmail.com
}

################################################################################
function wallpaper_setup(){
    ln -s $SCRIPT_DIR/wallpapers $NEW_HOME/.wallpapers

    screensize=$(xdpyinfo | awk '/dimensions/{print $2}')
    convert -scale "$screensize!" $NEW_HOME/.wallpapers/lockscreen.png \
	    $NEW_HOME/.wallpapers/lockscreen-scaled.png

}

################################################################################
function print_help() {
    cat <<EOF
$0 creates symbolic links in \$HOME and respective sub directories to
files provided by this repository.
For this to work this script expects the configuration files to be in the
same directory structure in the repository as they will be located in \$HOME.
Any hidden files or directories are prefixed with 'dot.' e.g.
$HOME/.zshrc will link to ./dot.zshrc
$HOME/.config/i3/config will link to ./dot.config/i3/config

Some configuration files are different on various machines. You can specify a
'work environment' for this. If you give a work environment (\$WE) and for a
given file in ./ a matching file in \$WE is found, the file in \$WE will be
linked. E.g.:
./dot.zshrc and \$WE/dot.zshrc exist  $HOME/.zshrc will link to \$WE/dot.zshrc

options:
	-f force link creation !WARNING: overwrites your config file(s) in $HOME!
	-w work environment refers to a folder with local overrides/extensions for
	   config
	-n dry-run, don't create (all links) - mail and gpg setup runs anyway
	-h print this help
EOF
}


################################################################################
while getopts w:hnf option
do
    case "${option}"
    in
	w)
	    WS_OVERRIDE=${OPTARG}
	    ;;
	f)
	    FORCE_LINK="-f"
	    ;;
	n)
	    DRYRUN="DRYRUN"
	    #	    NEW_HOME=/tmp/
	    ;;
	*)
	    print_help $0
	    ;;
    esac
done

# install ohmyzsh https://github.com/ohmyzsh/ohmyzsh
log_notice "Install Zsh"
read -p  " Install oh-my-zsh? [N|y] ?" -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    sh -c "$(wget -O- \
   	https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
fi

# Create local overrides
touch $NEW_HOME/.zshenv.local
touch $NEW_HOME/.zsh_aliases.local

# create config folder if not setup
[ -d $NEW_HOME/.config ] || mkdir -p $NEW_HOME/.config

# create config folder if not setup
[ -d $NEW_HOME/.local/bin ] || mkdir -p $NEW_HOME/.local/bin

# Download and install cliphist
cliphist_setup

# link wallpapers
wallpaper_setup

# all configfiles located in $NEW_HOME e.g. ~/.xsessionrc, ~/.gitconfig
log_notice "Creating links in $NEW_HOME"
setup_dot_links $SCRIPT_DIR $NEW_HOME

# Setup sub folder directories
log_notice "Creating links in .config and subdirectories"
setup_links_in_subdir dot.config $NEW_HOME
log_notice "Creating links in .emacs.d and subdirectories"
setup_links_in_subdir dot.emacs.d $NEW_HOME
systemctl enable --user emacs

# Setup SSH-Agent service
systemctl enable --user ssh-agent

# Setup gnupg
gnupg_setup

# Setup mail
mail_setup

systemctl --user daemon-reload
