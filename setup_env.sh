#!/bin/bash

# set -x
# set -e


##
# Create a link and ask if link exists and call was not forced
##
function create_link(){
	local target=$1
	local link=$2

	echo "LINK: $2 --> $1 "

	if [ -e $link ] && [ -z $FORCE_LINK ] ;
	then
		read -p "File $link exists, overwrite? [N|y] " -n 1 -r
		echo # new line
		if [[ $REPLY =~ ^[Yy]$ ]]
		then
			ln -sf $target $link
		fi	
	else
		ln -s $FORCE_LINK $target $link
	fi
}

################################################################################

##
# Find files local to src_dir and check if there is a corresponding file in
# work environment
##
function setup_dot_links(){
	local src_dir=$1
	local target_dir=$2
	
	for file in $(find $src_dir -maxdepth 1 -type f -name dot\* )
	do
		local dotfile=$(basename $file)
		local link_name=${dotfile/dot/}

		if [ ! -z $WS_OVERRIDE ] && [ -e $WS_OVERRIDE/$dotfile ]
		then
			echo "Using override $WS_OVERRIDE/$dotfile for $file"
			local src_root=$(realpath $WS_OVERRIDE)
		else
			local src_root=$(realpath $src_dir)
		fi
		create_link ${src_root}/${dotfile}  $target_dir/$link_name
	done				
}
################################################################################

##
# Find directories in src_dir, check if corresponding dir in WE exists
# Setup target directory structure and create link
##
function setup_links_in_subdir(){
	local src_dir=$1
	local target_dir=$2
	
	mkdir -p $target_dir

	# only take subdirectories, not $src_dir
	for file in $(find $src_dir -mindepth 1 -type f )
	do
		local confdir_name=$(dirname $file)
		local link_name=${confdir_name/dot/}

		if [ ! -z $WS_OVERRIDE ] && [ -e $WS_OVERRIDE/$confdir_name ]
		then
			echo "Using override $WS_OVERRIDE/$confdir for $confdir"
			local src_root=$(realpath $WS_OVERRIDE)
		else
			local src_root=$(realpath $confdir_name)
		fi
		
		create_link ${src_root}  $target_dir/$link_name
	done				
}

################################################################################


function i3_conf_fixup(){
	echo fixup
}

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
	-h print this help
EOF
}


while getopts w:hf option
do
	case "${option}"
	in
		w)
			WS_OVERRIDE=${OPTARG}
			;;
		f)
			FORCE_LINK="-f"
			;;
		*)
			print_help $0
			;;
	esac
done

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# all files flat in directory
setup_dot_links $SCRIPT_DIR $HOME
# Setup sub folder directories
setup_links_in_subdir dot.config $HOME
