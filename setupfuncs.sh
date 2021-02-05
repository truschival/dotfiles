RED='\033[0;91m'
GREEN='\033[0;92m'
YELLOW='\033[0;93m'
BLUE='\033[0;94m'
MAGENTA='\033[0;95m'
RES_ATTRS='\033[0m'

function log_debug() {
    if [ $VERBOSITY -ge 7 ]; then
	echo "$@"
    fi
}

function log_info() {
    if [ $VERBOSITY -ge 5 ]; then
	echo -e $GREEN "$@" $RES_ATTRS
    fi
}

function log_warn() {
    if [ $VERBOSITY -ge 4 ]; then
	echo -e $YELLOW "$@" $RES_ATTRS
    fi
}

function log_error() {
    if [ $VERBOSITY -ge 3 ]; then
	echo -e $RED "$@" $RES_ATTRS
    fi
}

##
# Create a link and ask if link exists and call was not forced
##
function create_link() {
    local target=$1
    local link=$2

    if [ -e $link ] ;
    then
	if [ ! -z $FORCE_LINK ] ;
	then
	    log_warn "link $link exists, overwrite forced"
	    [ ! -z $DRYRUN ] && ln -sf $target $link
	    return
	fi

    	log_warn "File $link exists"
	read -p  " overwrite? [N|y] ?" -n 1 -r
	if [[ $REPLY =~ ^[Yy]$ ]]
	then
	    log_debug "LINK: $2 --> $1 "
	    [ ! -z $DRYRUN ] && ln -sf $target $link
	else
	    log_info "Link $2 skipped"
	fi

    else # Link did not exist
	log_info "creating link: $2 --> $1 "
	[ ! -z $DRYRUN ] && ln -s $target $link
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
	log_debug "--"
	local dotfile=$(basename $file)
	local link_name=${dotfile#dot}
	if [ ! -z $WS_OVERRIDE ] && [ -e $WS_OVERRIDE/$dotfile ]
	then
	    log_info "Using override $WS_OVERRIDE/$dotfile for $file"
	    local src_root=$(realpath $WS_OVERRIDE)
	else
	    [ ! -z $WS_OVERRIDE ] &&
		log_info "no override found [$WS_OVERRIDE/$dotfile]"
	    local src_root=$(realpath $src_dir)
	fi
	create_link $src_root/$dotfile $target_dir/$link_name
    done
}

################################################################################

##
# Find directories in src_dir, check if corresponding dir in WE exists
# Setup target directory structure and create link
##
function setup_links_in_subdir(){
    local src_dir=$1
    local target_root=$2

    # only take subdirectories, not $src_dir
    for file in $(find $src_dir -mindepth 1 -type f )
    do
	log_debug "--"
	local local_dirname=$(dirname $file)
	local confdir_name=dot.${local_dirname#*dot.}
	local source_file=$(basename $file)
	log_debug "confdir_name: $confdir_name | $source_file"

	##
	# Check if a file in a corresponding path in WS_OVERRIDE exists
	# that matches the name and relative path
	# If so take this file as link source
	##
	if [ ! -z $WS_OVERRIDE ] && [ -e $WS_OVERRIDE/$confdir_name/$source_file ]
	then
    	    local link_source=$(realpath $WS_OVERRIDE/$confdir_name/$source_file)
	    log_warn "Using $WS_OVERRIDE/$confdir_name/$source_file for $source_file"
	else
	    log_info "$WS_OVERRIDE/$dotfile does not exist"
	    local link_source=$(realpath $file)
	fi

	# Create target structure
	target_dir=${confdir_name#*dot.}
	log_debug "TargetDir: $target_dir"
	log_debug "Target_root: $target_root"
	target_dir="$target_root/.$target_dir"
	log_debug "Full target_dir: $target_dir"

	[ ! -z $DRYRUN ] && mkdir -p $target_dir
	create_link $link_source $target_dir/$source_file
    done
}
