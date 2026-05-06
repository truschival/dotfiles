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

function log_info() {
    echo -e "[\\e[1;94mINFO\\e[0m] $*"
}

function log_warn() {
    echo -e "[\\e[1;93mWARN\\e[0m] $*"
}

function log_error() {
    echo -e "[\\e[1;91mERROR\\e[0m] $*"
}

## Returns the version of the package from git tags
## Examples:
## V1.2 -> 1.2
## v1.2.3 -> 1.2.3
## v1.2.3-1-g1234567 -> 1.2.3+post1-1234567
## 1234567 -> 0.0.0+pre1234567
##
function pep_version_from_git() {
    pep_version=$(git describe --tags --long --always  | sed -E '
          s/^[vV]//;
          # Case 1: exactly at tag (0 commits after tag)
          s/^([0-9]+(\.[0-9]+)*(\.[0-9]+)*[a-zA-Z0-9]*)-0-g[0-9a-f]+$/\1/;
          # Case 2: commits after tag
          s/^([0-9]+(\.[0-9]+)*(\.[0-9]+)*[a-zA-Z0-9]*)-([1-9][0-9]*)-g([0-9a-f]+)$/\1+post\4-\5/;
          # Case 3: no tag, just hash
          t end;
          s/^([0-9a-f]+)$/0.0.0+pre\1/;
          :end
        ')
    echo $pep_version
}


function build_kaniko(){
	title=$1
	destination=$AWS_ACCOUNT_ID.dkr.ecr.$AWS_DEFAULT_REGION.amazonaws.com/$title

	metadata="--label org.opencontainers.image.created='$(date)' \
 			  --label org.opencontainers.image.title='${title}'"

	docker run -it --rm --name kaniko  \
		   -v $(pwd):/workspace \
		   -eAWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID \
		   -eAWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY \
		   gcr.io/kaniko-project/executor:v1.23.2-debug \
		   --context=/workspace --dockerfile Dockerfile \
		   $metadata \
		   --build-arg PIP_EXTRA_INDEX_URL=$PIP_EXTRA_INDEX_URL \
		   --destination $destination

}

function aws_docker_login(){
	aws ecr get-login-password | \
		docker login \
			   --username AWS \
			   --password-stdin \
		$AWS_ACCOUNT_ID.dkr.ecr.$AWS_DEFAULT_REGION.amazonaws.com 
}

# Source additional workstation dependend settings
source ~/.zshenv.local
# source /usr/share/virtualenvwrapper/virtualenvwrapper.sh
export SSH_AUTH_SOCK=${XDG_RUNTIME_DIR}/ssh-agent.socket
. "$HOME/.cargo/env"
