
**WARNING:**

These are some of MY configuration files. While they are public they are not
intended to work perfectly on your machine. Especially don't expect the
``setup_env.sh`` script work on your machine. It barely works here.

That being said - feel free to copy whatever you need.

# setup_env.sh

This script creates symbolic links from the home directory and subdirs to the
files in this repository. If a file on the top level repository exists with a
name `dot.<something>` a link will be created in ``$HOME``.
E.g.
``~/.gitconfig -> /home/ruschi/dotfiles/VillaStraylight/dot.gitconfig``

It also creates links to files from subdirectories (``dot.config`` ``dot.gnupg``
``dot.emacs.d`` ``dot.ssh``) of this repository.  The script should take care of
creating sub-directories for the links if the source file is in a sub-directory
of these directories.
E.g.
```
~/.config/i3/config -> ~/dotfiles/dot.config/i3/config
```
The script automatically created the directory ``~/.config/i3/``.

I chose only some files like configuration to be included in the
repository. Other files, like keys will remain on the machine.

## options

*  ``-w`` Sets an override directory. Not all machines are equal. Sometimes I
   have different configurations for different machines, e.g. ``.gitconfig`` at
   work. To override ``/dot.gitconfig`` on a machine ``foo`` create a file
   called ``/foo/dot.gitconfig`` and run the ``setup_env.sh -w foo`` on the
   machine ``foo``.\
   In this case machine ``foo`` will create all common links but
   ``~/.gitconfig`` will point to ``dotfiles/foo/dot.gitconfig`` This only works
   with configuration files present in the main directory tree. It does not add
   new files.

*  ``-f`` force links. If a link already exists it will be overwritten without
   asking. **USE WITH CARE**

*  ``-n`` Dry run. Do not create links. **This still sets up mail and gpg - USE
   WITH CARE**
