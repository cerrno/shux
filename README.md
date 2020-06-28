# shux
A programming language for particle-based physics simulation

# Introduction

Smoothed Particle Hydrodynamics will henceforth be the bane of our existence.

# Language Reference Manual

The language reference manual for shux can be found [here](https://github.com/cerrno/shux/blob/master/shux%20lrm.pdf)
# Authors
- [Lucas Schuermann](http://lvs.io/)
- John Hui
- Mert Ussakli
- Andy Xu

# License
[MIT](https://lucasschuermann.com/license.txt)

# Compiling

Run the `install_deps.sh` script to set up submodules, install OCaml, and install third-party libraries.

This script is maintained on Ubuntu 15.04, although it will probably work on Ubuntu 14/16.x,
macOS 10.10+, and Debian as well.

Once you have the dependencies, just `cd` into the directory you want and `make`.

# Contributing

1. Grab the lastest master: `git checkout master && git pull origin master`
2. Create your branch: `git checkout -b alice_feature_name`
3. Make some changes: `git commit ...`
4. Squash commits: `git rebase -i master`, then change everything except the first commit to `squash`
5. Rebase on the latest master: `git checkout master && git pull origin master && git rebase master alice_feature_name`
6. Run tests: `make symlink && make && make tests`
7. Push for code review: `git push -f origin alice_feature_name` (only use push -f on feature branches, not master)

# Syntax Highlighting

This would be nice to eventually have...
