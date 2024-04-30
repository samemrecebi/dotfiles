# samemrecebi's dotfiles
These configuration files are the ones that I use in my daily life on my Mac OS and NixOS machines.

## Contents
This repo contains configurations for:
- git
- zsh
- alacritty
- emacs
- starship

## How to apply
GNU Stow is a requirement for properly applying these dotfiles.to apply this file clone this repo to the home folder and then run:

``` bash
stow .
```

inside the repo.

The configurations are mostly cross platfrom but for the ones that need seperate configs like Zsh, I added statements that uses the lines needed for the MacOS or Linux.
