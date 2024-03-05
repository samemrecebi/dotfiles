source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
export TERM=alacritty

autoload -Uz compinit && compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

alias em="emacsclient -c -n -a ''"
alias -g updatesys="brew update && brew upgrade"

eval "$(starship init zsh)"
eval "$(zoxide init zsh)"
