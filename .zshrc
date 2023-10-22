eval "$(starship init zsh)"

export TERM=alacritty

autoload -Uz compinit && compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

alias em="emacsclient -c -n -a ''"

alias r="radian"
alias gaa="git add ."
alias gcm="git commit -m "
alias gp="git push"
alias gpf!="git push --force"

alias -s txt=em
alias -s el=em
alias -s org=em

alias -g updatesys="brew update && brew upgrade"

source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
