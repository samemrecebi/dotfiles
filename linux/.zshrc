eval "$(starship init zsh)"

case `uname` in
  Darwin)
    alias -g updatesys="brew update && brew upgrade"
    source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
  ;;
  Linux)
    HISTFILE=~/.zsh_history
    HISTSIZE=10000
    SAVEHIST=10000
    setopt appendhistory
  ;;
esac


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
