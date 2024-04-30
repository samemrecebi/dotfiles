eval "$(starship init zsh)"
eval "$(zoxide init zsh)"

autoload -Uz compinit && compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

alias em="emacsclient -c -n -a ''"

case "$OSTYPE" in
  darwin*)
    source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
    alias -g updatesys="brew update && brew upgrade"
  ;;
  linux*)
    HISTFILE=~/.zsh_history
    HISTSIZE=10000
    SAVEHIST=10000
    setopt appendhistory
  ;;
esac