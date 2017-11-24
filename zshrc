# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
export ZSH_THEME="jay"
# export ZSH_THEME="dogenpunk"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want disable red dots displayed while waiting for completion
# DISABLE_COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git npm osx sublime history-substring-search django cloudapp dircycle dirhistory git-extras pip tmux tmuxinator vagrant web-search virtualenvwrapper)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
# export PATH=/usr/local/share/npm/bin/:/usr/local/bin::/Library/PostgreSQL/9.2/bin/Users/jay/Scripts:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin:/usr/local/share/npm/bin/:/Library/PostgreSQL/9.2/bin:/Users/jay/Scripts
export NODE_PATH=/usr/local/lib/node_modules

export EDITOR=vim

source ~/.bin/tmuxinator.zsh

# if [ -n "$BASH_VERSION" ] || [ -n "$ZSH_VERSION" ]; then
#   source /usr/local/opt/chruby/share/chruby/chruby.sh
#   chruby 2.1.3
# fi

alias rb="bundle exec ruby"
alias tmux="unset TMUX && tmux"
alias fuck='$(thefuck $(fc -ln -1))'
alias build="gulp build-this -f"
alias watchify="gulp watchify-this -f"

# Setup vimkeys
# bindkey -v
# function zle-line-init zle-keymap-select {
#   VIM_PROMPT="%{$FG[068]%}[% %{$fg_bold[cyan]%}NORMAL%{$FG[068]%}]% %{$reset_color%}"
#   VIM_EDIT_PROMPT="%{$FG[068]%}[% %{$fg_bold[cyan]%}EDIT%{$FG[068]%}]% %{$reset_color%}"
#   RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/$VIM_EDIT_PROMPT} "
#   zle reset-prompt
# }
# 
# zle -N zle-line-init
# zle -N zle-keymap-select

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$HOME/bin:$PATH"
