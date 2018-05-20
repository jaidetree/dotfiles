#!/usr/bin/env zsh

setopt promptsubst
autoload -U add-zsh-hook

ZSH_THEME_GIT_PROMPT_PREFIX="%{$FG[015]%}[%{%B$FG[201]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%b%{$FG[015]%})%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%b%{$FG[015]%}](%{$FG[204]%}☂ "
ZSH_THEME_GIT_PROMPT_UNTRACKED="%b%{$FG[015]%}](%{$FG[204]%}☂ "
ZSH_THEME_GIT_PROMPT_CLEAN="%b%{$FG[015]%}](%{$FG[051]%}☯ "
ZSH_THEME_GIT_PROMPT_AHEAD="%{$FG[047]%}⛢ %{$reset_color%}"

# PROMPT='
# %{$FG[068]%}[%{$FG[051]%}%n%{$FG[015]%}@%{$FG[027]%}%m%{$FG[015]%}:%B%{$FG[085]%}${PWD/$HOME/~}%b%{$FG[068]%}] %{$FG[201]%}%B%#%b 
# %{$FG[051]%}%c%{$reset_color%} $(git_prompt_info)%B(☚☚💀 ☛☛ )%b %{$FG[051]%}➜ %{$reset_color%} '
#
PROMPT='
%{$FG[068]%}[%{$FG[051]%}%n%{$FG[015]%}@%{$FG[027]%}%m%{$FG[015]%}:%B%{$FG[085]%}${PWD/$HOME/~}%b%{$FG[068]%}] %{$FG[201]%}%B%#%b 
%{$FG[051]%}%c%{$reset_color%} $(git_prompt_info) $(git_prompt_ahead) %{$FG[201]%}➜ %{$reset_color%} '

RPROMPT=''

# ZSH_THEME_GIT_PROMPT_ADDED="%{$FG[068]%})[%{$fg[cyan]%}ADDED{$reset_color%}"
# ZSH_THEME_GIT_PROMPT_MODIFIED="%{$FG[068]%})[%{$fg[yellow]%}MODIFIED{$reset_color%}"
# ZSH_THEME_GIT_PROMPT_DELETED="%{$FG[068]%})[%{$fg[red]%}DELETED{$reset_color%}"
# ZSH_THEME_GIT_PROMPT_RENAMED="%{$FG[068]%})[{$fg[blue]%}RENAMED{$reset_color%}"
# ZSH_THEME_GIT_PROMPT_UNMERGED="%{$FG[068]%})[%{$fg[magenta]%}UNMERGED{$reset_color%}"
# ZSH_THEME_GIT_PROMPT_ADDED="%{$FG[068]%})[%{$fg[cyan]%}ADDED{$reset_color%}"
# ZSH_THEME_GIT_PROMPT_UNTRACKED="(%b%{$FG[204]%}☂ %{$reset_color%})"
