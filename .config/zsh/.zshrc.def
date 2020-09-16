# Start configuration added by Zim install {{{
#
# User configuration sourced by interactive shells
#

# -----------------
# Zsh configuration
# -----------------

#
# History
#

# Remove older command from the history if a duplicate is to be added.
setopt HIST_IGNORE_ALL_DUPS
HISTFILE=~/.config/zsh/.zhistory
HISTSIZE=10000
SAVEHIST=10000

#
# Input/output
#

# Set editor default keymap to emacs (`-e`) or vi (`-v`)
bindkey -v

# Prompt for spelling correction of commands.
setopt CORRECT

# Customize spelling correction prompt.
# SPROMPT='zsh: correct %F{red}%R%f to %F{green}%r%f [nyae]? '

# Remove path separator from WORDCHARS.
WORDCHARS=${WORDCHARS//[\/]}


# --------------------
# Module configuration
# --------------------

#
# completion
#

# Set a custom path for the completion dump file.
# If none is provided, the default ${ZDOTDIR:-${HOME}}/.zcompdump is used.
zstyle ':zim:completion' dumpfile "${ZDOTDIR:-${HOME}}/.zcompdump-${ZSH_VERSION}"

#
# git
#

# Set a custom prefix for the generated aliases. The default prefix is 'G'.
# zstyle ':zim:git' aliases-prefix 'g'
# setopt nopromptbang prompt{cr,percent,sp,subst}

# zstyle ':zim:git-info:branch' format 'branch:%b'
# zstyle ':zim:git-info:commit' format 'commit:%c'
# zstyle ':zim:git-info:remote' format 'remote:%R'

# zstyle ':zim:git-info:keys' format \
#     'prompt'  'git(%b%c)' \
#     'rprompt' '[%R]'

# autoload -Uz add-zsh-hook && add-zsh-hook precmd git-info

# PS1='${(e)git_info[prompt]}%# '
# RPS1='${(e)git_info[rprompt]}'

#
# input
#

# Append `../` to your input for each `.` you type after an initial `..`
zstyle ':zim:input' double-dot-expand yes

#
# termtitle
#

# Set a custom terminal title format using prompt expansion escape sequences.
# See http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html#Simple-Prompt-Escapes
# If none is provided, the default '%n@%m: %~' is used.
# zstyle ':zim:termtitle' format '%1~'

#
# zsh-autosuggestions
#

# Customize the style that the suggestions are shown with.
# See https://github.com/zsh-users/zsh-autosuggestions/blob/master/README.md#suggestion-highlight-style
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=4,bold,underline"'

#
# zsh-syntax-highlighting
#

# Set what highlighters will be used.
# See https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/docs/highlighters.md
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)

# Customize the main highlighter styles.
# See https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/docs/highlighters/main.md#how-to-tweak-it
typeset -A ZSH_HIGHLIGHT_STYLES
 ZSH_HIGHLIGHT_STYLES[comment]='fg=14'

# ------------------
# Initialize modules
# ------------------

if [[ ${ZIM_HOME}/init.zsh -ot ${ZDOTDIR:-${HOME}}/.zimrc ]]; then
  # Update static initialization script if it's outdated, before sourcing it
  source ${ZIM_HOME}/zimfw.zsh init -q
fi
source ${ZIM_HOME}/init.zsh

# SORIN THEME
_prompt_sorin_vimode() {
  case ${KEYMAP} in
    vicmd) print -n ' %B%F{2}❮%F{3}❮%F{1}❮%b' ;;
    *) print -n ' %B%F{1}❯%F{3}❯%F{2}❯%b' ;;
  esac
}

zle-keymap-select() {
  zle reset-prompt
  zle -R
}
zle -N zle-keymap-select

VIRTUAL_ENV_DISABLE_PROMPT=1

setopt nopromptbang prompt{cr,percent,sp,subst}

typeset -gA git_info
if (( ${+functions[git-info]} )); then
  # Set git-info parameters.
  zstyle ':zim:git-info' verbose yes
  zstyle ':zim:git-info:action' format '%F{7}:%F{9}%s'
  zstyle ':zim:git-info:ahead' format ' %F{13}⬆'
  zstyle ':zim:git-info:behind' format ' %F{13}⬇'
  zstyle ':zim:git-info:branch' format ' %F{2}%b'
  zstyle ':zim:git-info:commit' format ' %F{3}%c'
  zstyle ':zim:git-info:indexed' format ' %F{2}✚'
  zstyle ':zim:git-info:unindexed' format ' %F{4}✱'
  zstyle ':zim:git-info:position' format ' %F{13}%p'
  zstyle ':zim:git-info:stashed' format ' %F{6}✭'
  zstyle ':zim:git-info:untracked' format ' %F{7}◼'
  zstyle ':zim:git-info:keys' format \
    'status' '%%B$(coalesce "%b" "%p" "%c")%s%A%B%S%i%I%u%f%%b'

  # Add hook for calling git-info before each command.
  autoload -Uz add-zsh-hook && add-zsh-hook precmd git-info
fi

# Define prompts.
PS1='%{$fg_bold[blue]%}%* %{$reset_color%}${SSH_TTY:+"%F{9}%n%F{7}@%F{3}%m "}%B%F{6}%~%b%(!. %B%F{1}#%b.)$(_prompt_sorin_vimode)%f '
RPS1='${VIRTUAL_ENV:+"%F{3}(${VIRTUAL_ENV:t})"}%(?:: %F{1}✘ %?)${VIM:+" %B%F{6}V%b"}${(e)git_info[status]}%f'
SPROMPT='Correct %F{1}%R%f to %F{2}%r%f [nyae]? '

# 16 Terminal Colors
# -- ---------------
#  0 black
#  1 red
#  2 green
#  3 yellow
#  4 blue
#  5 magenta
#  6 cyan
#  7 white
#  8 bright black
#  9 bright red
# 10 bright green
# 11 bright yellow
# 12 bright blue
# 13 bright magenta
# 14 bright cyan
# 15 bright white
#

# ------------------------------
# Post-init module configuration
# ------------------------------

#
# zsh-history-substring-search
#

# Bind ^[[A/^[[B manually so up/down works both before and after zle-line-init
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# Bind up and down keys
zmodload -F zsh/terminfo +p:terminfo
if [[ -n ${terminfo[kcuu1]} && -n ${terminfo[kcud1]} ]]; then
  bindkey ${terminfo[kcuu1]} history-substring-search-up
  bindkey ${terminfo[kcud1]} history-substring-search-down
fi

bindkey '^P' history-substring-search-up
bindkey '^N' history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down
# }}} End configuration added by Zim install

# Luke's config for the Zoomer Shell

autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

[[ -n "${key[Up]}"   ]] && bindkey -- "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search

# ZSH_THEME=""

autoload -U colors && colors
# PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "

# Load aliases and shortcuts if existent.
[ -f "$HOME/.config/shortcutrc" ] && source "$HOME/.config/shortcutrc"
[ -f "$HOME/.config/aliasrc" ] && source "$HOME/.config/aliasrc"

autoload -U compinit
# zstyle ':completion:*' menu select
zmodload zsh/complist
compinit

# Include hidden files in autocomplete:
_comp_options+=(globdots)

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

export KEYTIMEOUT=1

# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'

  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select

zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init

# Use beam shape cursor on startup.
echo -ne '\e[5 q'
# Use beam shape cursor for each new prompt.
preexec() { echo -ne '\e[5 q' ;}

# Use lf to switch directories and bind it to ctrl-o
lfcd () {
    tmp="$(mktemp)"
   lf -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp"
        if [ -d "$dir" ]; then
            if [ "$dir" != "$(pwd)" ]; then
                cd "$dir"
            fi
        fi
    fi
}

# ENABLE_CORRECTION="true"
setopt autocd

bindkey -s '^o' 'lfcd\n'  # zsh

# Load zsh-syntax-highlighting; should be last.
# source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
