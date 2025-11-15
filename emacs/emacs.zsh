#!/usr/bin/env zsh
[[ "$INSIDE_EMACS" != vterm ]] && return

# OSC 2
osc_title() {
    print -Pn "\e]2;${1}\a"
}

# OSC 51;E
osc_vterm_eval() {
    printf "\e]51;E"
    local arg
    for arg in "$@"; do
        printf '"%s" ' "${arg//\\/\\\\//\"/\\\"}"
    done
    printf "\e\\"
}

# OSC 51;A - vterm prompt annotation
osc_vterm_annotate() {
    printf "\e]51;A%s\e\\" "${1:-}"
}

# OSC 52
osc_copy() {
    local text="${1:-$(</dev/stdin)}"
    if (( $+commands[base64] )); then
        local encoded="$(print -n "$text" | base64 | tr -d '\n')"
        printf "\e]52;c;%s\a" "$encoded"
    fi
}

clear() {
    osc_vterm_eval vterm-clear-scrollback
}

if (( $+commands[evt] )); then
    find_file() { evt open "$@" }
    find_file_other_window() { evt open --other-window "$@" }
    export EDITOR='evt open'
    export VISUAL='evt open'
else
    find_file() {
        local file="${1:-.}"
        osc_vterm_eval find-file "${file:a}"
    }
    find_file_other_window() {
        local file="${1:-.}"
        osc_vterm_eval find-file-other-window "${file:a}"
    }
fi

magit() {
    osc_vterm_eval magit-status "${${1:-.}:a}"
}

compile() {
    local cmd="${1:-make}"
    osc_vterm_eval compile "$cmd"
}

recompile() {
    osc_vterm_eval recompile
}

_emacs_vterm_precmd() {
    osc_vterm_eval update-pwd "$PWD"
    osc_title "%n@%m:%~"
    osc_vterm_annotate "$(print -Pn '%n@%m:%~')"
}

_emacs_vterm_preexec() {
    local cmd="${1[(wr)^(*=*|sudo|ssh|-*)]:gs/%/%%}"
    osc_title "$cmd"
}

_emacs_vterm_chpwd() {
    osc_vterm_eval update-pwd "$PWD"
}

autoload -U add-zsh-hook
add-zsh-hook precmd _emacs_vterm_precmd
add-zsh-hook preexec _emacs_vterm_preexec
add-zsh-hook chpwd _emacs_vterm_chpwd

# Don't override C-l - let vterm handle it directly
# bindkey -s '^L' 'clear\n'
