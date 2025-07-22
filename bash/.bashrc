#
# ~/.bashrc
#

# Ssh-agent
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi
if [[ ! -f "$SSH_AUTH_SOCK" ]]; then
    source "$XDG_RUNTIME_DIR/ssh-agent.env" >/dev/null
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias cpc='xclip -sel c <'
PS1='[\u@\h \W]\$ '

export PATH="/home/$USER/.config/emacs/bin:/home/$USER/bin:$PATH"

eval "$(starship init bash)"
