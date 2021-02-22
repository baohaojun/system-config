
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
autoload -U compinit && compinit
zstyle ':completion:*' menu select
if [ -e /home/bhj/.nix-profile/etc/profile.d/nix.sh ]; then . /home/bhj/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
