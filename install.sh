#!/usr/bin/env bash

trim () {
  echo $1 | cut -c1 | tr '[:upper:]' '[:lower:]'
}

printf "\e[96m->\e[0m Creating symlinks ...\n"

# Create our dotfile symlinks
ln -s ~/.vim ./vim
ln -s ~/.vimrc ./vimrc
ln -s ~/.zshrc ./zshrc
ln -s ~/.oh-my-zsh/themes/jay.zsh-theme ./oh-my-zsh/themes/jay.zsh-theme

# Create backup of git zsh file
mv ~/.oh-my-zsh/lib/git.zsh ~/.oh-my-zsh/lib/git.zsh.bak

# Create symlink to modified git plugin
ln -s ~/.oh-my-zsh/lib/git.zsh ./oh-my-zsh/git/git.zsh

printf "\e[96m->\e[0m \e[92mDone.\e[0m\n"

# Find out if this is OSX or not
printf "\e[93mInstall settings for OSX?\e[0m"
read -e -p "(y/n [n]) > " answer

# Set the answer
if [ trim($answer) == "y" ]; then 
  os="osx"
else
  os="osx"
fi

printf "\e[96m->\e[0m Generating tmux config...\n"
# Generate the proper config for the OSX
cat > ~/.tmux.conf << EOF
  source-file ~/dotfiles/tmux.d/base.conf
  source-file ~/dotfiles/tmux.d/$os.conf
EOF

printf "\e[96m->\e[0m \e[92mDone.\e[0m\n"

printf "\e[96m->\e[0m Pulling latest submodules ...\n"
# Pull latest submodules
cd vim/bundle && git submodule foreach git pull origin master

printf "\e[96m->\e[0m \e[92mDone.\e[0m\n"

printf "\e[92mAll good here whoo!\e[0m\n"