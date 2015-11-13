#!/usr/bin/env bash

src="$HOME/dotfiles"

target="$HOME"
# target="$src/test"

trim () {
  echo $1 | cut -c1 | tr '[:upper:]' '[:lower:]'
}

printf "\e[92mInstalling dotfiles!\e[0m\n\n"

printf "\e[96m->\e[0m Creating symlinks ...\n"

# Create our dotfile symlinks
ln -s $src/.vim $target/vim
printf "\e[96m->\e[0m Symlinked \e[95mvim \e[93m->\e[0m \e[95m$target/.vim \e[0m \n"
ln -s $src/.vimrc $target/vimrc
printf "\e[96m->\e[0m Symlinked \e[95mvimrc \e[93m->\e[0m \e[95m$target/.vimrc \e[0m \n"
ln -s $src/.zshrc $target/zshrc
printf "\e[96m->\e[0m Symlinked \e[95mzshrc \e[93m->\e[0m \e[95m$target/.zshrc \e[0m \n"
ln -s $src/.oh-my-zsh/themes/jay.zsh-theme $target/oh-my-zsh/themes/jay.zsh-theme
printf "\e[96m->\e[0m Symlinked \e[95mjay.zsh-theme \e[93m->\e[0m \e[95m$target/.oh-my-zsh/themes/jay.zsh-theme \e[0m \n"

# Create backup of git zsh file
mv $src/.oh-my-zsh/lib/git.zsh $target/.oh-my-zsh/lib/git.zsh.bak

# Create symlink to modified git plugin
ln -s $src/.oh-my-zsh/lib/git.zsh $target/oh-my-zsh/lib/git.zsh
printf "\e[96m->\e[0m Symlinked \e[95mgit.zsh \e[93m->\e[0m \e[95m$target/.oh-my-zsh/lib/git.zsh \e[0m \n"

printf "\e[96m->\e[0m \e[92mDone.\e[0m\n\n"

# Find out if this is OSX or not
printf "\e[93mInstall settings for OSX? \e[0m"
read -e -p "(y/n [n]) > " answer

# Set the answer
if [ trim $answer == "y" ]; then 
  os="osx"
else
  os="unix"
fi

printf "\e[96m->\e[0m Generating tmux config for \e[95m$os...\e[0m\n"
# Generate the proper config for the OSX
cat > $target/.tmux.conf << EOF
  source-file $src/tmux.d/base.conf
  ource-file $src/tmux.d/$os.conf
EOF
printf "\e[96m->\e[0m \e[92mDone.\e[0m\n\n"

# Pull latest submodules
printf "\e[96m->\e[0m Cloning vundle submodules ...\n"
mkdir vim/bundle
git clone https://github.com/VundleVim/Vundle.vim.git vim/bundle/Vundle.vim
printf "\e[96m->\e[0m \e[92mDone.\e[0m\n\n"

printf "\e[96m->\e[0m Installing vim plugins ...\n"
vim +PluginInstall +qall
printf "\e[96m->\e[0m \e[92mDone.\e[0m\n\n"

printf "\e[92mAll good here whoo!\e[0m\n"