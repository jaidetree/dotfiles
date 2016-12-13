#!/usr/bin/env bash

src="$HOME/dotfiles/client"
target="$HOME"
os="osx"

printf "\e[92mInstalling dotfiles!\e[0m\n\n"

# Install zsh and oh-my-zsh
# printf "\e[96m->\e[0m Installing zsh & oh-my-zsh ...\n"
# brew install zsh
# sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
# sudo gem install tmuxinator

mkdir -p vim/bundle

# Create our dotfile symlinks
printf "\e[96m->\e[0m Creating symlinks ...\n"

rm -f $target/.vimrc
rm -f $target/.zshrc
rm -rf $target/.vim
rm -rf $target/.oh-my-zsh/custom/themes
rm -rf $target/.bin
rm -rf $target/.tmuxinator

ln -sf $src/vim $target/.vim
printf "\e[96m->\e[0m Symlinked \e[95mvim \e[93m->\e[0m \e[95m$target/.vim \e[0m \n"
ln -sf $src/vimrc $target/.vimrc
printf "\e[96m->\e[0m Symlinked \e[95mvimrc \e[93m->\e[0m \e[95m$target/.vimrc \e[0m \n"
ln -sf $src/zshrc $target/.zshrc
printf "\e[96m->\e[0m Symlinked \e[95mzshrc \e[93m->\e[0m \e[95m$target/.zshrc \e[0m \n"
ln -sf $src/zsh/themes $target/.oh-my-zsh/custom/themes
printf "\e[96m->\e[0m Symlinked \e[95mzsh/themes \e[93m->\e[0m \e[95m$target/.oh-my-zsh/custom/themes \e[0m \n"
ln -sf $src/bin $target/.bin
printf "\e[96m->\e[0m Symlinked \e[95mbin \e[93m->\e[0m \e[95m$target/.bin \e[0m \n"
ln -sf $src/tmuxinator $target/.tmuxinator
printf "\e[96m->\e[0m Symlinked \e[95mtmuxinator \e[93m->\e[0m \e[95m$target/.tmuxinator \e[0m \n"

printf "\e[96m->\e[0m \e[92mDone.\e[0m\n\n"

printf "\e[96m->\e[0m Generating tmux config for \e[95m$os...\e[0m\n"
# Generate the proper config for the OSX
cat > $target/.tmux.conf << EOF
  source-file $src/tmux.d/base.conf
  source-file $src/tmux.d/$os.conf
EOF
printf "\e[96m->\e[0m \e[92mDone.\e[0m\n\n"

# Pull latest submodules
printf "\e[96m->\e[0m Cloning vundle submodules ...\n"
git clone https://github.com/VundleVim/Vundle.vim.git vim/bundle/Vundle.vim
printf "\e[96m->\e[0m \e[92mDone.\e[0m\n\n"

printf "\e[96m->\e[0m Installing vim plugins ...\n"
vim +PluginInstall +qall
printf "\e[96m->\e[0m \e[92mDone.\e[0m\n\n"

printf "\e[92mAll good here whoo!\e[0m\n"
