# [[file:oh-my-fish.org::*Setup Install Script][Setup Install Script:1]]
#!/usr/bin/env bash
# Setup Install Script:1 ends here

# [[file:oh-my-fish.org::*Install Fish][Install Fish:1]]
brew install fish
# Install Fish:1 ends here

# [[file:oh-my-fish.org::*Change Default Shell][Change Default Shell:1]]
which fish | sudo tee -a /etc/shells
chsh -s `which fish`
# Change Default Shell:1 ends here

