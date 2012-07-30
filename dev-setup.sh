#!/bin/bash

# Bootstraps a new Nextdoor dev vmware instance.

set -o errexit
set -o xtrace

# Add installations here
sudo apt-get install \
git \
keychain \
tkcvs \
colortest-python \
screen \
tmux \
byobu \
git-el \
emacs23-el \
emacs-goodies-el \
gnome-panel \
libdigest-hmac-perl \
yasnippet \
pymacs \
python-ropemacs \
xclip \
;

# Remove the annoying mail icon from the menu bar.
echo "sudo dpkg -r indicator-status-provider-mc5 indicator-messages indicator-session indicator-sound"

# Remove annoying bluetooth icon from the menu bar.
mkdir -p ~/.config/autostart
cp /etc/xdg/autostart/bluetooth-applet.desktop ~/.config/autostart
perl -i -pe 's/^(OnlyShowIn=).*/\1KDE;/' ~/.config/autostart/bluetooth-applet.desktop

# Todo (Kip): Change sleep settings here
# Todo (kip): Change background (shell profile) here

# Copy dot files from git repo
sudo cp -a ~/src/settings/dot-files ~/

# Copy Documents
sudo cp -a ~/src/settings/Documents ~/

# Copy Pictures
sudo cp -a ~/src/settings/Pictures ~/

# Copy emacs fiels (or do this with dot-files)
sudo cp -a ~/src/settings/emacs ~/


# Copy ssh keys and config.
#test -d /mnt/hgfs/jhesch/Downloads/.ssh
#if [[ ! -d ~/.ssh ]]; then
#  cp -a /mnt/hgfs/jhesch/Downloads/.ssh ~/
#fi

# Comment out below
# Install hostconfig repo and configure host.
#if [[ ! -d ~/src/hostconfig ]]; then
#  mkdir -p ~/src
#  pushd ~/src
#  git clone alamo:git/hostconfig.git
#  cd hostconfig
#  nextdoor/install
##  common/gconf-settings
#  popd
#fi

# Install git-commit emacs mode.
if [[ ! -d /usr/local/src/git-commit-mode ]]; then
  sudo mkdir -p /usr/local/src
  sudo chown kkaehler /usr/local/src
  pushd /usr/local/src
  git clone https://github.com/rafl/git-commit-mode.git
  ln -s /usr/local/src/git-commit-mode/git-commit.el ~/.emacs.d/elisp
  popd
fi

# Install chrome
echo "wget https://dl.google.com/linux/direct/google-chrome-stable_current_i386.deb"
echo "sudo dpkg -i google-chrome-stable_current_i386.deb"
echo "   --- OR ----   "
echo "wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb"
echo "sudo dpkg -i google-chrome-stable_current_amd64.deb"
echo
echo "You may need to reinstall vmware tools"
echo "You may need to remove Pymacs from virtualenv:"
echo "$ sudo su -"
echo "# . /opt/nextdoor-ve/bin/activate"
echo "# pip uninstall Pymacs"

echo "sudo service apache2 stop"
echo "sudo update-rc.d apache2 disable"
echo "sudo service puppet stop"
echo "sudo update-rc.d puppet disable"

# See also http://askubuntu.com/questions/37313/how-do-i-deactivate-f1-and-f10-keybindings
echo "fix F10 gnome-terminal issue: https://bugs.launchpad.net/ubuntu/+source/unity/+bug/726639/comments/18"
