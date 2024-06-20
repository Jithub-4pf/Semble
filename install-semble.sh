echo "Installing Semblelang!"
git clone https://github.com/Jithub-4pf/Semble.git
mkdir ~/.semble
cp -rf Semble ~/.semble
sudo rm -rf Semble
g++ -o ~/.semble/Semble/semble ~/.semble/Semble/main.cpp
echo "done compiling"
sudo chmod 775 ~/.semble/Semble/semble
sudo export PATH=~/.semble/Semble:$PATH
sudo mv ~/.semble/Semble/semble /usr/bin/semble
echo "done!"