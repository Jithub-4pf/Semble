echo "Installing Semblelang!"
git clone https://github.com/Jithub-4pf/Semble.git
mkdir ~/.semble
cp -rf Semble ~/.semble
rm -rf Semble
g++ -o semble ~~/.semble/Semble/main.cpp
sudo chmod 775 ~/.semble/Semble/semble
export PATH=~/.semble/Semble:$PATH
echo "done!"