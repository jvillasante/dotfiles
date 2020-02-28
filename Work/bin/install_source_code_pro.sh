#!/usr/bin/env bash

cd Downloads

wget https://github.com/adobe-fonts/source-code-pro/archive/2.030R-ro/1.050R-it.zip

if [ ! -d "~/.fonts" ] ; then
    mkdir ~/.fonts
fi

unzip 1.050R-it.zip 
cp source-code-pro-*-it/OTF/*.otf ~/.fonts/
rm -rf source-code-pro* 
rm 1.050R-it.zip 

cd ~/
fc-cache -f -v