#!/usr/bin/env bash

VERSION=$(sed 's/\..*//' /etc/debian_version)

# debian packages
export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get -y install software-properties-common

# FPC 3.0 is in debian SID
add-apt-repository -y "deb http://ftp.de.debian.org/debian/ unstable main contrib non-free"
add-apt-repository -y "deb-src ttp://ftp.de.debian.org/debian/  unstable main contrib non-free"
# deb     http://security.debian.org/         unstable/updates  main contrib non-free

apt-get update
apt-get upgrade -y

apt-get install -y fpc apache2 lynx

ln -s /vagrant_project/ /home/vagrant/DeuSu 2>/dev/null

cd /home/vagrant/DeuSu
./build-linux.sh
./makedirs.sh

# create an empty URL-database
./bin/ImportUrls

#cp domainrank.txt data/txt/urls.txt
