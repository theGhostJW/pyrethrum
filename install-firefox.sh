#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Remove the Debian unstable repository from sources list
sed -i '/deb.debian.org\/debian unstable/d' /etc/apt/sources.list

# Update the package list
apt-get update

# Install necessary dependencies
apt-get install -y wget tar libdbus-glib-1-2 libgtk-3-0 libx11-xcb1 libxt6 libxrender1 libxrandr2 libdbus-1-3 libxtst6 libxss1

# Download the latest Firefox tarball
wget -O /tmp/firefox.tar.bz2 "https://download.mozilla.org/?product=firefox-latest&os=linux64&lang=en-US"

# Extract the tarball to /opt
tar -xjf /tmp/firefox.tar.bz2 -C /opt/

# Create a symbolic link to the Firefox binary
ln -s /opt/firefox/firefox /usr/local/bin/firefox

# Clean up
rm /tmp/firefox.tar.bz2

echo "Firefox installation completed."