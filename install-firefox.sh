#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Remove the Debian unstable repository from sources list
sed -i '/deb.debian.org\/debian unstable/d' /etc/apt/sources.list

# Update the package list
apt-get update

# Install necessary dependencies
apt-get install -y wget tar libdbus-glib-1-2 libgtk-3-0t64 libx11-xcb1 libxt6t64 libxrender1 libxrandr2 libdbus-1-3 libxtst6 libxss1 libasound2t64 libpci3 libegl1 libgl1 libglx-mesa0 locales \
                   gstreamer1.0-libav gstreamer1.0-plugins-base gstreamer1.0-plugins-good gstreamer1.0-plugins-bad gstreamer1.0-plugins-ugly pulseaudio pulseaudio-utils alsa-utils

# Set up locale for Australia
locale-gen en_AU.UTF-8
update-locale LANG=en_AU.UTF-8

# Ensure the locale is available
export LANG=en_AU.UTF-8
export LANGUAGE=en_AU:en
export LC_ALL=en_AU.UTF-8

# Start PulseAudio
pulseaudio --start

# Download the latest Firefox tarball
wget -O /tmp/firefox.tar.bz2 "https://download.mozilla.org/?product=firefox-latest&os=linux64&lang=en-US"

# Extract the tarball to /opt
tar -xjf /tmp/firefox.tar.bz2 -C /opt/

# Create a symbolic link to the Firefox binary
ln -s /opt/firefox/firefox /usr/local/bin/firefox

# Clean up
rm /tmp/firefox.tar.bz2

# Download a test audio file
wget -O /tmp/test-audio.wav "https://www2.cs.uic.edu/~i101/SoundFiles/StarWars3.wav"

# Test audio playback
aplay /tmp/test-audio.wav

echo "Firefox installation and audio test completed."

# TODO Audio not working - check if works with playwright emulators and get working if it does