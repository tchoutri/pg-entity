#!/usr/bin/env bash

set -euo pipefail

CI_OS=$(uname -s)

install_deps_linux() {
  echo "Setting up the environment for linux"
  sudo apt-get update
  sudo apt -y upgrade
  sudo apt install -y postgresql-12 libpq-dev 
}

install_deps_darwin() {
  echo "Setting up the environment for macOS"
  brew update
  brew upgrade
  brew install postgresql@12
}

case $CI_OS in
  Linux) install_deps_linux;;
  Darwin) install_deps_darwin;;
esac

