#!/bin/sh

SCRIPT_DIR=$(cd $(dirname $0) && pwd)

if [ -z $1 ]; then
  echo "$0 <dir>"
  exit 1
fi
dir=$1

set -ex
. $SCRIPT_DIR/vscode.shsrc 
mkdir -p $dir || exit

VSCODE_SETTING_DIR=$(get_vscode_dir)

cp "$VSCODE_SETTING_DIR/settings.json" "$dir"
cp "$VSCODE_SETTING_DIR/keybindings.json" "$dir"
code --list-extensions > "$dir/extensions"
