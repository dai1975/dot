#!/bin/sh

SCRIPT_DIR=$(cd $(dirname $0) && pwd)

if [ -z $1 ]; then
  echo "$0 <dir>"
fi
dir=$1

set -ex
. $SCRIPT_DIR/vscode.shsrc 

VSCODE_SETTING_DIR=$(get_vscode_dir)

mv "$VSCODE_SETTING_DIR/settings.json" "$VSCODE_SETTING_DIR/settings.json.bak"
ln -s "${SCRIPT_DIR}/data/settings.json" "${VSCODE_SETTING_DIR}/settings.json"

mv "$VSCODE_SETTING_DIR/keybindings.json" "$VSCODE_SETTING_DIR/keybindings.json.bak"
ln -s "$SCRIPT_DIR/data/keybindings.json" "${VSCODE_SETTING_DIR}/keybindings.json"

cat "$SCRIPT_DIR/data/extensions" | while read line; do
  code --install-extension $line
done
