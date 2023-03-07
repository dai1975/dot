#!/bin/bash -e

mkdir /tmp/skksetup$$; cd /tmp/skksetup$$

git clone https://github.com/wachikun/yaskkserv2
git clone https://github.com/skk-dev/dict
git clone https://github.com/tokuhirom/skk-jisyo-neologd.git

cd yaskkserv2
cargo install --path .
cd ..

$HOME/.cargo/bin/yaskkserv2_make_dictionary --utf8 --dictionary-filename=$HOME/myskkdic ./dict/SKK-JISYO.* ./skk-jisyo-neologd/SKK-JISYO.neologd
