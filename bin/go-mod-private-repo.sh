#!/bin/sh

# maybe set .gitconfig
# [url "git@github.com:"]
#   insteadOf = https://github.com/
# maybe set GOPRIVATE="github.com/...""

echo GIT_SSH_COMMAND='ssh -o ControlMaster=no -o BatchMode=no' go mod tidy
GIT_SSH_COMMAND='ssh -o ControlMaster=no -o BatchMode=no' go mod tidy


