# Created by newuser for 4.3.11

is_emacs(){
    [[ $EMACS != "" ]] && return 0
    return 1
}
is_macos(){
    test "$(uname)" == "Darwin"
    return $?
}
is_wsl2(){
     grep -e 'microsoft-standard-WSL2' <(uname -r) >/dev/null
     return $?
}

DOTDIR=$(dirname $0)

export LC_ALL=en_US.UTF_8

# make podman docker-compose runnable by user
DOCKER_HOST=unix:$XDG_RUNTIME_DIR/podman/podman.sock; export DOCKER_HOST

#GOROOT=/usr/local/go; export GOROOT
GOPATH=$HOME/local/go; export GOPATH
if [ ! -d $GOPATH ]; then
	mkdir -p $GOPATH
fi

# rye
#if [ -r $HOME/.rye/env ]; then
#  . $HOME/.rye/env
#  alias python="rye run python"
#else
#  echo "warn: rye is not found: install rye as:"
#  echo "  $ curl -sSf https://rye-up.com/get | bash"
#fi

# at first load asdf
if [ -r $HOME/.asdf/asdf.sh ]; then
  . $HOME/.asdf/asdf.sh
else
  echo "warn: asdf is not found: install asdf as:"
  echo "  $ git clone https://github.com/asdf-vm/asdf.git ~/.asdf"
  echo "  $ cd ~/.asdf; git checkout \"$\(git describe -abbrev=0 --tags\)\""
fi

# -- set path --------------------------
# PATH0, PATH1 is prior to PATH(and asdf): PATH=$PATH0:$PATH1:$PATH:$PATH100
# -- PATH0 -----------------------------

PATH0="$PATH0:${AQUA_ROOT_DIR:-${XDG_DATA_HOME:-$HOME/.local/share}/aquaproj-aqua}/bin"
export AQUA_GLOBAL_CONFIG=${XDG_CONFIG_HOME:-$HOME/.config}/aquaproj-aqua/aqua.yaml

PATH0=$PATH0:$HOME/.cargo/bin
PATH0=$PATH0:$GOPATH/bin
PATH0=$PATH0:$HOME/.krew/bin #kubectl krew
PATH0=$PATH0:$HOME/.pulumi/bin

# -- PATH1 -----------------------------
PATH1=$PATH1:$DOTDIR/bin:$HOME/bin:$HOME/local/bin:$HOME/.local/bin

# -- PATH99 -----------------------------
PATH99=/snap/bin #snap

# -- PATH -----------------------------
export PATH=$PATH0:$PATH1:$PATH:$PATH99
#eval "$(anyenv init -)"

# -- end of PATH -----------------------------

# keyring
if [ -n "$DESKTOP_SESSION" ]; then
  eval $(gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
  export SSH_AUTH_SOCK
fi


#if [ "x$TERM" = "xemacs" ]; then
  #TERM=dumb
  #TERM=tmux-256color
  #TERM=screen-256color
  #export TERM
#fi

set -o ignoreeof
# -- copy from @sirrow --------------------------------------------------------
# Created by newuser for 4.3.17


# ------------------------------
# General Settings
# ------------------------------
export EDITOR=vi	 # エディタをviに設定
export LANG=ja_JP.UTF-8  # 文字コードをUTF-8に設定
export KCODE=u           # KCODEにUTF-8を設定
export AUTOFEATURE=true  # autotestでfeatureを動かす

bindkey -e               # キーバインドをemacsモードに設定
#bindkey -v              # キーバインドをviモードに設定

setopt no_beep           # ビープ音を鳴らさないようにする
setopt auto_cd           # ディレクトリ名の入力のみで移動する 
#setopt auto_pushd        # cd時にディレクトリスタックにpushdする
setopt correct           # コマンドのスペルを訂正する
setopt magic_equal_subst # =以降も補完する(--prefix=/usrなど)
setopt prompt_subst      # プロンプト定義内で変数置換やコマンド置換を扱う
setopt notify            # バックグラウンドジョブの状態変化を即時報告する
setopt equals            # =commandを`which command`と同じ処理にする

### Complement ###
autoload -U compinit; compinit # 補完機能を有効にする
setopt auto_list               # 補完候補を一覧で表示する(d)
setopt auto_menu               # 補完キー連打で補完候補を順に表示する(d)
setopt list_packed             # 補完候補をできるだけ詰めて表示する
setopt list_types              # 補完候補にファイルの種類も表示する
bindkey "^[[Z" reverse-menu-complete  # Shift-Tabで補完候補を逆順する("\e[Z"でも動作する)
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' # 補完時に大文字小文字を区別しない

### Glob ###
setopt extended_glob # グロブ機能を拡張する
unsetopt caseglob    # ファイルグロブで大文字小文字を区別しない

### History ###
HISTFILE=~/.zsh_history   # ヒストリを保存するファイル
HISTSIZE=10000            # メモリに保存されるヒストリの件数
SAVEHIST=10000            # 保存されるヒストリの件数
setopt bang_hist          # !を使ったヒストリ展開を行う(d)
setopt extended_history   # ヒストリに実行時間も保存する
#setopt hist_ignore_dups   # 直前と同じコマンドはヒストリに追加しない
#setopt share_history      # 他のシェルのヒストリをリアルタイムで共有する
setopt hist_reduce_blanks # 余分なスペースを削除してヒストリに保存する

# マッチしたコマンドのヒストリを表示できるようにする
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# すべてのヒストリを表示する
function history-all { history -E 1 }


# ------------------------------
# Look And Feel Settings
# ------------------------------
### Ls Color ###
# 色の設定
export LSCOLORS=Exfxcxdxbxegedabagacad
# 補完時の色の設定
export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
# ZLS_COLORSとは？
export ZLS_COLORS=$LS_COLORS
# lsコマンド時、自動で色がつく(ls -Gのようなもの？)
export CLICOLOR=true
# 補完候補に色を付ける
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# git ブランチ名表示
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '[%b]'
zstyle ':vcs_info:*' actionformats '[%b|%a]'

autoload -Uz is-at-least
if is-at-least 4.3.10; then
  # この check-for-changes が今回の設定するところ
  zstyle ':vcs_info:git:*' check-for-changes true
  zstyle ':vcs_info:git:*' stagedstr "+"    # 適当な文字列に変更する
  zstyle ':vcs_info:git:*' unstagedstr "-"  # 適当の文字列に変更する
  zstyle ':vcs_info:git:*' formats '[%b]%c%u'
  zstyle ':vcs_info:git:*' actionformats '[%b|%a]%c%u'
fi

### Prompt ###
# プロンプトに色を付ける
autoload -U colors; colors
# 一般ユーザ時
#tmp_prompt="%{${fg[cyan]}%}%n%{$fg[yellow]}%}@${HOST%%.*} ${fg[green]}%}%1~%{${reset_color} %# %}"
tmp_prompt="
[%{${fg[cyan]}%}%n%{$fg[yellow]%}@${HOST%%.*}:%{${fg[green]}%}%v]
%{$fg[green]%}%2~%{${reset_color}%}%# "
tmp_prompt2="%{${fg[cyan]}%}%_> %{${reset_color}%}"
tmp_rprompt="%{${fg[green]}%}%~%{${fg[magenta]}%}%v%{${reset_color}%}"
tmp_sprompt="%{${fg[yellow]}%}%r is correct? [Yes, No, Abort, Edit]:%{${reset_color}%}"

# rootユーザ時(太字にし、アンダーバーをつける)
if [ ${UID} -eq 0 ]; then
  tmp_prompt="%B%U${tmp_prompt}%u%b"
  tmp_prompt2="%B%U${tmp_prompt2}%u%b"
  tmp_rprompt="%B%U${tmp_rprompt}%u%b"
  tmp_sprompt="%B%U${tmp_sprompt}%u%b"
fi


PROMPT=$tmp_prompt    # 通常のプロンプト
PROMPT2=$tmp_prompt2  # セカンダリのプロンプト(コマンドが2行以上の時に表示される)
RPROMPT=$tmp_rprompt  # 右側のプロンプト
SPROMPT=$tmp_sprompt  # スペル訂正用プロンプト


### Title (user@hostname) ###
precmd() {
    case "x${TERM}" in
	xkterm*|xxterm*)
	    echo -ne "\033]0;${USER}@${HOST%%.*}\007"
    esac
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"

}


### Aliases ###
alias emacs='emacs -nw'
alias ls='ls --color'
alias k='kubectl'


alias ecr-login='`aws ecr get-login --no-include-email`'


function update-awscli-mfa() {
  if [ "x$1" = "x" ]; then
    echo "update-awscli-mfa <mfa code>"
    return 1
  fi

  device=$(sed -n '/^\[mfa/,/^\[/p'  ~/.aws/credentials.tmpl|awk '$1=="aws_mfa_device"{print $3}')
  if [ "x$device" = "x" ]; then
    echo "no mfa profile or aws_mfa_device found in ~/.aws/credentials.tmpl"
    return 2
  fi

  cp ~/.aws/credentials.tmpl ~/.aws/credentials
  aws sts get-session-token --profile mfa --serial-number $device --token-code $1 | awk 'BEGIN { print "[default]" } $1 == "\"AccessKeyId\":" { gsub(/[\",]/,""); print "aws_access_key_id = "$2 } $1 == "\"SecretAccessKey\":" { gsub(/[",]/,""); print "aws_secret_access_key = "$2 } $1 == "\"SessionToken\":" { gsub(/[",]/,""); print "aws_session_token = "$2 } ' >> ~/.aws/credentials
  aws configure list
}

## -- X Window -------------------------------------------------------------
if [ is_wsl2 ]; then
   export DISPLAY=$(grep /etc/resolv.conf -e nameserver | awk '{print $2}'):0
fi

test -f $HOME/.zshrc.local && source $HOME/.zshrc.local
