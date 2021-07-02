TARGETS=emacs stumpwm zsh tmux xdefaults xsession image

SED_E='s|sDOTDIR|$(HOME)/dot|g;s|sHOMEDIR|$(HOME)|g'

help: 
	@echo "$(MAKE) install: install dot files."

install: $(TARGETS)

clean:
	cd $(HOME)/dot && find . -name "*~" | xargs rm

dot-config:
	@cd dot-config; for f in $$(find . -type f -print); do \
	  if [ -f $$HOME/.config/$$f ]; then \
	    echo "skip $$f"; \
	  else \
	    mkdir -p $$HOME/.config/$$(dirname $$f); \
	    cp -i $$f $$HOME/.config/$$(dirname $$f); \
	  fi \
	done

emacs:
	@sed -e $(SED_E) .emacs > $(HOME)/.emacs
	@echo update $(HOME)/.emacs

xdefaults:
	@sed -e $(SED_E) .Xdefaults > $(HOME)/.Xdefaults
	@echo update $(HOME)/.Xdefaults

xsession:
	@sed -e $(SED_E) .xsession > $(HOME)/.xsession
	@echo update $(HOME)/.xsession

stumpwm:
	@sed -e $(SED_E) .stumpwmrc > $(HOME)/.stumpwmrc
	@echo update $(HOME)/.stumpwmrc

fluxbox:
	@mkdir -p $(HOME)/.fluxbox
	@for f in startup init lastwallpaper; do \
          sed -e $(SED_E) .fluxbox-$$f > $(HOME)/.fluxbox/$$f; \
        done
	@if [ ! -f $(HOME)/.wallpaper ]; then cp .fluxbox-wallpaper $(HOME)/.wallpaper; fi
	@echo update $(HOME)/.fluxbox

global:
	@sed -e $(SED_E) .global > $(HOME)/.global
	@echo update $(HOME)/.global

screen:
	@sed -e $(SED_E) .screenrc > $(HOME)/.screenrc
	@touch $(HOME)/.screenrc-local
	@echo update $(HOME)/.screenrc

tmux:
	@sed -e $(SED_E) tmpl/tmux.conf > $(HOME)/.tmux.conf
	@touch $(HOME)/.tmux.local.conf
	@echo update $(HOME)/.tmux.conf

zsh:
	@sed -e $(SED_E) .zshrc > $(HOME)/.zshrc
	@echo update $(HOME)/.zshrc

image:
	mkdir $(HOME)/background-images

.PHONY: all install clean emacs fluxbox global screen zsh dot-config
