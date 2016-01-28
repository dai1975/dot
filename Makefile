TARGETS=emacs fluxbox screen zsh

SED_E='s|sDOTDIR|$(HOME)/dot|g;s|sHOMEDIR|$(HOME)|g'

all: $(TARGETS)
	

install:
	@for f in $(TARGETS); do $(MAKE) $$f; done

clean:
	cd $(HOME)/dot && find . -name "*~" | xargs rm

emacs:
	@sed -e $(SED_E) .emacs > $(HOME)/.emacs
	@echo update $(HOME)/.emacs

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

zsh:
	@sed -e $(SED_E) .zshrc > $(HOME)/.zshrc
	@echo update $(HOME)/.zshrc

.PHONY: all install clean emacs fluxbox global screen zsh
