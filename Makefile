DIRS ?= $(shell git ls-tree --name-only -d HEAD)

define print-help
	$(if $(need-help),$(warning $1 -- $2))
endef

need-help := $(filter help,$(MAKECMDGOALS))

help: ; @echo $(if $(need-help),,Type \'$(MAKE)$(dash-f) help\' to get help)
install: clean venvwrapper stow
update: clean restow
uninstall: clean unstow

get-pip.py: $(call print-help,get-pip.py,downloads pip for python 2 and 3)
	@wget https://bootstrap.pypa.io/get-pip.py
	@python2 get-pip.py --user --upgrade 2> /dev/null
	@python3 get-pip.py --user --upgrade 2> /dev/null

venvwrapper: get-pip.py $(call print-help,venvwrapper,installs various python 2 and 3 support packages)
	@python2 -m pip install --user --upgrade pip setuptools gitpython
	@python3 -m pip install --user --upgrade pip setuptools gitpython

install: $(call print-help,install,creates symlinks to setup dot-files)
	@stow ${DIRS}

re-install: $(call print-help,re-install,re-creates symlinks to setup dot-files)
	@stow -R ${DIRS}

uninstall: $(call print-help,uninstall,unlinks dot-files)
	@stow -D ${DIRS}

clean: $(call print-help,clean,removes build artifacts)
	@rm -f get-pip.py

.PHONY: cleanup install update uninstall install uninstall re-install
