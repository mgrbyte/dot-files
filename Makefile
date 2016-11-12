DIRS ?= $(shell git ls-tree --name-only -d HEAD)
LEIN_BINARY := ${HOME}/.local/bin/lein
TARGET := ${HOME}
STOW := @stow --verbose=1 -t ${TARGET}

define print-help
	$(if $(need-help),$(warning $1 -- $2))
endef

need-help := $(filter help,$(MAKECMDGOALS))

help: ; @echo $(if $(need-help),,Type \'$(MAKE)$(dash-f) help\' to get help)

.PHONY: install-lein
install-lein: $(call print-help,install-lein,installs leinigen)
	@curl -L ${LEIN_URL} -o ${LEIN_BINARY}
	@chmod u+x ${LEIN_BINARY}

get-pip.py: $(call print-help,get-pip.py,downloads pip for python 2 and 3)
	@wget https://bootstrap.pypa.io/get-pip.py
	@python2 get-pip.py --user --upgrade 2> /dev/null
	@python3 get-pip.py --user --upgrade 2> /dev/null

venvwrapper: get-pip.py $(call print-help,venvwrapper,installs various python 2 and 3 support packages)
	@python2 -m pip install --user --upgrade pip setuptools gitpython
	@python3 -m pip install --user --upgrade pip setuptools gitpython

install: $(call print-help,install,creates symlinks to setup dot-files)
	${STOW} ${DIRS}


reinstall: $(call print-help,re-install,re-creates symlinks to setup dot-files)
	${STOW} -R ${DIRS}

uninstall: $(call print-help,uninstall,unlinks dot-files)
	${STOW} -D ${DIRS}

clean: $(call print-help,clean,removes build artifacts)
	@rm -f get-pip.py

install-lein: $(call print-help,install-lein,installs leinigen)
	curl -L https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > ${LEIN_BINARY}
	chmod u+x ${LEIN_BINARY}

.PHONY: clean install uninstall install uninstall reinstall
