DIRS ?= $(shell git ls-tree --name-only -d HEAD)
TARGET := ${HOME}
STOW := @stow --verbose=1 -t ${TARGET}

define print-help
	$(if $(need-help),$(warning $1 -- $2))
endef

need-help := $(filter help,$(MAKECMDGOALS))

help: ; @echo $(if $(need-help),,Type \'$(MAKE)$(dash-f) help\' to get help)

install: $(call print-help,install,creates symlinks to setup dot-files)
	${STOW} ${DIRS}

reinstall: $(call print-help,reinstall,re-creates symlinks to setup dot-files)
	${STOW} -R ${DIRS}

uninstall: $(call print-help,uninstall,unlinks dot-files)
	${STOW} -D ${DIRS}

.PHONY: install uninstall install uninstall reinstall
