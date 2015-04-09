DIRS=$(shell find . -maxdepth 1 -type d -exec basename {} \; | grep -vE '^(\.)')

install: clean venvwrapper stow
update: clean restow
uninstall: clean unstow

venvwrapper:
	@wget https://bootstrap.pypa.io/get-pip.py
	@python3 get-pip.py --user --upgrade 2> /dev/null
	@python3 -m pip install --user virtualenvwrapper gitpython
	@python2 get-pip.py --user --upgrade 2> /dev/null
	@python2 -m pip install --user virtualenvwrapper gitpython

stow:
	@stow ${DIRS}

restow:
	@stow -R ${DIRS}

unstow:
	@stow -D ${DIRS}

clean:
	@rm -f get-pip.py

.PHONY: cleanup install update uninstall stow unstow restow
