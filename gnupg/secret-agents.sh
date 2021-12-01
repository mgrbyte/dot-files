export ISWSL=2 # WSL 2

# WSL 2 require socat to create socket on Linux side and sorelay on the Windows side to interop
if [ ! -d ${HOME}/.gnupg ]; then
    mkdir ${HOME}/.gnupg
    chmod 0700 ${HOME}/.gnupg
fi
if [ -n ${WIN_GNUPG_HOME} ]; then
    # setup gpg-agent socket
    _sock_name=${HOME}/.gnupg/S.gpg-agent
    ss -a | grep -q ${_sock_name}
    if [ $? -ne 0  ]; then
        rm -f ${_sock_name}
        ( setsid socat UNIX-LISTEN:${_sock_name},fork EXEC:"${HOME}/winhome/.wsl/sorelay.exe -a ${WIN_GNUPG_HOME//\:/\\:}/S.gpg-agent",nofork & ) >/dev/null 2>&1
    fi
    # setup gpg-agent.extra socket
    _sock_name=${HOME}/.gnupg/S.gpg-agent.extra
    ss -a | grep -q ${_sock_name}
    if [ $? -ne 0  ]; then
        rm -f ${_sock_name}
        ( setsid socat UNIX-LISTEN:${_sock_name},fork EXEC:"${HOME}/winhome/.wsl/sorelay.exe -a ${WIN_GNUPG_HOME//\:/\\:}/S.gpg-agent.extra",nofork & ) >/dev/null 2>&1
    fi
    unset _sock_name
fi
