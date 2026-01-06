FROM shinmera/cl-all
COPY .woodpecker/plugin.sh /usr/local/bin/plugin.sh
RUN cl-all -pil "${HOME}/.quicklisp/setup.lisp" -e "(ql:quickload :parachute)" \
    && cl-all -xpil "${HOME}/.quicklisp/setup.lisp" -e "(ql:quickload :parachute)" \
    && sudo chmod +x /usr/local/bin/plugin.sh

ENTRYPOINT ["/usr/local/bin/plugin.sh"]
