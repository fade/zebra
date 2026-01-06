FROM shinmera/cl-all
RUN cl-all sbcl -pil "${HOME}/.quicklisp/setup.lisp" \
              -e "(ql:update-all-dists :prompt NIL)" \
              -e "(ql:quickload :parachute)" \
    && cl-all -xpil "${HOME}/.quicklisp/setup.lisp" \
              -e "(ql:quickload :parachute)"
COPY .woodpecker/plugin.sh /usr/local/bin/plugin.sh
RUN sudo chmod +x /usr/local/bin/plugin.sh
ENTRYPOINT ["/usr/local/bin/plugin.sh"]
