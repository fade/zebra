#!/bin/bash
IFS=', ' read -r -a systems <<< "${PLUGIN_SYSTEMS:-${CI_REPO_NAME:-$@}}"
IFS=', ' read -r -a tests <<< "${PLUGIN_TESTS:-${CI_REPO_NAME:-$@}}"
readonly IMPLEMENTATION="${PLUGIN_IMPLEMENTATION:-sbcl}"

exec /usr/local/bin/cl-all "$IMPLEMENTATION" -ni \
     -l "${HOME}/.quicklisp/setup.lisp" \
     -e "(asdf:initialize-source-registry (list :source-registry :ignore-inherited-configuration (list :tree (uiop:parse-native-namestring \"$(pwd)/\"))))" \
     -e "(ql:quickload '(parachute ${systems[@]}))" \
     -e "(parachute:test-toplevel '(${tests[@]}))"
