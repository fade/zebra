(asdf:defsystem zebra-lisp-unit
  :version "0.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Brian O'Reilly <fade@deepsky.com>"
  :description "Parachute's lisp-unit compatibility layer."
  :homepage "https://github.com/fade/zebra"
  :bug-tracker "https://github.com/fade/zebra/issues"
  :source-control (:git "https://github.com/fade/zebra.git")
  :serial T
  :components ((:file "lisp-unit"))
  :depends-on (:zebra))
