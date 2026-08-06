(asdf:defsystem zebra-prove
  :version "0.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Brian O'Reilly <fade@deepsky.com>"
  :description "Parachute's Prove compatibility layer."
  :homepage "https://github.com/fade/zebra"
  :bug-tracker "https://github.com/fade/zebra/issues"
  :source-control (:git "https://github.com/fade/zebra.git")
  :serial T
  :components ((:file "prove"))
  :depends-on (:zebra
               :cl-ppcre))
