(asdf:defsystem zebra-prove
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Brian O'Reilly <fade@deepsky.com>"
  :description "Parachute's Prove compatibility layer."
  :homepage "https://shinmera.com/docs/parachute/"
  :bug-tracker "https://shinmera.com/project/parachute/issues"
  :source-control (:git "https://shinmera.com/project/parachute.git")
  :serial T
  :components ((:file "prove"))
  :depends-on (:zebra
               :cl-ppcre))
