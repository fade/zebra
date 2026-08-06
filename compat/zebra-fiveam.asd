(asdf:defsystem zebra-fiveam
  :version "0.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Brian O'Reilly <fade@deepsky.com>"
  :description "Parachute's FiveAM compatibility layer."
  :homepage "https://github.com/fade/zebra"
  :bug-tracker "https://github.com/fade/zebra/issues"
  :source-control (:git "https://github.com/fade/zebra.git")
  :serial T
  :components ((:file "fiveam"))
  :depends-on (:zebra))
