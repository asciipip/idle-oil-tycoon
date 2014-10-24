(in-package :asdf-user)

(defsystem "idle-oil-tycoon"
  :description "idle-oil-tycoon: A system for modeling the Android game Idle Oil Tycoon."
  :version "1.11.0"
  :author "Phil Gold <phil_g@pobox.com>"
  :license "CC0"
  :depends-on (:iterate
               :cl-ppcre
               :cl-heap)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "formatting")
               (:file "experience")
               (:file "investments")
               (:file "ceos")
               (:file "achievements")
               (:file "technologies")
               (:file "state")
               (:file "actions")
               (:file "search")))
