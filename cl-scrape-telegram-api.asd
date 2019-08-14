#|
  This file is a part of cl-scrape-telegram-api
  (c) 2019 Hiro https://protagon.space (hiro@protagon.space)
  Author: Valentin Boettcher <hiro@protagon.space>
|#

(asdf:defsystem cl-scrape-telegram-api
  :name "cl-scrape-telegram-api"
  :version "0.0.1"
  :author "Valentin Boettcher <hiro@protagon.space>"
  :maintainer "Valentin Boettcher <hiro@protagon.space>"
  :license "zlib"
  :description "A tool to scrape the telegram api docs and create lisp code for cl-telegram."
  :homepage ""
  :bug-tracker ""
  ;; :source-control (:git "https://github.com/Shinmera/lquery.git")
  :serial T
  :components ((:file "package")
               (:file "utils")
               (:file "scrape"))
  :depends-on (:alexandria :cl-ppcre :cl-arrows :trivia :cl-json :lquery :dexador))
