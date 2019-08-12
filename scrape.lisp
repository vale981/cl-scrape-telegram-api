(ql:quickload '(:dexador :plump :lquery :alexandria))
(defpackage :space.protagon.cl-telegram-scrape
  (:use :common-lisp :alexandria))
(in-package :space.protagon.cl-telegram-scrape)

;; Load the api spec
(defvar *url* "https://core.telegram.org/bots/api")
(defvar *request* (dex:get *url*))
(defvar *parsed-content* (plump:parse *request*))

;; Unimportant categories
(defconstant *unimportant-categories* #("Recent Changes"
                                        "Authorizing your bot"
                                        "Making requests"
                                        "Getting updates"
                                        "Available Types"))
(defun find-categories ()
  (lquery:$ *parsed-content* "#dev_page_content h3"))
