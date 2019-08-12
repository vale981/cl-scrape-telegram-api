(ql:quickload '(:alexandria :cl-arrows :cl-json :cl-ppcre))
(defpackage :space.protagon.cl-telegram-scrape.utils
  (:use :common-lisp :alexandria :cl-arrows)
  (:export :lispify
   :telegramify
           :camel->symbol
   :snake->keyword
           :snake->symbol))
(in-package :space.protagon.cl-telegram-scrape.utils)

(defun lispify (str)
  "Converts a snake case string to dash-style."
  (string-upcase (cl-ppcre:regex-replace-all "_" str "-")))

(defun telegramify (str)
  "Converts dash-style to snake-case."
  (string-downcase (cl-ppcre:regex-replace-all "-" str "_")))

(defun camel->symbol (ident)
  (intern (json:camel-case-to-lisp ident)))

(defun snake->keyword (str)
  (-> str (lispify) (make-keyword)))

(defun snake->symbol (str)
  (-> str (lispify) (intern)))
