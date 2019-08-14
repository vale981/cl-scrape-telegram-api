#|
  This file is a part of cl-scrape-telegram-api
  (c) 2019 Hiro https://protagon.space (hiro@protagon.space)
  Author: Valentin Boettcher <hiro@protagon.space>
|#


(defpackage :space.protagon.cl-telegram-scrape.utils
  (:use :common-lisp :alexandria :cl-arrows)
  (:export :lispify
   :telegramify
           :camel->symbol
   :snake->keyword
           :snake->symbol))

(defpackage :space.protagon.cl-telegram-scrape
  (:nicknames :tg-scrape)
  (:use :common-lisp :alexandria :cl-arrows :space.protagon.cl-telegram-scrape.utils :trivia)
  (:export
   :scrape-to-disk))
