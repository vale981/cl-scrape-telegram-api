(ql:quickload '(:dexador :plump :lquery :alexandria :cl-arrows :cl-json :cl-ppcre))
(defpackage :space.protagon.cl-telegram-scrape
  (:use :common-lisp :alexandria :cl-arrows :space.protagon.cl-telegram-scrape.utils))
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

(defconstant *method-categories* #("Available methods"
                                   "Updating messages"
                                   "Stickers"))

(defstruct tg-param
  (name "" :type string)
  (type "" :type string)
  (optional t :type boolean)
  (desc "" :type string))

(defun make-tg-param-from-vec (lst)
  (make-tg-param :name (elt lst 0)
                        :type (elt lst 1)
                        :optional (string= "Optional" (elt lst 2))
                 :desc (elt lst 3)))

(defun param->keyword (param)
  (-> (tg-param-name param) (string-upcase) (make-keyword)))


(defstruct (tg-method
            (:constructor create-tg-method (name parameters doc anchor)))
  (name "" :type string)
  (parameters nil :type vector)
  (doc "" :type string)
  (anchor "" :type string))

(defun find-categories ()
  "Returns an alist of categories from the telegram api spec."
  (let ((cat-data
          (remove-if
           #'(lambda (el)
               (find (lquery-funcs:text el) *unimportant-categories* :test 'string-equal))
           (lquery:$ *parsed-content* "#dev_page_content h3"))))
    (map 'list #'(lambda (el) (cons (lquery-funcs:text el) el)) cat-data)))

(defun parse-parameters (param-table)
  "Creates a vector of th-parameter from an apropriate table element."
  (->> (lquery:$ param-table "tr")
     (map 'vector #'(lambda (el)
                      (lquery:$ el "td" (text))))
     (remove-if #'(lambda (el) (not (= (length el) 4))))
     (map 'vector #'make-tg-param-from-vec))
  )

(defun h4->tg-method (h4)
  (declare (type plump-dom:element h4))
  (let* ((name (lquery:$1 h4 (text)))
         (anchor (lquery:$1 h4 "a" (attr :href)))
         (doc-elt (lquery:$1 h4 (next)))
         (doc (lquery:$1 doc-elt (text)))
         (param-elt (lquery:$1 doc-elt (next)))
         (params (parse-parameters param-elt)))
    (print doc-elt)
    (create-tg-method name params doc anchor)))

(defun separate-params (params)
  "Separates a sequence of `tg-param` into (required optional)"
  (reduce (lambda (a b)
            (if (tg-param-optional a)
                (push a (second b))
                (push a (first b)))
            b)
          params
          :initial-value (list nil nil)
          :from-end t))

(defun tg-method->function (method)
  "Creates a function for use in `cl-telegram-bot` from a cl-method-object."
  (let* ((name (tg-method-name method))
         (name-sym (camel->symbol name))
         (params (separate-params (tg-method-parameters method)))
         (req-args (first params))
         (opt-args (second params)))
    `(defun ,name-sym (bot ,@(map 'list #'param->arg req-args) &key ,@(map 'list #'param->arg opt-args))
       ,(format nil "~a~a~%~a" *url* (tg-method-anchor method) (tg-method-doc method))
       (let ((options
               (list
                ,@(mapcar #'(lambda (opt)
                              `(cons ,(param->keyword opt)
                                     ,(param->arg opt)))

                          req-args))))
         ,@(mapcar #'(lambda (param)
                       `(when ,(param->arg param)
                          (nconc options (list (cons ,(param->keyword param) ,(param->arg param))))))
                   opt-args)
         (make-request bot ,name options)))))
