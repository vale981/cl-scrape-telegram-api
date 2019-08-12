(ql:quickload '(:dexador :plump :lquery :alexandria :cl-arrows :cl-json :cl-ppcre))
(defpackage :space.protagon.cl-telegram-scrape
  (:use :common-lisp :alexandria :cl-arrows :space.protagon.cl-telegram-scrape.utils))
(in-package :space.protagon.cl-telegram-scrape)

;; Load the api spec
(defvar *url* "https://core.telegram.org/bots/api")
(defvar *request* "")
(defvar *parsed-content* nil)
(defvar *out-package* :space.protagon.cl-telegram)
(defvar *out-file* "out.lisp")

;; Unimportant categories
(defconstant unimportant-categories* #("Recent Changes"
                                       "Authorizing your bot"
                                       "Making requests"
                                       "Getting updates"
                                       "Available Types"
                                       "Payments"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              Structures             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               Scraping              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-categories ()
  "Returns an alist of categories from the telegram api spec."
  (let ((cat-data
          (remove-if
           #'(lambda (el)
               (find (lquery-funcs:text el) unimportant-categories* :test 'string-equal))
           (lquery:$ *parsed-content* "#dev_page_content h3"))))
    (map 'list #'(lambda (el) (cons (lquery-funcs:text el) el)) cat-data)))

(defun parse-parameters (param-table)
  "Creates a vector of th-parameter from an apropriate table element."
  (->> (lquery:$ param-table "tr")
       (map 'vector #'(lambda (el)
                        (lquery:$ el "td" (text))))
       (remove-if #'(lambda (el) (not (= (length el) 4))))
       (map 'vector #'make-tg-param-from-vec)))

(defun h4->tg-method (h4)
  (declare (type plump-dom:element h4))
  (if (not (lquery:$ h4 (is "h4")))
      (return-from h4->tg-method nil))

  (let* ((name (lquery:$1 h4 (text)))
         (anchor (lquery:$1 h4 "a" (attr :href)))
         (doc-elt (lquery:$1 h4 (next)))
         (param-elt (lquery:$ h4 (next-until "table") (next))))
    (if (and (lquery:$ h4 (next) (is "p"))
           (or (lquery:$ doc-elt (next) (is "table")) (lquery:$ doc-elt (next) (next) (is "table"))))
        (let ((doc (lquery:$1 doc-elt (text)))
              (params (parse-parameters param-elt)))
          (create-tg-method name params doc anchor))
        nil)))

(defun parse-categories (categories)
  "Parses the given categorues into a `tg-method` returning an alist of (name . (vektor of parsed))."
  (mapcar #'(lambda (it)
              (let ((name (car it))
                    (element (cdr it))
                    (parsed nil))
                (do ((el (lquery:$1 element (next)) (lquery:$1 el (next))))
                    ((or (not (lquery:$1 el (next))) (lquery:$ el (is "h3"))))
                  (when (lquery:$ el (is "h4"))
                    (let ((meth (h4->tg-method el)))
                      (when meth (push (h4->tg-method el) parsed)))))
                (cons name (nreverse parsed))))
          categories))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;            Code Generator           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun param->arg (param)
  "Converts a TG-PARAM into a symbol to be used as argument.n"
  (-> (tg-param-name param) (snake->symbol)))

(defun tg-method->function (method)
  "Creates a function for use in `cl-telegram-bot` from a cl-method-object."
  (let* ((name (tg-method-name method))
         (name-sym (camel->symbol name))
         (params (separate-params (tg-method-parameters method)))
         (req-args (first params))
         (opt-args (second params)))
    `(defun ,name-sym (bot ,@(map 'list #'param->arg req-args) ,@(if opt-args `(&key ,@(map 'list #'param->arg opt-args))))
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


(defun write-file-header (stream)
  (write `(in-package ,*out-package*) :stream stream))

(defun print-methods (parsed-cats stream)
  "Takes parsed categories and prints them out to functions."
  (dolist (item parsed-cats)
    (let ((name (car item))
          (methods (cdr item)))
      (format stream "~%;----~a----~%" name)
      (dolist (method methods)
        (write (tg-method->function method) :stream stream)
        (format stream "~%~%")))))

(defun generate-and-write-functions ()
  "Discovers and generates methods from the telegram api as funcions and writes them to a file."
  (with-open-file (out *out-file* :direction :output :if-exists :supersede)
    (write-file-header out)
    (-> (find-categories) (parse-categories) (print-methods out))))

(defun scrape-to-disk (&key (url *url*) (out-file *out-file*) (out-package *out-package*))
  "Main entry. Makes the web request and scrapes the telegram api docs."
  (let* ((*request* (dex:get url))
         (*parsed-content* (plump:parse *request*))
         (*out-package* out-package)
         (*out-file* out-file))
    (generate-and-write-functions)))
