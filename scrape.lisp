(in-package :space.protagon.cl-telegram-scrape)

;; Load the api spec
(defvar *url* "https://core.telegram.org/bots/api")
(defvar *request* "")
(defvar *parsed-content* nil)
(defvar *out-package* :cl-telegram-bot)
(defvar *out-file* "out/out.lisp")

;; Unimportant categories
(defparameter *unimportant-categories* #("Recent Changes"
                                       "Authorizing your bot"
                                       "Making requests"
                                         "Payments"))

(defparameter *type-map*
  '(("Integer" . integer)
    ("String" . string)
    ("Boolean" . boolean)
    ("Float number" . float)
    ("True" . nil)
    ("False" . nil)))

(defparameter *blacklist*
  #("Formatting options" "Inline mode objects" "Sending files" "Inline mode methods" "CallbackGame"
    "InputFile" "InputMedia"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              Structures             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct tg-param
  (name "" :type string)
  (type nil :type (or list symbol))
  (optional t :type boolean)
  (desc "" :type string))

(defun param->keyword (param)
  (-> (tg-param-name param) (string-upcase) (make-keyword)))


(defstruct (tg-object
            (:constructor create-tg-object (name parameters doc anchor type return-type)))
  (name "" :type string)
  (type "" :type keyword)
  (parameters nil :type vector)
  (doc "" :type string)
  (anchor "" :type string)
  (return-type))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               Scraping              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-categories ()
  "Returns an alist of categories from the telegram api spec."
  (let ((cat-data
          (remove-if
           #'(lambda (el)
               (find (lquery-funcs:text el) *unimportant-categories* :test 'string-equal))
           (lquery:$ *parsed-content* "#dev_page_content h3"))))
    (map 'list #'(lambda (el) (cons (lquery-funcs:text el) el)) cat-data)))

;; TODO: MAYBE REMOVE
(defun tg-type->lisp-type (type-str)
  (let ((types (ppcre:split " or " type-str)))
    (mapcar #'(lambda (single-type)
                (let* ((is-array (search "Array of " single-type))
                       (el-types (if is-array
                                     (let* ((single-type (ppcre:regex-replace-all "Array of " single-type "")))
                                       (ppcre:split " and " single-type))
                                     `(,single-type)))
                       (types (mapcar #'(lambda (el)
                                          (assoc el *type-map* :test #'string=))
                                      el-types))
                       (type-symbols (mapcar
                                      #'(lambda (type el-type)
                                          (if type
                                              (cdr type)
                                              (camel->symbol el-type)))
                                      types el-types)))
                  (if is-array
                      `(array ,(if (> (length type-symbols) 1)
                                   `(or ,@type-symbols)
                                   (car type-symbols)))
                      (car type-symbols))))
            types)))

(defun parse-parameters (param-table)
  "Creates a vector of th-parameter from an apropriate table element (description of a method)."
  (->> (lquery:$ param-table "tr")
       (map 'vector #'(lambda (el)
                        (lquery:$ el "td" (text))))
       (remove-if #'(lambda (el) (not (= (length el) 4))))
       (map 'vector
            #'(lambda (el)
                (match el
                  ((array :rank 1 :contents (name type optional doc))
                   (make-tg-param :name name
                                  :type (tg-type->lisp-type type)
                                  :optional (string= "Optional" optional)
                                  :desc doc)))))))

(defun parse-fields (field-table)
  "Creates a vector of th-parameter from an apropriate table element (description of a method)."
  (->> (lquery:$ field-table "tr")
     (map 'vector #'(lambda (el)
                      (lquery:$ el "td" (text))))
     (remove-if #'(lambda (el)
                    (not (= (length el) 3))))
     (map 'vector
          #'(lambda (el)
              (match el
                ((array :rank 1 :contents (name type desc))
                 (multiple-value-bind (doc optional) (ppcre:regex-replace "Optional\\.* " desc "")
                   (make-tg-param :name name
                                  :type (tg-type->lisp-type type)
                                  :optional optional
                                  :desc doc))))))))

(defun detect-api-type (table)
  "Detects the type of the declaration in the api. 3 collumns => field, 4 collumns => parameter."
  (case (lquery:$ table "tr" (first) (children) (length))
    (3 :object)
    (4 :method)
    (otherwise :method)))

(defun find-return-type (doc)
      (let* ((ret-sent
               (first
                (remove-if-not
                 #'(lambda (el)
                     (search "RETURN" (string-upcase el)))
                 (ppcre:split "[\\.,]\\s" doc))))
             (href (nth-value 1 (ppcre:scan-to-strings "href=\"#([a-zA-Z]+?)\">"
                                                       ret-sent))))
        (when (and href (> (length href) 0))
          (let* ((anchor (elt href 0))
                 (selector (concatenate 'string "h4>a[href=\"#" anchor "\"]")))
            (lquery:$1 *parsed-content*
              selector
              (parent)
              (text))))))

(defun parse-h4 (h4)
  "Parses an H4 into a method or a struct."
  (declare (type plump-dom:element h4))
  (let ((name (lquery:$1 h4 (text))))
    (when (or (not (and (lquery:$ h4 (is "h4"))
                   (lquery:$ h4 (next) (is "p"))))
             (find name *blacklist* :test #'string=))
      (return-from parse-h4 nil))



    (let* ((anchor (lquery:$1 h4 "a" (attr :href)))
           (doc-elt (lquery:$1 h4 (next)))
           (doc (lquery:$1 doc-elt (text)))
           (return-type (find-return-type (lquery:$1 doc-elt (serialize)))))
      (h4->tg-object h4 name anchor doc return-type))))

(defun has-table-p (h4)
  "Checks if there is a corresponding table to a h4."
  (emptyp (lquery:$ h4 (next-until "table") (filter "h4"))))

(defun h4->tg-object (h4 name anchor doc return-type)
  (declare (type plump-dom:element h4))
  (let* ((table (lquery:$ h4 (next-until "table") (next)))
         (has-table (has-table-p h4))
         (type (if has-table
                   (detect-api-type table)
                   :method))
         (params (if (has-table-p h4)
                     (case type
                       (:method
                           (parse-parameters table))
                       (:object
                        (parse-fields table)))
                     #())))
    (create-tg-object name params doc anchor type return-type)))

(defun parse-categories (categories)
  "Parses the given categorues into a `tg-method` returning an alist of (name . (vektor of parsed))."
  (mapcar
   #'(lambda (it)
       (let ((name (car it))
             (element (cdr it))
             (parsed nil))
         (do ((el (lquery:$1 element (next)) (lquery:$1 el (next))))
             ((or (not (lquery:$1 el (next))) (lquery:$ el (is "h3"))))
           (when (lquery:$ el (is "h4"))
             (let ((meth (parse-h4 el)))
               (when meth (push meth parsed)))))
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
  (-> (tg-param-name param) (json:camel-case-to-lisp) (intern)))

(defun tg-object->lisp (object)
  (let* ((name (tg-object-name object))
         (name-sym (camel->symbol name))
         (params (tg-object-parameters object))
         (sep-params (separate-params params))
         (req-args (first sep-params))
         (opt-args (second sep-params))
         (type (tg-object-type object))
         (return-type (tg-object-return-type object))
         (docstring (make-docstring-from-tg-object object)))
    (case type
      (:method (tg-object->function name name-sym req-args opt-args docstring return-type))
      (:object (tg-object->clos name name-sym params docstring)))))

(defun make-docstring-from-tg-object (object)
  (format nil "~a~a~%~a" *url* (tg-object-anchor object) (tg-object-doc object)))

(defun make-argument-list (req-args opt-args)
  `(,@(map 'list #'param->arg req-args) ,@(if opt-args `(&key ,@(map 'list #'param->arg opt-args)))))

(defun make-type-specifier (type)
  (if (> (length type) 1) `((or ,@type)) type))

(defun tg-object->function (name name-sym req-args opt-args docstring return-type)
  "Creates a function for use in `cl-telegram-bot` from a tg-object."
  `(defun ,name-sym ,(make-argument-list req-args opt-args)
     ,docstring
     ,@(mapcar
        #'(lambda (opt)
            (let ((type (tg-param-type opt)))
              (when type
                `(check-type ,(param->arg opt) ,@(make-type-specifier type)))))
        req-args)
     (let ((options
             (list ,@(mapcar #'(lambda (opt)
                                 `(cons ,(param->keyword opt)
                                        ,(param->arg opt)))
                             req-args))))
       ,@(mapcar #'(lambda (param)
                     `(when ,(param->arg param)
                        (setf options (nconc options (list (cons ,(param->keyword param) ,(param->arg param)))))))
                 opt-args)
       (list ,name options ,@(when return-type
                               `(:return-type (quote ,(camel->symbol return-type))))))))

;; (make-request bot ,name options ,@(when return-type
;;                                            (let ((ret-sym (camel->symbol (car return-type))))
;;                                              `(:return-type
;;                                                ,(if (cdr return-type)
;;                                                     `(quote (array ,ret-sym))
;;                                                     `(quote ,ret-sym))))))
;; TODO: convert name to kw, symbol directly in class

(defun tg-object->clos (name name-sym params docstring)
  "Creates clos object from a tg-object."
  `(defclass ,name-sym ()
     ((type-name :allocation :class
                 :reader name
                 :initform ,name)
      ,@(map 'list #'(lambda (param)
                       (let* ((pname-sym (param->arg param))
                              (pname-kw (make-keyword pname-sym))
                              (optional (tg-param-optional param))
                              (type (tg-param-type param))
                              (doc (tg-param-desc param)))
                         `(,pname-sym
                           :initarg ,pname-kw
                           ,@(unless optional
                               '(:initform nil))
                           :accessor ,(intern (concatenate 'string "TG-" (symbol-name pname-sym)))
                           :type ,@(make-type-specifier type)
                           :documentation ,doc)))
             params))
     (:documentation ,docstring)))

(defun write-file-header (categories stream)
  (format stream "; DO NOT EDIT, AUTO GENERATED~%~%")
  (write `(defpackage ,*out-package*
            (:export
             :*API-TYPES*
             ,@(let ((symbols nil))
                 (dolist (item categories)
                   (let ((objects (cdr item)))
                     (dolist (object objects)
                       (push (make-keyword (camel->symbol (tg-object-name object))) symbols))))
                 (nreverse symbols))))
         :stream stream)

  (write `(in-package ,*out-package*) :stream stream)
  (format stream "~%")
  (write `(defparameter *API-TYPES*
            (quote
             ,(let ((symbols nil))
                (dolist (item categories)
                  (let ((objects (cdr item)))
                    (dolist (object objects)
                      (when (eq :object (tg-object-type object))
                        (push (camel->symbol (tg-object-name object)) symbols)))))
                (nreverse symbols))))
         :stream stream)
  (format stream "~%")
  (write `(defparameter *API-METHODS*
            (quote
             ,(let ((symbols nil))
                (dolist (item categories)
                  (let ((objects (cdr item)))
                    (dolist (object objects)
                      (when (eq :method (tg-object-type object))
                        (push (camel->symbol (tg-object-name object)) symbols)))))
                (nreverse symbols))))
         :stream stream)
    (format stream "~%")
  categories)

(defun print-objects (parsed-cats stream)
  "Takes parsed categories and prints them out to functions."
  (dolist (item parsed-cats)
    (let ((name (car item))
          (objects (cdr item)))
      (format stream "~%;----~a----~%" name)
      (dolist (object objects)
        (write (tg-object->lisp object) :stream stream) ; nicer with generics
            (format stream "~%~%")
        ))))

(defun generate-and-write-functions ()
  "Discovers and generates methods from the telegram api as funcions and writes them to a file."
  (ensure-directories-exist *out-file*)

  (with-open-file (out *out-file* :direction :output :if-exists :supersede)
    (-> (find-categories) (parse-categories) (write-file-header out) (print-objects out))))

(defun scrape-to-disk (&key (url *url*) (out-file *out-file*) (out-package *out-package*))
  "Main entry. Makes the web request and scrapes the telegram api docs."
  (let* ((*request* (dex:get url))
         (*parsed-content* (plump:parse *request*))
         (*out-package* out-package)
         (*out-file* out-file))
    (generate-and-write-functions)))
