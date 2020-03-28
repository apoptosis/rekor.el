;;; rekor.el --- An ORM over EmacSQL -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Dustin Lacewell

;; Author: Dustin Lacewell <dlacewell@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26") (emacsql-sqlite "0") (dash "0") (s "0"))
;; Keywords: org-mode formatting
;; URL: http://github.com/apoptosis/rekor.el

;;; Commentary:

;; This package provides an ORM over EmacSQL

;;; Code:
(require 'emacsql-sqlite)
(require 'dash)
(require 's)

(setq rekor:db (emacsql-sqlite "/tmp/rekor.sql"))

(setq rekor:db:tables (make-hash-table))

(defun rekor:db:generate-template (type index)
  (intern (format "$%s%s" (rekor:db:pattern-for-type type) index)))

(defun rekor:db:pattern-for-type (type)
  (pcase type
    (integer "s")
    (float "s")
    (number "s")
    (string "s")
    (list "v")
    (vector "v")
    (symbol "i")))

(defun rekor:db:generate-set-form (obj slot index)
  `(= ,slot ,(rekor:db:generate-template slot index)))

(defun rekor:db:connect (filename)
  (setq rekor:db (emacsql-sqlite filename)))

(defun rekor:db:exists (table-symbol)
  (= 1 (caar (emacsql rekor:db
                     [:select (funcall count *)
                      :from sqlite_master
                      :where (= name $s1)] table-symbol))))

(defun rekor:db:create (table-symbol fields)
  (unless (rekor:db:exists table-symbol)
    (eval `(emacsql rekor:db [:create-table ,table-symbol ([,@fields])]))))

(defmacro rekor:db:query (table-symbol where &rest values)
  ` (let  ((result (emacsql rekor:db
                            [:select *
                             :from ,table-symbol
                             :where ,where] ,values)))
      (if (= 1 (length result))
          (car result)
        result)))

(defun rekor:db:drop (&optional table-symbol)
  (interactive)
  (let ((table-symbol (or table-symbol (completing-read (map-keys rekor:db:tables)))))
    (when (rekor:db:exists table-symbol)
      (eval `(emacsql rekor:db [:drop-table ,table-symbol])))))

(defun rekor:db:migrate-all ()
  (interactive)
  (map-apply (lambda (table-symbol fields)
               (rekor:db:create table-symbol fields)) rekor:db:tables))

(defun rekor:db:drop-all ()
  (interactive)
  (map-apply (lambda (table-symbol _)
               (rekor:db:drop table-symbol)) rekor:db:tables))

(defun rekor:objs:class-name (model-name)
  (s-lex-format "${model-name}-class"))

(defun rekor:objs:class-symbol (model-name)
  (intern (rekor:objs:class-name model-name)))

(defun rekor:objs:constructor-name (model-name)
  (s-lex-format "${model-name}:new"))

(defun rekor:objs:constructor-symbol (model-name)
  (intern (rekor:objs:constructor-name model-name)))

(defun rekor:objs:generate-class (model-name fields)
  (let ((class-symbol (rekor:objs:class-symbol model-name))
        (slot-forms (-map 'rekor:objs:generate-slot fields)))
    `(defclass ,class-symbol () ,slot-forms)))

(defun rekor:objs:generate-setters (slots)
  (--map (let ((method-name (intern (format "::%s" it))))
           `(defun ,method-name (obj value)
              (oset obj ,it value))) slots))

(defun rekor:objs:generate-getters (slots)
  (--map (let ((method-name (intern (format ":%s" it))))
           `(defun ,method-name (obj)
              (oref obj ,it))) slots))

(defun rekor:objs:generate-slot (slot-form)
  (seq-let (slot-name slot-type _) slot-form
    `(,slot-name :initarg ,(intern (s-lex-format ":${slot-name}"))
            :accessor ,(intern (s-lex-format ":${slot-name}"))
            :type ,slot-type)))

;; (rekor:generate-slot '(foo bar))

(defun rekor:objs:generate-constructor (model-name)
  (let ((constructor-symbol (rekor:objs:constructor-symbol model-name)))
    `(defun ,constructor-symbol (&rest initargs)
       (rekor:objs:new ',model-name initargs))))

(defun rekor:objs:slot-value (obj slot)
  (let ((slot-name (intern (format ":%s" slot))))
    (if (eval `(slot-boundp ,obj ,slot-name))
      (eval `(oref ,obj ,slot-name))
      nil)))

(defun rekor:objs:values (obj)
  (--map (rekor:objs:slot-value obj it) (object-slots obj)))

(defun rekor:objs:new (model-name initargs)
  (let* ((class-symbol (rekor:objs:class-symbol model-name))
         (obj (apply class-symbol initargs))
         (values (rekor:objs:values obj))
         (_ (eval `(emacsql rekor:db [:insert-into ,model-name :values [,@values]])))
         (id (caar (emacsql rekor:db [:select (funcall last_insert_rowid)]))))
    (oset obj :id id)
    obj))

(defmacro :? (model-name where &rest values)
  `(let* ((class-symbol (rekor:objs:class-symbol ',model-name))
          (data (emacsql rekor:db
                         [:select * :from ,model-name :where ,where] ,@values))
          (slot-infos (eieio--class-slots (eieio--class-object class-symbol)))
          (slots (--map (aref it 1) slot-infos))
          (initargs (--map (eieio--class-slot-initarg (eieio--class-object class-symbol) it) slots))
          (params (--map (-interleave initargs it) data)))
     (--map (apply class-symbol it) params)))

(defun rekor:save (obj)
  (let* ((class (eieio-object-class obj))
         (class-name (eieio-object-class-name obj))
         (model-name (make-symbol (s-chop-suffix "-class" (symbol-name class-name))))
         (slots (object-slots obj))
         (values (rekor:objs:values obj))
         (set-forms (--map-indexed (rekor:db:generate-set-form obj it (+ 2 it-index))
                                   (cdr slots)))
         (query-form `(emacsql rekor:db [:update ,model-name :set [,@set-forms] :where (= id $s1)] ,@values)))
    (eval query-form)))

(defmacro defmodel (model-name &rest fields)
  (let* ((id-field '(id integer :primary-key :autoincrement :unique))
         (fields (append (list id-field) fields))
         (class (rekor:objs:generate-class model-name fields))
         (constructor (rekor:objs:generate-constructor model-name))
         (slots (-map 'car fields))
         (setters (rekor:objs:generate-setters slots))
         (getters (rekor:objs:generate-getters slots)))
    (map-put rekor:db:tables model-name fields)
    `(progn
       ,class
       ,constructor
       ,@setters
       ,@getters)))

(provide 'rekor)

;;; rekor.el ends here
