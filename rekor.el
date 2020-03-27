;;; rekor.el --- An ORM over EmacSQL -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Dustin Lacewell

;; Author: Dustin Lacewell <dlacewell@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26") (emacsql-sqlite "0"))
;; Keywords: org-mode formatting
;; URL: http://github.com/apoptosis/rekor.el

;;; Commentary:

;; This package provides an ORM over EmacSQL

;;; Code:
(require 'emacsql-sqlite)

(setq rekor:db (emacsql-sqlite "/tmp/rekor.sql"))

(setq rekor:db:tables (make-hash-table))

(defun rekor:objs:generate-slot (form)
  (seq-let (name type _) form
    `(,name :initarg ,(intern (s-lex-format ":${name}"))
            :accessor ,(intern (s-lex-format ":${name}"))
            :type ,type)))

;; (rekor:generate-slot '(foo bar))

(defun rekor:objs:slot-value (obj slot)
  (let ((slot-name (intern (format ":%s" slot))))
    (if (eval `(slot-boundp obj ,slot-name))
      (eval `(oref ,obj ,slot-name))
      nil)))

(defun rekor:objs:values (obj)
  (--map (rekor:objs:slot-value obj it) (object-slots obj)))

(defun rekor:objs:new (class initargs)
  (let* ((obj (apply class initargs))
         (values (rekor:objs:values obj))
         (_ (eval `(emacsql rekor:db [:insert-into ,class :values [,@values]])))
         (id (caar (emacsql rekor:db [:select (funcall last_insert_rowid)]))))
    (oset obj :id id)
    obj))

(defun rekor:objs:save (obj)
  (let* ((class (eieio-object-class obj))
         (name (eieio-object-class-name obj))
         (slots (object-slots obj))
         (values (rekor:objs:values obj))
         (set-forms (--map-indexed (rekor:db:generate-set-form obj it (+ 2 it-index))
                                   (cdr slots)))
         (query-form `(emacsql rekor:db [:update ,name :set [,@set-forms] :where (= id $s1)] ,@values)))
    (eval query-form)))

(defmacro rekor:objs:find (class where &rest values)
  `(let* ((data (emacsql rekor:db
                         [:select * :from ,class :where ,where] ,@values))
          (slot-infos (eieio--class-slots (eieio--class-object ,class)))
          (slots (--map (aref it 1) slot-infos))
          (initargs (--map (eieio--class-slot-initarg (eieio--class-object ,class) it) slots))
          (params (--map (-interleave initargs it) data)))
     (--map (apply ,class it) params)))

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

(defun rekor:db:exists (table-name)
  (= 1 (caar (emacsql rekor:db
                     [:select (funcall count *)
                      :from sqlite_master
                      :where (= name $s1)] table-name))))

(defun rekor:db:create (name fields)
  (unless (rekor:db:exists name)
    (eval `(emacsql rekor:db [:create-table ,name ([,@fields])]))))

(defmacro rekor:db:query (table where values)
  ` (let  ((result (emacsql rekor:db
                            [:select *
                             :from ,table
                             :where ,where] ,values)))
      (if (= 1 (length result))
          (car result)
        result)))

(defun rekor:db:drop (&optional name)
  (interactive)
  (let ((name (or name (completing-read (map-keys rekor:db:tables)))))
    (when (rekor:db:exists name)
      (eval `(emacsql rekor:db [:drop-table ,name])))))

(defun rekor:db:migrate-all ()
  (interactive)
  (map-apply (lambda (name fields)
               (rekor:db:create name fields)) rekor:db:tables))

(defun rekor:db:drop-all ()
  (interactive)
  (map-apply (lambda (name _)
               (rekor:db:drop name)) rekor:db:tables))

(defun rekor:objs:generate-setters (class slots)
  (--map `(defun ,(intern (format "%s:%s" ,class it)) (obj value)
            (oset obj ,it value)) slots))

(defmacro defmodel (name &rest fields)
  (let* ((id-field '(id integer :primary-key :autoincrement :unique))
         (fields (append (list id-field) fields))
         (slot-forms (-map 'rekor:objs:generate-slot fields))
         (class-name (intern (s-lex-format "${name}-class")))
         (constructor-name (intern (s-lex-format "${name}:new")))
         (save-method-name (intern (s-lex-format "${name}:save")))
         (setters (rekor:objs:generate-setters name (-map 'car fields))))
    (map-put rekor:db:tables name fields)
    `(progn
       (defclass ,name () ,slot-forms)

       (cl-defmethod rekor:save ((obj ,name))
         (rekor:objs:save obj))

       ,@setters

       (defun ,constructor-name (&rest initargs)
         (rekor:objs:new ,name initargs)))))

(provide 'rekor)

;;; rekor.el ends here
