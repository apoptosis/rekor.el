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

(defmacro with-var (expr &rest forms)
  (declare (indent defun))
  `(let ((it ,expr))
     (progn ,@forms)))

(defmacro with-vars (expr &rest forms)
  (declare (indent defun))
  (let ((vars (--map-indexed (list (intern (format "it%s" (if (= 0 it-index) "" (+ 1 it-index)))) it) expr)))
    `(let* (,@vars) ,@forms)))

;; (with-var (+ 5 5)
;;   (format "Answer is: %s" it))

;; (with-vars ((+ 5 5)
;;             (+ 25 5))
;;   (format "Answer is: %s and %s" it it2))

(defmacro quickclass (class-name slot-forms &optional documentation)

  (let* ((slot-updater (lambda (form sym fmt) (append form (list sym (intern (format fmt (car form)))))))
         (slots (--map (-as-> it slot
                              (if (not (or (seq-contains-p slot :initarg)
                                           (seq-contains-p slot :noinit)))
                                  (funcall slot-updater slot :initarg ":%s")
                                slot)
                              (if (seq-contains-p slot :noinit)
                                  (-filter (lambda (it) (not (equal :noinit it))) slot)
                                slot)
                              (if (not (seq-contains-p slot :reader))
                                  (funcall slot-updater slot :reader ":%s")
                                slot)
                              (if (not (seq-contains-p slot :writer))
                                  (funcall slot-updater slot :writer "::%s")
                                slot))
                       slot-forms)))
    `(defclass ,class-name () (,@slots) ,(when documentation documentation))))

(quickclass rekor:column
  ((table
    :type rekor:table
    :documentation "Parent table of the column.")
   (name
    :type symbol
    :documentation "Name of the SQL column.")
   (datatype
    :type symbol
    :documentation "The type of column.")
   (constraints
    :type list
    :initform nil
    :documentation "The constraints on the column.")
   (foreign-key-table
    :noinit
    :type symbol
    :documentation "The related table.")
   (foreign-key-column
    :noinit
    :type symbol
    :documentation "The related column.")
   (foreign-key-constraints
    :noinit
    :type list
    :documentation "The foreign-key constraints."))
  "An SQL column.")

(cl-defmethod rekor:column:to-string ((column rekor:column))
  (with-slots (name datatype constraints) column
    (if (rekor:column:foreign-key-p column)
        (with-slots (foreign-key-table foreign-key-column) column
          (s-lex-format "${name} ${datatype} ${constraints} => ${foreign-key-table}.${foreign-key-column}"))
      (s-lex-format "${name} ${datatype} ${constraints}"))))

;; (rekor:column:to-string (rekor:column :name 'foo :datatype 'string))

(cl-defmethod rekor:column:foreign-key-p ((column rekor:column))
  (and (slot-boundp column 'foreign-key-table)
       (slot-boundp column 'foreign-key-column)))

(cl-defmethod rekor:column:foreign-key ((column rekor:column))
  (when (rekor:column:foreign-key-p column)
    (with-slots (foreign-key-table foreign-key-column) column
        (list :foreign-key (vector (oref column name))
              :references foreign-key-table (vector foreign-key-column)))))

(cl-defmethod rekor:column:create-forms ((column rekor:column))
  `( ,(:name column) ,(:datatype column) ,@(:constraints column)))

(quickclass rekor:table
  ((database
    :type rekor:database
    :documentation "Parent database of the table.")
   (name
    :type symbol
    :documentation "Name of the SQL table.")
   (columns
    :noinit
    :type hash-table
    :documentation "Mapping of columns in the table"))
  "An SQL table.")

(cl-defmethod :columns ((table rekor:table))
  (if (slot-boundp table 'columns)
      (oref table columns)
    (oset table columns (make-hash-table))
    (oref table columns)))

(cl-defmethod rekor:table:to-string ((table rekor:table))
  (with-slots (name columns) table
    (if (map-empty-p columns) name
      (with-var (s-join "\n"
                        (--map (format " | %s" (rekor:column:to-string it))
                               (map-values columns)))
        (s-lex-format "${name}\n${it}")))))

(cl-defmethod rekor:table:add-column ((table rekor:table) column)
  (map-put! (:columns table) (:name column) column))

(cl-defmethod rekor:table:insert-values ((table rekor:table) values)
  (with-slots (database name) table
    (eval `(rekor:database:execute database [:insert-into ,name :values [,@values]])
          (list (cons 'database database)))))

(cl-defmethod rekor:table:foreign-keys ((table rekor:table))
  (map-values-apply 'rekor:column:foreign-key (:columns table)))

(cl-defmethod rekor:table:column-forms ((table rekor:table))
  (map-values-apply 'rekor:column:create-forms (:columns table)))

(cl-defmethod rekor:table:create ((table rekor:table))
  (let* ((table-name (oref table name))
         (fields (rekor:table:column-forms table))
         (foreign-keys (-non-nil (rekor:table:foreign-keys table))))
    (eval
     `(rekor:database:execute database
                              [:create-table :if-not-exists ,table-name
                               ([,@fields] ,@foreign-keys)])
     (list (cons 'database (:database table))))))

(defun rekor:database:generate-template (type index)
  (intern (format "$%s%s" (rekor:database:pattern-for-type type) index)))

(defun rekor:database:pattern-for-type (type)
  (pcase type
    (integer "s")
    (float "s")
    (number "s")
    (string "s")
    (list "v")
    (vector "v")
    (symbol "i")))

(defun rekor:database:generate-set-form (obj slot index)
  `(= ,slot ,(rekor:database:generate-template slot index)))

(quickclass rekor:database
            ((filename
              :initform "/tmp/test.sql"
              :type string
              :documentation "Filename of the SQLite database.")
             (cursor
              :noinit
              :type emacsql-sqlite-connection
              :documentation "Cursor connection to the SQLite database.")
             (tables
              :noinit
              :type hash-table
              :documentation "Mapping of known tables."))
            "An SQLite database.")

(defun rekor:database (&rest args)
  (let ((database (apply 'make-instance 'rekor:database args)))
    (rekor:database:connect database)
    database))


(cl-defmethod :tables ((database rekor:database))
  (if (slot-boundp database 'tables)
      (oref database tables)
    (oset database tables (make-hash-table))
    (oref database tables)))

(cl-defmethod rekor:database:to-string ((database rekor:database))
  (with-slots (filename tables) database
    (if (map-empty-p tables) filename
      (with-var (s-join "\n"
                        (--map (format " - %s" (rekor:table:to-string it))
                               (map-values tables)))
        (s-lex-format "${filename}:\n${it}")))))

(cl-defmethod rekor:database:add-table ((database rekor:database) table)
  (map-put! (:tables database) (oref table name) table))

(cl-defmethod rekor:database:connect ((obj rekor:database))
  (with-slots (filename) obj
    (::cursor obj (emacsql-sqlite filename))))

(cl-defmethod rekor:database:execute ((db rekor:database) &rest query)
  (eval `(emacsql cursor ,@query)
        (list (cons 'cursor (:cursor db)))))

(cl-defmethod rekor:database:last-row-id ((database rekor:database))
  (caar (rekor:database:execute database [:select (funcall last_insert_rowid)])))

(cl-defmethod rekor:database:table-exists ((db rekor:database) table-symbol)
  (= 1 (caar (emacsql rekor:db
                     [:select (funcall count *)
                      :from sqlite_master
                      :where (= name $s1)] table-symbol))))

(cl-defmethod rekor:database:create-tables ((database rekor:database))
  (map-values-apply 'rekor:table:create (:tables database)))

(cl-defmethod rekor:database:drop-tables ((database rekor:database))
  (delete-file (:filename database)))

(defun rekor:database:create (table-symbol fields &optional foreign-keys)
  (unless (rekor:database:exists table-symbol)
    (eval `(emacsql rekor:db [:create-table ,table-symbol ([,@fields] ,@foreign-keys)]))))

(defmacro rekor:database:query (table-symbol where &rest values)
  ` (let  ((result (emacsql rekor:db
                            [:select *
                             :from ,table-symbol
                             :where ,where] ,values)))
      (if (= 1 (length result))
          (car result)
        result)))

(defun rekor:database:drop (&optional table-symbol)
  (interactive)
  (delete-file "/tmp/test.sql")
  ;; TODO sort tables and delete in the proper order to avoid foreign-key constraints
  ;; (let ((table-symbol (or table-symbol (completing-read (map-keys rekor:db:tables)))))
  ;;   (when (rekor:db:exists table-symbol)
  ;;     (eval `(emacsql rekor:db [:drop-table ,table-symbol]))))
  )

(defun rekor:objs:class-name (model-name)
  (s-lex-format "${model-name}-class"))

(defun rekor:objs:class-symbol (model-name)
  (intern (rekor:objs:class-name model-name)))

(defun rekor:objs:constructor-name (model-name)
  (s-lex-format "${model-name}:new"))

(defun rekor:objs:constructor-symbol (model-name)
  (intern (rekor:objs:constructor-name model-name)))

(defun rekor:objs:generate-slot (slot-form)
  (seq-let (slot-symbol slot-type _) slot-form
    (if (s-starts-with-p "__" (symbol-name slot-symbol))
        `(,slot-symbol :type ,slot-type :allocation :class)
      `(,slot-symbol :initarg ,(intern (s-lex-format ":${slot-symbol}"))
                   :accessor ,(intern (s-lex-format ":${slot-symbol}"))
                   :type ,slot-type))))

;; (rekor:objs:generate-slot '(foo bar))
;; (rekor:objs:generate-slot '(__foo bar))

(defun rekor:objs:generate-class (model-name fields)
  (let ((class-symbol (rekor:objs:class-symbol model-name))
        (slot-forms (-map 'rekor:objs:generate-slot fields)))
    `(defclass ,class-symbol () ,slot-forms)))

(defun rekor:objs:public-slots (obj)
  (--reject (s-starts-with-p "__" (symbol-name it)) (object-slots obj)))

(defun rekor:objs:generate-setters (class-symbol slots)
  (--map (let ((method-name (intern (format ":!%s" it))))
           `(cl-defmethod ,method-name ((obj ,class-symbol) value)
              (if (object-p value)
                  (let ((id (oref value id)))
                    (oset obj ,it id))
                (oset obj ,it value))))
         slots))

(defun rekor:objs:generate-getters (class-symbol slots)
  (--map (let ((method-name (intern (format ":?%s" it))))
           `(cl-defmethod ,method-name ((obj ,class-symbol) &optional related-model-name)
              (let ((value (oref obj ,it)))
                (if related-model-name
                    (let* ((results (eval (list :? related-model-name (list '= 'id value))))
                           (first (car results)))
                      (or first value))
                  value))
              )) slots))

(defun rekor:objs:generate-constructor (model-name)
  (let ((constructor-symbol (rekor:objs:constructor-symbol model-name)))
    `(defun ,constructor-symbol (&rest initargs)
       (rekor:objs:new ',model-name initargs))))

(defun rekor:objs:generate-query (model-name where &rest values)
  `(let* ((class-symbol (rekor:objs:class-symbol ',model-name))
          (dummy (apply class-symbol nil))
          (table (oref dummy __table))
          (database (oref table database))
          (data (rekor:database:execute database
                                        [:select * :from ,model-name :where ,where] ,@values))
          (slot-infos (eieio--class-slots (eieio--class-object class-symbol)))
          (slots (--map (aref it 1) slot-infos))
          (initargs (--map (eieio--class-slot-initarg (eieio--class-object class-symbol)
                                                      it) slots))
          (params (--map (-interleave initargs it) data)))
     (--map (apply class-symbol it) params)))

(defun rekor:objs:generate-lookup (obj attribute &optional related-model-name)
  `(let ((value (oref ,obj ,attribute)))
     (if (and ,related-model-name (symbolp ,related-model-name))
         (let* ((results (eval (list :? ,related-model-name (list '= 'id value))))
                (first (car results)))
           (or first value))
       value)))

(defun rekor:objs:slot-value (obj slot)
  (let ((slot-name (intern (format ":%s" slot))))
    (if (eval `(slot-boundp ,obj ,slot-name))
      (eval `(oref ,obj ,slot-name))
      nil)))

(defun rekor:objs:values (obj)
  (--map (rekor:objs:slot-value obj it) (object-slots obj)))

(defun rekor:objs:new (model-name initargs)
  (let* ((initargs (--map (if (object-p it) (oref it id) it) initargs))
         (class-symbol (rekor:objs:class-symbol model-name))
         (obj (apply class-symbol initargs))
         (table (oref obj __table))
         (database (oref table database))
         (values (rekor:objs:values obj)))
    (rekor:table:insert-values table values)
    (oset obj :id (rekor:database:last-row-id database))
    obj))

(defmacro :? (model-name where &rest values)
  (if (symbolp where)
      (apply 'rekor:objs:generate-lookup where model-name values)
    (apply 'rekor:objs:generate-query model-name where values)))

(defun rekor:save (obj)
  (let* ((class (eieio-object-class obj))
         (class-name (eieio-object-class-name obj))
         (model-name (make-symbol (s-chop-suffix "-class" (symbol-name class-name))))
         (slots (object-slots obj))
         (values (rekor:objs:values obj))
         (table (oref obj __table))
         (database (:database table))
         (set-forms (--map-indexed (rekor:database:generate-set-form obj it (+ 2 it-index))
                                   (cdr slots)))
         (query-form `(rekor:database:execute database [:update ,model-name :set [,@set-forms] :where (= id $s1)] ,@values)))
    (eval query-form (list (cons 'database database)))))

(defun rekor:defmodel:insert-id-field (fields)
  (with-var '(id integer :primary-key :autoincrement :unique)
    (append (list it) fields)))

(defun rekor:defmodel:insert-table-field (fields)
  (with-var '(__table rekor:table)
    (append (list it) fields)))

(defun rekor:defmodel:extract-foreign-key (field-form)
  (let ((form-copy (copy-seq field-form))
        (output-form nil)
        (foreign-key-table nil)
        (foreign-key-column nil)
        (field-symbol (car field-form)))
    (while form-copy
      (let ((it (pop form-copy)))
        (if (equal it :foreign-key)
            (setq foreign-key-table (pop form-copy)
                  foreign-key-column (pop form-copy))
          (push it output-form))))
    (list (seq-reverse output-form) (when (and foreign-key-table
                                               foreign-key-column)
                                      (list foreign-key-table
                                            foreign-key-column)))))

(defun rekor:defmodel:generate-column (table field-form)
  (-let* (((fields foreign-key) (rekor:defmodel:extract-foreign-key field-form))
          ((name type . constraints) fields))
    (let ((column (rekor:column :table table
                                :name name
                                :datatype type
                                :constraints constraints)))
      (-when-let ((f-table f-column) foreign-key)
          (::foreign-key-table column f-table)
          (::foreign-key-column column f-column))

      (rekor:table:add-column table column)
      column)))

;; (--> (rekor:database)
;;      (rekor:table :database it :name 'foo)
;;      (rekor:defmodel:generate-column it '(people integer blah :foreign-key biz baz :foo bar))
;;      (:constraints it))

(defun rekor:defmodel:generate-table (database table-name fields)
  (let* ((table (rekor:table :database database
                             :name table-name))
         (columns (--map (rekor:defmodel:generate-column table it) fields)))
    (--each columns (rekor:table:add-column table it))
    (rekor:database:add-table database table)
    table))

(defmacro defmodel (database model-name &rest fields)
  (let* ((database (eval database))
         (table-fields (rekor:defmodel:insert-id-field fields))
         (model-fields (rekor:defmodel:insert-table-field table-fields))
         (table (rekor:defmodel:generate-table database model-name table-fields))
         (class-symbol (rekor:objs:class-symbol model-name))
         (class (eval (rekor:objs:generate-class model-name model-fields)))
         (dummy (funcall class-symbol))
         (constructor (eval (rekor:objs:generate-constructor model-name)))
         (slots (-map 'car fields))
         (setters (-map 'eval (rekor:objs:generate-setters class-symbol slots)))
         (getters (-map 'eval (rekor:objs:generate-getters class-symbol slots)))
         )
    (oset dummy __table table)

    ;; (map-put rekor:database:tables model-name fields)
    ;; `(progn
    ;;    ,class
    ;;    ,constructor
    ;;    ,@setters
    ;;    ,@getters)
    ))

(provide 'rekor)

;;; rekor.el ends here
