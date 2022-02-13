;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun user-class-name (class-sym)
  (intern (str:concat "USER-" (string class-sym))
          (symbol-package class-sym)))

(defvar *user-classes* (make-hash-table)
  "Keys are the user class symbols (without the 'USER-' prefix), values their
superclasses as specified by `define-user-class'.

In particular, generated user classes from `define-configuration' are not
included.")

(export-always 'define-user-class)
(defmacro define-user-class (name &optional superclasses)
  "Define the user class of NAME.
This helper function is useful to compose the customizations of a class.

The resulting class is named with the return value of (user-class-name NAME).

This may be called multiple times.
NAME must be an existing class.
NAME is automatically appended to SUPERCLASSES, so that 'user-NAME' inherits
from NAME last."
  `(set-user-class ',name ',superclasses :register-p))

(defun set-user-class (class-sym &optional superclasses register-p)
  "See `define-user-class'
When register-p is nil, does not register in `*user-classes*'.
This is useful for local changes to a class, or to add generated superclasses."
  (let ((user-class-name (user-class-name class-sym))
        (superclasses-with-original (remove-duplicates
                                     (append superclasses (list class-sym)))))
    (progn
      (export-always user-class-name (symbol-package user-class-name))
      ;; Probably no need to call the defclass macro if we just need to
      ;; set the superclasses.
      (when (and register-p (hash-table-p *user-classes*))
        (setf (gethash class-sym *user-classes*) superclasses-with-original))
      (closer-mop:ensure-class user-class-name
                               :direct-superclasses superclasses-with-original
                               :documentation (documentation class-sym 'type)))))

(defun reset-user-class (class-sym)
  (set-user-class class-sym (gethash class-sym *user-classes*)))

(defun reset-all-user-classes ()
  (mapc #'reset-user-class (alex:hash-table-keys *user-classes*)))

(defun user-class-p (class-specifier)
  (sera:true (gethash (if (symbolp class-specifier)
                          class-specifier
                          (class-name class-specifier))
                      *user-classes*)))

(defmacro with-user-class ((class-sym new-superclasses) &body body) ; TODO: Export if users ever demand it.
  "Dynamically override the superclasses of the user class corresponding to
CLASS-SYM to NEW-SUPERCLASSES.  The class is restored when exiting BODY."
  ;; Test:
  ;; (with-user-class (buffer (buffer))
  ;;   (mopu:direct-superclasses 'user-buffer))
  (let ((user-class (user-class-name class-sym)))
    (unless (user-class-p user-class)
      (error "Argument must be a user class (see `user-class-p')."))
    (let ((old-superclasses (mapcar #'class-name (mopu:direct-superclasses user-class))))
      `(unwind-protect
            (progn
              (set-user-class ',class-sym ',new-superclasses)
              ,@body)
         (set-user-class ',class-sym ',old-superclasses)))))

(-> method-combination-name ((or symbol function)) *)
(defun method-combination-name (fun)
  (let ((fun (if (functionp fun)
                 fun
                 (symbol-function fun))))
    (funcall
     #+sbcl
     'sb-pcl::method-combination-type-name
     #+ccl
     'ccl::method-combination-name
     #-(or sbcl ccl)
     (error "Not implemented")
     (closer-mop:generic-function-method-combination fun))))

(-> standard-method-combination-p ((or symbol function)) boolean)
(defun standard-method-combination-p (fun)
  (eq 'standard
      (method-combination-name fun)))

(export-always '%slot-default%)
(defmacro %define-configuration (name &body slots)
  (let* ((final-name (user-class-name name))
         (temp-name (gentemp (string final-name) (symbol-package name))))
    (dolist (name (list name final-name))
      (unless (find-class name nil)
        (error "define-configuration argument ~a is not a known class." name)))
    `(progn
       (define-class ,temp-name ()
         ,(let ((super-class (closer-mop:ensure-finalized (find-class final-name))))
            (loop for slot in (sera:filter #'standard-method-combination-p (first slots)
                                           :key #'first)
                  for known-slot? = (find (first slot) (mopu:slot-names super-class))
                  for initform = (and known-slot?
                                      (getf (mopu:slot-properties super-class (first slot))
                                            :initform))
                  if known-slot?
                    collect (list (first slot)
                                  :initform `(funcall (lambda (%slot-default%)
                                                        (declare (ignorable %slot-default%))
                                                        ,(cadr slot))
                                                      ,initform))
                  else do
                    (log:warn "Undefined slot ~a in ~a" (first slot) final-name)))
         (:accessor-name-transformer (class*:make-name-transformer name)))
       ;; TODO: Register the user methods and add function to remove them, like
       ;; `reset-user-class'.
       ;; Non-standard accessors, e.g. `default-modes':
       ,@(loop for slot in (remove-if #'standard-method-combination-p (first slots)
                                      :key #'first)
               for slot-name = (first slot)
               collect
               `(defmethod ,slot-name :around ((,(user-class-name name) ,(user-class-name name)))
                  (funcall (lambda (%slot-default%)
                             (declare (ignorable %slot-default%))
                             ,(cadr slot))
                           (call-next-method))))
       (set-user-class ',name ',(cons temp-name
                                      (mapcar #'class-name
                                              (mopu:direct-superclasses final-name)))))))

(defun get-initform (class-symbol class-slot)
  (getf (mopu:slot-properties (find-class class-symbol) class-slot) :initform))

(export-always 'define-configuration)
(defmacro define-configuration (names &body slots)
  "Helper macro to customize the class slots of the NAMES classes.
NAMES is either a symbol or a list of symbols.

Classes can be modes or a one of the user-configurable classes like `browser',
`buffer', `prompt-buffer', `window'.  Note that the classes must _not_ be prefixed
by 'user-'.

The `%slot-default%' variable is replaced by the slot initform.

Example that sets some defaults for all buffers:

\(define-configuration (buffer web-buffer)
  ((status-buffer-height 24)
   (default-modes (append '(vi-normal-mode) %slot-default%))))

Example to get the `blocker-mode' command to use a new default hostlists:

\(define-configuration nyxt/blocker-mode:blocker-mode
  ((nyxt/blocker-mode:hostlists (append (list *my-blocked-hosts*) %slot-default%))))

In the above, `%slot-default%' will be substituted with the return value of
`default-modes'.

In the last example, `nyxt/blocker-mode:user-blocker-mode' is defined to inherit
from the original `blocker-mode' and a generated class containing the
specialized hostlists.

To discover the default value of a slot or all slots of a class, use the
`describe-slot' or `describe-class' commands respectively."
  (if (listp names)
      `(progn
         ,@(mapcar (lambda (name)
                     `(%define-configuration ,name ,@slots))
                   names))
      `(%define-configuration ,names ,@slots)))
