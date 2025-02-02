;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class socket-file (files:runtime-file nyxt-file)
  ((files:base-path #p"nyxt.socket")
   (editable-p nil))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Socket files are typically stored in a dedicated directory."))

(defmethod files:resolve ((profile nyxt-profile) (socket socket-file))
  "Return finalized path for socket files."
  (if (or (getf *options* :no-socket)
          (multiple-value-bind (option-found? path) (get-properties *options* '(:socket))
            (and option-found? (uiop:emptyp path))))
      #p""
      (uiop:ensure-pathname (or (getf *options* :socket) (call-next-method))
                            :truenamize t)))

(export-always '*socket-file*)
(defvar *socket-file* (make-instance 'socket-file)
  "Path of the Unix socket used to communicate between different instances of
Nyxt.

If `files:expand' resolves this to #p\"\", then Nyxt starts in multi-instance mode.
This means that re-running Nyxt will start a new instance of Nyxt instead of
prompting the first instance.

This path cannot be set from the configuration file because we want to be able
to set and use the socket without parsing any file.  Instead, the socket can be
set from the corresponding command line option.")

(defun handle-malformed-cli-arg (condition)
  (format t "Error parsing argument ~a: ~a.~&" (opts:option condition) condition)
  (opts:describe)
  (uiop:quit 0))

(sera:eval-always
  (defun define-opts ()
    "Define command line options.
This must be called on startup so that code is executed in the user environment
and not the build environment."
    (opts:define-opts
      (:name :help
       :description "Print this help and exit."
       :short #\h
       :long "help")
      (:name :verbose
       :short #\v
       :long "verbose"
       :description "Print debugging information to stdout.")
      (:name :version
       :long "version"
       :description "Print version and exit.")
      (:name :system-information
       :long "system-information"
       :description "Print system information and exit.")
      (:name :config
       :short #\i
       :long "config"
       :arg-parser #'identity
       :description (format nil "Set path to configuration file.
Default: ~s" (files:expand *config-file*)))
      (:name :no-config
       :short #\I
       :long "no-config"
       :description "Do not load the configuration file.")
      (:name :auto-config
       :short #\c
       :long "auto-config"
       :arg-parser #'identity
       :description (format nil "Set path to auto-configuration file.
Default: ~s" (files:expand *auto-config-file*)))
      (:name :no-auto-config
       :short #\C
       :long "no-auto-config"
       :description "Do not load the user auto-configuration file.")
      (:name :socket
       :short #\s
       :long "socket"
       :arg-parser #'identity
       :description "Set path to socket.
Unless evaluating remotely (see --remote), Nyxt starts in single-instance mode when a socket is set.")
      (:name :no-socket
       :short #\S
       :long "no-socket"
       :description "Do not use any socket.")
      (:name :eval
       :short #\e
       :long "eval"
       :arg-parser #'identity
       :description "Eval the Lisp expressions.  Can be specified multiple times.
Without --quit or --remote, the evaluation is done after parsing the config file
(if any) and before initializing the browser.")
      (:name :load
       :short #\l
       :long "load"
       :arg-parser #'identity
       :description "Load the Lisp file.  Can be specified multiple times.
Without --quit or --remote, the loading is done after parsing the config file
(if any) and before initializing the browser.")
      (:name :quit
       :short #\q
       :long "quit"
       :description "Quit after --load or --eval.")
      (:name :script
       :long "script"
       :arg-parser #'identity
       :description "Load the Lisp file (skip #! line if any), skip config file, then exit.
Set to '-' to read standard input instead.")
      (:name :remote
       :short #\r
       :long "remote"
       :description "Send the --eval and --load arguments to the running instance of Nyxt.
Unless --quit is specified, also send s-expressions from the standard input.
The remote instance must be listening on a socket which you can specify with --socket
and have the `remote-execution-p' browser slot to non-nil.")
      (:name :headless
       :long "headless"
       :description "Start Nyxt without showing any graphical element.
This is useful to run scripts for instance."))))
;; Also define command line options at read-time because we parse
;; `opts::*options*' in `start'.
(sera:eval-always (define-opts))

(define-command quit (&optional (code 0))
  "Quit Nyxt."
  (let ((*quitting-nyxt-p* t))
    (when (slot-value *browser* 'ready-p)
      (hooks:run-hook (before-exit-hook *browser*))
      ;; Unready browser:
      ;; - after the hook, so that on hook error the browser is still ready;
      ;; - before the rest, so to avoid nested `quit' calls.
      (setf (slot-value *browser* 'ready-p) nil)
      (setf (slot-value *browser* 'exit-code) code)
      (mapcar #'ffi-window-delete (window-list))
      (when (socket-thread *browser*)
        (destroy-thread* (socket-thread *browser*))
        ;; Warning: Don't attempt to remove socket-path if socket-thread was not
        ;; running or we risk remove an unrelated file.
        (let ((socket (files:expand *socket-file*)))
          (when (uiop:file-exists-p socket)
            (log:info "Deleting socket ~s." socket)
            (uiop:delete-file-if-exists socket))))
      (ffi-kill-browser *browser*)
      ;; Reset global state.
      (setf *browser* nil
            *options* nil)
      (uninstall *renderer*)
      (unless *run-from-repl-p*
        (run-thread "force-quitter"
          ;; Force-quit in case `ffi-kill-browser' hangs.  Must be run in a
          ;; separate thread because the renderer loop is waiting for this
          ;; function to finish.
          (sleep 1)
          (uiop:quit code))))))

(define-command quit-after-clearing-session (&key confirmation-p) ; TODO: Rename?
  "Close all buffers then quit Nyxt."
  (delete-all-buffers :confirmation-p confirmation-p)
  (quit))

(define-command start-swank (&optional (swank-port *swank-port*))
  "Start a Swank server enabling connecting to Emacs via SLIME.

Warning: This allows Nyxt to be controlled remotely, that is, to
execute arbitrary code with the privileges of the user running Nyxt.
Make sure you understand the security risks associated with this
before running this command."
  (swank:create-server :port swank-port :dont-close t)
  (echo "Swank server started at port ~a" swank-port))

(define-command start-slynk (&optional (slynk-port *slynk-port*))
  "Start a Slynk server enabling connecting to Emacs via SLY.

Warning: This allows Nyxt to be controlled remotely, that is, to
execute arbitrary code with the privileges of the user running Nyxt.
Make sure you understand the security risks associated with this
before running this command."
  (slynk:create-server :port slynk-port :dont-close t)
  (echo "Slynk server started at port ~a" slynk-port))

;; From sbcl/src/code/load.lisp
(defun maybe-skip-shebang-line (stream)
  (let ((p (file-position stream)))
    (when p
      (flet ((next () (read-byte stream nil)))
        (unwind-protect
             (when (and (eq (next) (char-code #\#))
                        (eq (next) (char-code #\!)))
               (setf p nil)
               (loop for x = (next)
                     until (or (not x) (eq x (char-code #\newline)))))
          (when p
            (file-position stream p)))))))

(cffi:defcallback handle-interrupt
    :void ((signum :int) (siginfo :pointer) (ptr :pointer))
  (declare (ignore signum siginfo ptr))
  (quit))

(export-always 'entry-point)
(defun entry-point ()
  "Read the CLI arguments and start the browser.
This is the entry point of the binary program.
Don't run this from a REPL, prefer `start' instead."
  (define-opts)
  (multiple-value-bind (options free-args)
      (handler-bind ((opts:unknown-option #'handle-malformed-cli-arg)
                     (opts:missing-arg #'handle-malformed-cli-arg)
                     (opts:arg-parser-failed #'handle-malformed-cli-arg))
        (opts:get-opts))
    (setf *run-from-repl-p* nil)             ; Not a REPL.
    (let ((interrupt-sigaction (cffi:foreign-alloc '(:struct isys::sigaction))))
      ;; Mimics https://www.systutorials.com/catching-the-signal-sent-by-kill-in-c-on-linux/
      (isys:memset interrupt-sigaction 0 (cffi:foreign-type-size '(:struct isys::sigaction)))
      (setf (cffi:foreign-slot-value
             interrupt-sigaction '(:struct isys:sigaction) 'isys::sigaction)
            (cffi:callback handle-interrupt))
      (setf (cffi:foreign-slot-value
             interrupt-sigaction '(:struct isys:sigaction) 'isys::flags)
            isys:sa-siginfo)
      (isys:sigaction isys:sigint interrupt-sigaction (cffi:null-pointer))
      (isys:sigaction isys:sigterm interrupt-sigaction (cffi:null-pointer)))
    (apply #'start (append options (list :urls free-args)))))

(defun eval-expr (expr)
  "Evaluate the form EXPR (string) and print the result of the last expression."
  (with-input-from-string (input expr)
    (let ((*package* (find-package :nyxt-user)))
      (flet ((eval-protect (s-exp)
               (with-protect ("Error in s-exp evaluation: ~a" :condition)
                 (eval s-exp))))
        (let* ((sexps (safe-slurp-stream-forms input))
               (but-last (butlast sexps))
               (last (alex:last-elt sexps)))
          (mapc #'eval-protect but-last)
          (format t "~&~a~&" (eval-protect last)))))))

(defun parse-urls (expr)
  "Do _not_ evaluate EXPR and try to open URLs that were send to it.
EXPR is expected to be as per the expression sent in `listen-or-query-socket'."
  (let ((urls (ignore-errors (rest (read-from-string expr nil)))))
    (if (and urls (every #'stringp urls))
        (apply #'open-external-urls urls)
        (progn
          (log:warn "Could not extract URLs from ~s." expr)
          nil))))

(export-always 'open-external-urls)
(-> open-external-urls (&rest string) *)
(defun open-external-urls (&rest url-strings)
  "Open URL-STRINGS on the renderer thread and return URLs.
This is a convenience wrapper to make remote code execution to open URLs as
short as possible.
It takes URL-STRINGS so that the URL argument can be `cl-read' in case
`remote-execution-p' is nil."
  (let ((urls (remove-if #'url-empty-p (mapcar #'url url-strings))))
    (if urls
        (log:info "Externally requested URL(s): ~{~a~^, ~}" urls)
        (log:info "Externally pinged."))
    (ffi-within-renderer-thread (lambda () (open-urls urls)))
    urls))

(defun listen-socket ()
  (files:with-paths ((socket-path *socket-file*))
    (let ((native-socket-path (uiop:native-namestring socket-path)))
      (ensure-directories-exist socket-path :mode #o700)
      ;; TODO: Catch error against race conditions?
      (iolib:with-open-socket (s :address-family :local
                                 :connect :passive
                                 :local-filename native-socket-path)
        (isys:chmod native-socket-path #o600)
        (log:info "Listening to socket: ~s" socket-path)
        (loop as connection = (iolib:accept-connection s)
              while connection
              do (progn
                   (when-let ((expr (alex:read-stream-content-into-string connection)))
                     (unless (uiop:emptyp expr)
                       (if (remote-execution-p *browser*)
                           (progn
                             (log:info "External evaluation request: ~s" expr)
                             (eval-expr expr))
                           (progn
                             (parse-urls expr)
                             ;; It's customary to focus the browser window when
                             ;; opening a URL (but not when evaluating Lisp
                             ;; code).
                             ;; If we get pinged too early, we do not have a current-window yet.
                             (when (current-window)
                               (ffi-window-to-foreground (current-window)))))))))))))

(defun listening-socket-p ()
  (ignore-errors
   (iolib:with-open-socket (s :address-family :local
                              :remote-filename (uiop:native-namestring
                                                (files:expand *socket-file*)))
     (iolib:socket-connected-p s))))

(-> listen-or-query-socket ((or null (cons quri:uri *))) *)
(defun listen-or-query-socket (urls)
  "If another Nyxt is listening on the socket, tell it to open URLS.
Otherwise bind socket and return the listening thread."
  (let ((socket-path (files:expand *socket-file*)))
    (cond
      ((listening-socket-p)
       (if urls
           (progn
             (log:info "Nyxt already started, requesting to open URL(s): ~{~a~^, ~}" urls)
             (iolib:with-open-socket (s :address-family :local
                                        :remote-filename (uiop:native-namestring socket-path))
               ;; Can't use `render-url' at this point because the GTK loop is not running.
               (format s "~s" `(open-external-urls ,@(mapcar #'quri:render-uri urls)))))
           (log:info "Nyxt already started."))
       nil)
      (t
       (uiop:delete-file-if-exists socket-path) ; Safe since socket-path is a :socket at this point.
       (run-thread "socket listener"
         (listen-socket))))))

(defun remote-eval (expr)
  "If another Nyxt is listening on the socket, tell it to evaluate EXPR."
  (if (listening-socket-p)
      (iolib:with-open-socket (s :address-family :local
                                 :remote-filename (uiop:native-namestring
                                                   (files:expand *socket-file*)))
        (write-string expr s))
      (progn
        (log:info "No instance running.")
        (uiop:quit 0))))

(sera:eval-always
  (defvar %start-args (mapcar (compose #'intern
                                       #'symbol-name
                                       #'opts::name)
                              opts::*options*)))
(export-always 'start)
(defun start #.(append '(&rest options &key urls) %start-args)
  #.(format nil "Parse command line or REPL options then start the browser.
Load URLS if any (a list of strings).

This function focuses on OPTIONS parsing.  For the actual startup procedure, see
`start-browser'.

The OPTIONS are the same as the command line options.

~a"
            (with-output-to-string (s) (opts:describe :stream s)))
  (declare #.(cons 'ignorable %start-args))
  (unless *renderer*
    (log:warn "No renderer set, Nyxt will not be able to render pages.  Try:

\(progn
 (asdf:load-system :nyxt/gi-gtk)
 (nyxt:ffi-initialize nyxt:*browser* '() (time:now)))
"))
  ;; Nyxt extensions should be made accessible straight from the beginning,
  ;; e.g. before a script is run.
  (pushnew 'nyxt-source-registry asdf:*default-source-registries*)
  (asdf:clear-configuration)
  (let ((source-directory (files:expand *source-directory*)))
    (if (uiop:directory-exists-p source-directory)
        (set-nyxt-source-location source-directory)
        (log:debug "Nyxt source directory not found.")))

  ;; Initialize the lparallel kernel
  (initialize-lparallel-kernel)

  ;; Options should be accessible anytime, even when run from the REPL.
  (setf *options* options)
  (destructuring-bind (&key (headless *headless-p*) verbose help version
                         system-information script load eval quit remote
                       &allow-other-keys)
      options
    (setf *headless-p* headless)

    (if verbose
        (progn
          (log:config :debug)
          (format t "Arguments parsed: ~a and ~a~&" options urls))
        (log:config :pattern *log-pattern*))

    (cond
      (help
       (opts:describe :prefix "nyxt [options] [URLs]"))

      (version
       (format t "Nyxt version ~a~&" +version+))

      (system-information
       (princ (system-information)))

      (script
       (setf *run-from-repl-p* t)       ; To report errors.
       (flet ((run-script (stream)
                (maybe-skip-shebang-line stream)
                (load-lisp stream :package (find-package :nyxt-user))))
         (match (getf options :script)
           ("-" (run-script *standard-input*))
           (file (with-open-file (f file :element-type :default)
                   (run-script f))))))

      ((or remote
           (and (or load eval) quit))
       (start-load-or-eval))
      (t
       (with-protect ("Error: ~a" :condition)
         (start-browser urls))))
    (unless *run-from-repl-p* (uiop:quit 0))))

(defun load-or-eval (&key remote)
  (when remote
    (log:info "Probing remote instance listening to ~a." (files:expand *socket-file*)))
  (loop for (opt value . nil) on *options*
        do (match opt
             (:load (let ((value (uiop:truename* value)))
                      ;; Absolute path is necessary since remote process may have
                      ;; a different working directory.
                      (if remote
                          (remote-eval (format nil "~s" `(load-lisp ,value)))
                          (load-lisp value))))
             (:eval (if remote
                        (remote-eval value)
                        (eval-expr value)))))
  (when (and remote
             (not (getf *options* :quit)))
    (log:info "Reading s-expressions from standard input until end of input (Ctrl-D on *nix).")
    ;; We do not use `safe-read' because this is controlled by the user anyways.
    ;; TODO: `read' may fail, e.g. when a package prefix is not known, but the
    ;; input would still be valid if the remote instance knows the package.
    (handler-case (loop for sexp = (read)
                        do (remote-eval (write-to-string sexp)))
      (end-of-file ()
        (log:info "Quitting interpreter."))))
  (when remote
    (uiop:quit 0)))

(defun start-load-or-eval ()
  "Evaluate Lisp.
The evaluation may happen on its own instance or on an already running instance."
  (let ((remote (getf *options* :remote)))
    (unless remote
      (load-lisp (files:expand *auto-config-file*) :package (find-package :nyxt-user))
      (load-lisp (files:expand *config-file*) :package (find-package :nyxt-user)))
    (load-or-eval :remote remote)))

(defun start-browser (url-strings)
  "Start Nyxt.
First load `*auto-config-file*' if any.
Then load `*config-file*' if any.
Instantiate `*browser*'.
Finally, run the browser, load URL-STRINGS if any, then run
`after-init-hook'."
  (restart-case
      (progn
        (when *browser*
          (error 'browser-already-started
                 :message "Another global browser instance is already running."))
        (let ((log-path (files:expand *log-file*)))
          (unless (files:nil-pathname-p log-path)
            (uiop:delete-file-if-exists log-path) ; Otherwise `log4cl' appends.
            ;; REVIEW: Do we want to back up the log?
            (log:config :backup nil :pattern *log-pattern* :daily log-path)))
        (format t "Nyxt version ~a~&" +version+)
        (log:info "Source location: ~s" (files:expand *source-directory*))

        (install *renderer*)

        (let* ((urls (remove-if #'url-empty-p (mapcar #'url url-strings)))
               (startup-timestamp (time:now))
               (startup-error-reporter nil))
          (if (or (getf *options* :no-socket)
                  (null (files:expand *socket-file*))
                  (not (listening-socket-p)))
              (progn
                (load-lisp (files:expand *auto-config-file*)
                           :package (find-package :nyxt-user))
                (multiple-value-bind (condition backtrace)
                    (load-lisp (files:expand *config-file*)
                               :package (find-package :nyxt-user))
                  (when backtrace
                    (setf startup-error-reporter
                          (lambda ()
                            (echo-warning "~a" condition)
                            (error-in-new-window "Configuration file errors"
                                                 (princ-to-string condition)
                                                 backtrace)))))
                (load-or-eval :remote nil)
                (setf *browser*
                      (make-instance
                       'browser
                       :startup-error-reporter-function startup-error-reporter
                       :startup-timestamp startup-timestamp
                       :socket-thread (unless (nfiles:nil-pathname-p (files:expand *socket-file*))
                                        (listen-or-query-socket urls))))
                ;; Defaulting to :nyxt-user is convenient when evaluating code
                ;; (such as remote execution or the integrated REPL).
                ;; This must be done in a separate thread because the calling
                ;; thread may have set `*package*' as an initial-binding (see
                ;; `bt:make-thread'), as is the case with the SLY mrepl thread.
                (bt:make-thread (lambda () (in-package :nyxt-user)))
                (ffi-initialize *browser* urls startup-timestamp)
                (lpara:force (slot-value *browser* 'startup-promise)))
              (listen-or-query-socket urls))))
    (quit ()
      :report "Run `nyxt:quit' and try again."
      (quit)
      (start-browser url-strings))
    (force-quit ()
      :report "Run `nyxt:quit' and set `*browser*' to NIL in any case."
      (ignore-errors (quit))
      (setf *browser* nil)
      (start-browser url-strings))))

(defun restart-with-message (&key condition backtrace)
  (flet ((set-error-message (condition backtrace)
           (let ((*package* (find-package :cl))) ; Switch package to use non-nicknamed packages.
             (write-to-string
              `(hooks:add-hook
                (nyxt:after-init-hook nyxt:*browser*)
                (make-instance
                 'hooks:handler
                 :fn (lambda ()
                       (setf (nyxt::startup-error-reporter-function *browser*)
                             (lambda ()
                               (nyxt:echo-warning "Restarted without configuration file due to error: ~a"
                                                  ,(princ-to-string condition))
                               (nyxt::error-in-new-window "Initialization error"
                                                          ,(princ-to-string condition)
                                                          ,backtrace))))
                 :name 'error-reporter))))))
    (log:warn "Restarting with ~s."
              (append (uiop:raw-command-line-arguments) '("--no-config"
                                                          "--no-auto-config")))
    (uiop:launch-program (append (uiop:raw-command-line-arguments)
                                 `("--no-config"
                                   "--no-auto-config"
                                   "--eval"
                                   ,(set-error-message condition backtrace))))
    (quit 1)))

(define-command nyxt-init-time ()
  "Return the duration of Nyxt initialization."
  (echo "~,2f seconds" (slot-value *browser* 'init-time)))
