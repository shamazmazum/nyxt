(in-package :nyxt/web-mode)

;;;; commands for navigating/editing input fields on HTML pages

(define-parenscript active-input-area-content ()
  (ps:chain document active-element value))

(define-parenscript set-active-input-area-content (content)
  (setf (ps:chain document active-element value) (ps:lisp content)))

(define-parenscript active-input-area-cursor ()
  (ps:chain document active-element selection-start))

(define-parenscript set-active-input-area-cursor (selection-start
                                                  selection-end)
  (ps:chain document active-element (set-selection-range
                                     (ps:lisp selection-start)
                                     (ps:lisp selection-end))))

(define-command cursor-forwards ()
  "Move cursor forward by one element."
  (with-result* ((cursor-position (active-input-area-cursor)))
    (let ((new-position (+ (parse-integer cursor-position) 1)))
      (set-active-input-area-cursor new-position new-position))))

(define-command cursor-backwards ()
  "Move cursor backwards by one element."
  (with-result* ((cursor-position (active-input-area-cursor)))
    (let ((new-position (- (parse-integer cursor-position) 1)))
      (set-active-input-area-cursor new-position new-position))))

(define-command cursor-forwards-word ()
  "Move cursor forwards a word."
  (with-result* ((contents (active-input-area-content))
                 (cursor-position (active-input-area-cursor)))
    (let ((text-buffer (make-instance 'text-buffer:text-buffer))
          (cursor (make-instance 'text-buffer:cursor)))
      (cluffer:attach-cursor cursor text-buffer)
      (text-buffer::insert-string cursor contents)
      (setf (cluffer:cursor-position cursor)
            (parse-integer cursor-position))
      (text-buffer::move-forward-word cursor)
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-command cursor-backwards-word ()
  "Move cursor backwards a word."
  (with-result* ((contents (active-input-area-content))
                 (cursor-position (active-input-area-cursor)))
    (let ((text-buffer (make-instance 'text-buffer:text-buffer))
          (cursor (make-instance 'text-buffer:cursor)))
      (cluffer:attach-cursor cursor text-buffer)
      (text-buffer::insert-string cursor contents)
      (setf (cluffer:cursor-position cursor)
            (parse-integer cursor-position))
      (text-buffer::move-backward-word cursor)
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-command delete-forwards ()
  "Delete character after cursor."
  (with-result* ((contents (active-input-area-content))
                 (cursor-position (active-input-area-cursor)))
    (let ((text-buffer (make-instance 'text-buffer:text-buffer))
          (cursor (make-instance 'text-buffer:cursor)))
      (cluffer:attach-cursor cursor text-buffer)
      (text-buffer::insert-string cursor contents)
      (setf (cluffer:cursor-position cursor)
            (parse-integer cursor-position))
      (text-buffer::delete-item-forward cursor)
      (set-active-input-area-content
       (text-buffer::string-representation text-buffer))
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-command delete-backwards ()
  "Delete character before cursor."
  (with-result* ((contents (active-input-area-content))
                 (cursor-position (active-input-area-cursor)))
    (let ((text-buffer (make-instance 'text-buffer:text-buffer))
          (cursor (make-instance 'text-buffer:cursor)))
      (cluffer:attach-cursor cursor text-buffer)
      (text-buffer::insert-string cursor contents)
      (setf (cluffer:cursor-position cursor)
            (parse-integer cursor-position))
      (text-buffer::delete-item-backward cursor)
      (set-active-input-area-content
       (text-buffer::string-representation text-buffer))
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-command delete-backwards-word ()
  "Delete backwards a word."
  (with-result* ((contents (active-input-area-content))
                 (cursor-position (active-input-area-cursor)))
    (let ((text-buffer (make-instance 'text-buffer:text-buffer))
          (cursor (make-instance 'text-buffer:cursor)))
      (cluffer:attach-cursor cursor text-buffer)
      (text-buffer::insert-string cursor contents)
      (setf (cluffer:cursor-position cursor)
            (parse-integer cursor-position))
      (text-buffer::delete-backward-word cursor)
      (set-active-input-area-content
       (text-buffer::string-representation text-buffer))
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-command delete-forwards-word ()
  "Delete forwards a word."
  (with-result* ((contents (active-input-area-content))
                 (cursor-position (active-input-area-cursor)))
    (let ((text-buffer (make-instance 'text-buffer:text-buffer))
          (cursor (make-instance 'text-buffer:cursor)))
      (cluffer:attach-cursor cursor text-buffer)
      (text-buffer::insert-string cursor contents)
      (setf (cluffer:cursor-position cursor)
            (parse-integer cursor-position))
      (text-buffer::delete-forward-word cursor)
      (set-active-input-area-content
       (text-buffer::string-representation text-buffer))
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))
