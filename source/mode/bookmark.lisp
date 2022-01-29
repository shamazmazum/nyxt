;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package nyxt/bookmark-mode
  (:use #:cl #:nyxt)
  (:import-from #:serapeum #:export-always)
  (:documentation "Bookmark buffer mode and command"))
(in-package :nyxt/bookmark-mode)

(export-always 'bookmark-mode)
(define-mode bookmark-mode ()
  "Mode for the bookmarks buffer."
  ((rememberable-p nil)
   (style (theme:themed-css (theme *browser*)
            (".collapsible"
             :background-color theme:secondary
             :color            theme:background
             :cursor           "pointer"
             :padding          "18px"
             :width            "100%"
             :border           "none"
             :text-align       "left"
             :outline          "none"
             :font-size        "15px")
            (".collapsible:hover"
             :background-color theme:primary)
            (".content"
             :padding          "0 18px"
             :display          "none"
             :overflow         "hidden")))))

(defun group-bookmarks (buffer)
  (let ((bookmarks-table (make-hash-table :test #'equalp)))
    (with-data-unsafe (bookmarks (bookmarks-path buffer))
      (dolist (bookmark bookmarks)
        (let ((tags (tags bookmark)))
          (if tags
              (dolist (tag tags)
                (push bookmark (gethash tag bookmarks-table nil)))
              (push bookmark (gethash tags bookmarks-table nil))))))
    bookmarks-table))

(export-always 'list-bookmarks)
(define-internal-page-command-global list-bookmarks ()
    (bookmarks-buffer "*Bookmarks*" 'bookmark-mode)
  "List all bookmarks in a new buffer."
  (let ((bookmarks (group-bookmarks bookmarks-buffer)))
    (spinneret:with-html-string
      (:style (style (find-mode bookmarks-buffer 'bookmark-mode)))
      (:h1 "Bookmarks")
      (:body
       (if (zerop (hash-table-count bookmarks))
           (format nil "No bookmarks in ~s." (expand-path (bookmarks-path bookmarks-buffer)))
           (maphash
            (lambda (tag bookmarks)
              (:button :class "collapsible"
                       :onclick
                       (ps:ps
                         (let* ((collapsible (ps:chain document active-element))
                                (content (ps:chain collapsible next-element-sibling)))
                           (setf (ps:chain content style display)
                                 (if (= (ps:chain content style display) "block")
                                     "none" "block"))
                           (values)))
                       (or tag "Unsorted"))
              (:div :class "content"
                    (dolist (bookmark bookmarks)
                      (let ((url-display (render-url (url bookmark)))
                            (url-href (render-url (url bookmark))))
                        (:div :class "bookmark-entry"
                              (:p (:b "Title: ") (title bookmark))
                              (:p (:b "URL: ") (:a :href url-href
                                                   url-display))
                              (:p (:b "Tags: ")
                                  (when (tags bookmark)
                                    (format nil " (~{~a~^, ~})" (tags bookmark))))
                              (:p (:button :class "button"
                                           :onclick
                                           (ps:ps
                                             (let ((section (ps:chain document active-element
                                                                      (closest ".bookmark-entry"))))
                                               (ps:chain section parent-node (remove-child section))
                                               (nyxt/ps:lisp-eval
                                                `(nyxt::delete-bookmark ,url-href))))
                                           "Delete"))
                              (:hr ""))))))
            bookmarks))))))
