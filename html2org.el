;; -*- lexical-binding: t; -*-
(require 'dom)
(require 'shr)
(require 'subr-x)

(defgroup html2org nil
  "Save http(s) page as org file")

(defcustom html2org-store-dir "/home/lujun9972/github/emacs-document/raw/"
  "The directory to store org files"
  :type 'directory)

(defcustom html2org-timeout 30
  "Timeout seconds"
  :type 'number)

(defun html2org-get-dom (&optional url)
  "Retrive `URL' and return the dom."
  (let* ((url (or url (read-string "url: ")))
         (buf (with-timeout
                  (html2org-timeout
                   (error "fetch %s failed in %d seconds" url html2org-timeout))
                (url-retrieve-synchronously url))))
    (prog1 (ignore-errors (with-current-buffer buf
                            (goto-char url-http-end-of-headers)
                            (libxml-parse-html-region (point) (point-max) url t)))
      (kill-buffer buf))))

(defun html2org-tag-a (dom)
  "Convert DOM into org-mode style link."
  (let ((url (dom-attr dom 'href))
        (title (dom-attr dom 'title))
        (text (dom-texts dom))
        (start (point)))
    (when (and shr-target-id
               (equal (dom-attr dom 'name) shr-target-id))
      ;; We have a zero-length <a name="foo"> element, so just
      ;; insert...  something.
      (when (= start (point))
        (shr-ensure-newline)
        (insert " "))
      (put-text-property start (1+ start) 'shr-target-id shr-target-id))
    (let ((description (or title text)))
      (if (string-empty-p (string-trim description))
          (insert (format "[[%s]]" url))
        (insert (format "[[%s][%s]]" url description))))))

(defun html2org-transform-dom (dom)
  "Transform DOM into org file content."
  (let ((shr-external-rendering-functions '((a . html2org-tag-a))))
    (with-temp-buffer
      (shr-insert-document dom)
      (replace-regexp-in-string "^\\*" ",*" (buffer-string)))))

;; (with-current-buffer "*eww*"
;;   (html2org (eww-current-url) nil :TITLE "title" :TAGS "tag1 tags"))

;;;###autoload
(defun html2org (&optional url org-file &rest option-plist)
  "Retrive URL and write the content into ORG-FILE with org-mode style links.

This function will return the saved ORG-FILE path"
  (interactive)
  (let* ((url (or url (read-string "url: ")))
         (dom (html2org-get-dom url))
         (title (dom-text (dom-by-tag dom 'title)))
         (org-file (or org-file (expand-file-name (concat title ".org") html2org-store-dir)))
         (content (html2org-transform-dom dom)))
    (with-temp-file org-file
      (while option-plist
        (let ((option (format "%s" (pop option-plist)))
              (value (format "%s" (pop option-plist))))
          (when (string-prefix-p ":" option)
            (setq option (substring option 1))) ;handler format like :option
          (insert (format "#+%s: %s\n" option value))))
      (insert content))
    org-file))

(provide 'html2org)
