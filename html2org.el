;; -*- lexical-binding: t; -*-
(require 'dom)
(require 'shr)
(require 'subr-x)

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

(defun html2org-fontize-dom (dom type)
  (unless (or (looking-back "[[:blank:]]")
              (save-excursion
                (= (point) (progn (beginning-of-line)
                                  (point)))))
    (insert " "))
  (insert type)
  (shr-generic dom)
  (insert type)
  (unless (looking-at-p "[[:blank:]]")
    (insert " ")))

(defun html2org-tag-b (dom)
  (html2org-fontize-dom dom "*"))

(defun html2org-tag-i (dom)
  (html2org-fontize-dom dom "/"))

(defun html2org-tag-em (dom)
  (html2org-fontize-dom dom "/"))

(defun html2org-tag-strong (dom)
  (html2org-fontize-dom dom "*"))

(defun html2org-tag-u (dom)
  (html2org-fontize-dom dom "_"))

(defun html2org-transform-dom (dom)
  "Transform DOM into org file content."
  (let ((shr-external-rendering-functions '((a . html2org-tag-a)
                                            (b . html2org-tag-b)
                                            (i . html2org-tag-i)
                                            (em . html2org-tag-em)
                                            (strong . html2org-tag-strong)
                                            (u . html2org-tag-u))))
    (with-temp-buffer
      (shr-insert-document dom)
      (replace-regexp-in-string "^\\(\\*[[:blank:]]+\\)" ",\\1" (buffer-string)))))



(defun html2org (&optional buf)
  "Convert HTML to org text in the BUF"
  (interactive)
  (let ((buf (or buf (current-buffer)))
        (shr-external-rendering-functions '((a . html2org-tag-a))))
    (with-current-buffer buf
      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
        (erase-buffer)
        (insert (html2org-transform-dom dom))))))


(provide 'html2org)
