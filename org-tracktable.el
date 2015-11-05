;;; org-tracktable.el --- Track your writing progress in an org-table

;; Author: tty-tourist
;; E-mail: andreasrasholm@protonmail.com
;; Created: 2015-11-03

;; test

;;; Commentary:

;; Inserts a table to track word count. With 'org-tt-status' you can get
;; current status. With 'org-tt-write' you can writ to table.

;; The code from for the actual word count is taking from Simon Guest's
;; org.el (https://github.com/dato/org-wc/blob/master/org-wc.el)

;; Some of the code to display status is taken from Lit Wakefield's
;; chronicler.el (https://github.com/noctuid/chronicler)

;;; Code:

(eval-when-compile (require 'org)
                   (require 'cl))

(defun org-wc-in-heading-line ()
  "Is point in a line starting with `*'?"
  (equal (char-after (point-at-bol)) ?*))

(defun org-tt-count (beg end)
  "Report the number of words in the Org mode buffer or selected region."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (message (format "%d words in %s."
                   (org-tt-word-count beg end)
                   (if (use-region-p) "region" "buffer"))))

(defun org-tt-status ()
  "Report the number of words in the Org mode buffer or selected region."
  (interactive)
  (let ((wc (org-tt-word-count (point-min) (point-max)))
        (wc-old (org-table-get-remote-range "stats" "@>$4" )))
    (message (concat (format "%d" wc) " words total. "
                     (number-to-string (- wc (string-to-number wc-old))) " written today."))))

(defun org-tt-current ()
  "Reports the number of words in the Org mode buffer."
  (interactive)
   (let ((wc (org-tt-word-count (point-min) (point-max))))
     (format "%d" wc)))

(defun org-tt-insert ()
  "Inserts the number of words in the Org mode buffer or selected region."
  (interactive)
   (let ((wc (org-tt-word-count (point-min) (point-max))))
     (insert (format "%d" wc))))

(defun org-tt-word-count (beg end)
  "Report the number of words in the selected region.
Ignores: heading lines,
         blocks,
         comments,
         drawers.
LaTeX macros are counted as 1 word."

  (let ((wc 0)
        (block-begin-re "^#\\\+BEGIN")
        (block-end-re "^#\\+END")
        (latex-macro-regexp "\\\\[A-Za-z]+\\(\\[[^]]*\\]\\|\\){\\([^}]*\\)}"))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (cond
         ;; Ignore heading lines, and sections tagged 'nowc' or 'noexport'.
         ((org-wc-in-heading-line)
          (let ((tags (org-get-tags-at)))
            (if (or (member "nowc" tags)
                    (member "noexport" tags))
                (outline-next-heading)
              (forward-line))))
         ;; Ignore blocks.
         ((looking-at block-begin-re)
          (re-search-forward block-end-re))
         ;; Ignore comments.
         ((org-at-comment-p)
          (forward-line))
         ;; Ignore drawers.
         ((org-at-drawer-p)
          (forward-line))
         ;; Count latex macros as 1 word, ignoring their arguments.
         ((save-excursion
            (backward-char)
            (looking-at latex-macro-regexp))
          (goto-char (match-end 0))
          (setf wc (+ 2 wc)))
         (t
          (progn
            (and (re-search-forward "\\w+\\W*" end 'skip)
                 (incf wc)))))))
    wc))

(provide 'org-tracktable)
;;; org-tracktable.el ends here
