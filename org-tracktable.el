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

(load "org-table.el") ; Some additional functions from this package is used.

(defun tracktable-exists-p ()
  "Check if the line '#+NAME: tracktable' exists in buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^#\\+NAME: tracktable" nil t)))

(defun last-entry-today-p ()
  "Check if the last entry in the tracktable was made today."
  (let ((last-entry (substring-no-properties (org-table-get-remote-range "tracktable" "@>$2") 1 11))
        (today (format-time-string "%F" (time-subtract (current-time) (seconds-to-time 14400)))))
  (string= last-entry today)))

(defun org-tt-written-today ()
  "Calculate words written today but substractin"
  (let ((wc (org-tt-word-count (point-min) (point-max)))
        (wc-old (org-table-get-remote-range "tracktable" "@>$4" )))
    (number-to-string (- wc (string-to-number wc-old)))))

(defun org-tt-current-count ()
  "Reports words in buffer. This function is used in the table formula."
   (let ((wc (org-tt-word-count (point-min) (point-max))))
     (format "%d" wc)))

(defun org-tt-stamp ()
    "Makes a timestamp for today minus 4 hours.
This function is used in the table formula."
  (org-insert-time-stamp (time-subtract (current-time) (seconds-to-time 14400)) nil t))

(defun org-tt-status (beg end)
  "Report the number of words in the Org mode buffer or if active.
If the table 'tracktable' exists, show words written today."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (message (format "%d words in %s. %s"
                   (org-tt-word-count beg end)
                   (if (use-region-p) "region" "buffer")
                   (if (tracktable-exists-p)
                       (concat (org-tt-written-today) " words written today.")
                     ""))))

(defun org-tt-goto-table ()
  "Go to the last line of the table 'tracktable'."
  (interactive)
  (if (tracktable-exists-p)
      (let ((tabel "^#\\+NAME: tracktable"))
        (goto-char (point-min))
        (re-search-forward tabel nil t)
        (show-subtree)
        (goto-char (org-table-end))
        (previous-line 2)
        (org-table-goto-column 6)
        (org-table-next-row))
    (message "Tabel 'tracktable' racktable doesn't exist")))

(defun org-tt-write ()
  "Go to the last line of the table 'tracktable'."
  (interactive)
  (if (tracktable-exists-p)
      (let ((tabel "^#\\+NAME: tracktable"))
        (org-mark-ring-push)
        (goto-char (point-min))
        (re-search-forward tabel nil t)
        (show-subtree)
        (goto-char (org-table-end))
        (previous-line 2)
        (org-table-goto-column 6)
        (if (last-entry-today-p)
            (progn (org-table-recalculate)
            (message "Last entry updated. Enter comment or C-c & to go back."))
           (progn (org-table-next-row)
           (org-table-recalculate)
           (message "New entry recorded. Enter comment or C-c & to go back"))))
    (message "Tabel 'tracktable' doesn't exist.")))

(defun org-tt-word-count (beg end)
  "Report the number of words in the selected region.
Ignores: heading lines, blocks, comments and drawers.
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
         ((org-at-heading-p) ; org-wc-in-heading-line
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
