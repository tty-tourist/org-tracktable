;;; org-tracktable.el --- Package for tracking your writing progress in
;; an org-table

;; Author: tty-tourist <andreasrasholm@protonmail.com>
;; URL: https://github.com/tty-tourist/org-tracktable
;; Created: 2015-11-03
;; Package-Requieres: ((emacs "24"))
;; Version 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: For more info on installation and use, see README at:

;; Provides these three interactive functions:

;; - 'org-tt-insert-table' insert a table to track word count.
;; - 'org-tt-write' adds an entry with your progress to the table. You
;;   only need to do this when you're done writing for the dag. If an
;;   entry for the current day already exists, this entry will be
;;   updated.
;; - 'org-tt-status' messages the total word count of the buffer or
;;   region if active. If the table exists, the words amount of words
;;   written today is also shown.

;; For additional info on use and customization, see the README in the
;; github repo.

;; Implementation based on:
;; - Simon Guest's org.el:
;;   https://github.com/dato/org-wc/blob/master/org-wc.el
;; - Lit Wakefield's chronicler.el:
;;   https://github.com/noctuid/chronicler

;;; Code:

(eval-when-compile (require 'org)
                   (require 'cl))

;;;###autoload
(load "org-table.el") ; Some additional functions from this package is used.

(defcustom org-tt-day-delay 5
  "Number of hours after midnight that should be considered part of the
previos day. Default is 5 which means that entry added or updated at 4:59am
will have timestamp for the previous day. Set to 0 to change just after
midnight"
  :type 'integer)

(defcustom org-tt-daily-goal 300
  "The number of words you want to write for the day. Your progress in % will 
be shown with 'org-tt-status'. Set to 0 to disable 'org-tt-status' from
displaying daily goal."
  :type 'integer)

(defcustom org-tt-table-name "tracktable"
  "The name given to the table inserted by 'org-tt-insert'. This is also the
name the the other functions refer to. If you want to change this varibale
it's recommendable to do it before inserting the table to to ensure consistency.
The default name is 'tracktable'."
  :type 'string)

(defun tracktable-exists-p ()
  "Check if the line '#+NAME: tracktable' exists in buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "#\\+NAME:\s*" org-tt-table-name) nil t)))

(defun last-entry-today-p ()
  "Check if the last entry in the tracktable was made today."
  (let ((last-entry (substring-no-properties (org-table-get-remote-range org-tt-table-name "@>$2") 1 11))
        (today (format-time-string "%F" (time-subtract (current-time) (seconds-to-time (* 60 60 org-tt-day-delay))))))
    (string= last-entry today)))

(defun current-line-empty-p ()
  "Used to check if point is at an empty line before inserting the table"
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

;;;###autoload
(defun org-tt-written-today ()
  "Calculate words written today by substracting last entry - or second last
if last entry is today - from current word count."
  (let ((current-wc (org-tt-word-count (point-min) (point-max)))
        (last-entry (org-table-get-remote-range org-tt-table-name "@>$4" ))
        (second-last-entry (org-table-get-remote-range org-tt-table-name "@>>$4" )))
    (if (last-entry-today-p)
        (- current-wc (string-to-number second-last-entry))
      (- current-wc (string-to-number last-entry)))))

(defun org-tt-current-count ()
  "Reports words in buffer. This function is used in the table formula."
   (let ((wc (org-tt-word-count (point-min) (point-max))))
     (format "%d" wc)))

(defun org-tt-stamp ()
    "Makes a timestamp for today minus 4 hours.
This function is used in the table formula."
    (org-insert-time-stamp (time-subtract (current-time) (seconds-to-time (* 60 60 org-tt-day-delay))) nil t))

;;;###autoload
(defun org-tt-insert-table ()
  "Insert the tracktable"
  (interactive)
  (unless (current-line-empty-p) (newline))
  (insert (format "#+NAME: %s
|---+------+-----+-----+---------+---------|
| ! | date | beg | end | written | comment |
|---+------+-----+-----+---------+---------|
|   |      |     |     |         |         |
|---+------+-----+-----+---------+---------|
#+TBLFM: @2$2=initial count::$2='(org-tt-stamp)::@2$3=0::$3=(@-1$4)::$4='(org-tt-current-count)::$5=$4-$3"
                  org-tt-table-name))
  (previous-line)
  (org-table-previous-field)
  (org-table-recalculate))

;;;###autoload
(defun org-tt-status (beg end)
  "Report the number of words in the Org mode buffer or if active.
If the table 'tracktable' exists, show words written today."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (message "%d words in %s. %s %s"
                   (org-tt-word-count beg end)
                   (if (use-region-p) "region" "buffer")
                   (if (tracktable-exists-p)
                       (concat (number-to-string (org-tt-written-today)) " words written today.")
                     "")
                   (if (and (tracktable-exists-p) (< 0 org-tt-daily-goal))
                       (concat (number-to-string (round (* 100 (/ (org-tt-written-today)
                                                          (float org-tt-daily-goal))))) "% of daily goal.")
                     "")))

;;;###autoload
(defun org-tt-write ()
  "Go to the last line of the table 'tracktable'."
  (interactive)
  (if (tracktable-exists-p)
      (let ((tabel (concat "#\\+NAME:\s*" org-tt-table-name)))
        (org-mark-ring-push)
        (goto-char (point-min))
        (re-search-forward tabel nil t)
        (show-subtree)
        (goto-char (org-table-end))
        (previous-line 2)
        (org-table-goto-column 6)
        (if (last-entry-today-p)
            (progn (org-table-recalculate)
            (message "Last entry updated. Comments go here. Go back with C-c &."))
           (progn (org-table-next-row)
           (org-table-recalculate)
           (message "New entry recorded. Comments go here. Go back with C-c &."))))
    (message "Tabel 'tracktable' doesn't exist.")))


;;;###autoload
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
