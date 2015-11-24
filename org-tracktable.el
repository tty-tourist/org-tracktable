;;; org-tracktable.el --- Track your writing progress in an org-table

;; Author: tty-tourist <andreasrasholm@protonmail.com>
;; URL: https://github.com/tty-tourist/org-tracktable
;; Created: 2015-11-03
;; Keywords: org, writing
;; Package-Requires: ((emacs "24")(cl-lib "0.5"))
;; Version: 0.1

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

;;; Commentary:

;; The package provides these three interactive functions:

;; - org-tt-insert-table: inserts a table to keep track of word count
;;   in an org-mode buffer.
;; - org-tt-write: adds an entry with the current word count to the
;;   table. You only need to do this when you're done writing for the
;;   day. If an entry for the current day already exists, this entry
;;   will be updated.
;; - org-tt-status: messages the total word count in the buffer, or
;;   region if active. If the table inserted by org-tt-insert-table
;;   exists, the count of words written the current day is shown
;;   together with percentage of your daily writing goal.

;; These three variables can be customized:

;; - org-tt-day-delay: hours after midnight for new day to start.
;; - org-tt-daily-goal: The number of words you pan to write each day.
;; - org-tt-table-name: The name given to the table inserted by
;;   org-tt-insert-table.


;; For additional info on use and customization, see the README in the
;; github repo.

;; Implementation is based on:
;; - Simon Guest's org-wc.el:
;;   https://github.com/dato/org-wc/blob/master/org-wc.el
;; - Lit Wakefield's chronicler.el:
;;   https://github.com/noctuid/chronicler

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'org-table)

(defcustom org-tt-day-delay 5
  "Hours after midnight that are considered part of the previuos day.
Default is 5 which means that a new day is considered to start at 5am."
  :type 'integer :group 'convenience)

(defcustom org-tt-daily-goal 300
  "The number of words plan to write each day.
Your progress in % will be shown with `org-tt-status'.  Set to 0 to
disable 'org-tt-status' from displaying daily goal."
  :type 'integer :group 'convenience)

(defcustom org-tt-table-name "tracktable"
  "The name given to the table inserted by `org-tt-table-insert'.
This is the name that the other functions in the package tries refer to.
If you want to change this variable, it's recommended to do it before
inserting the table, to ensure consistency.  The default name is
'tracktable'."
  :type 'string :group 'convenience)

(defun org-tt-tracktable-exists-p ()
  "Check if the 'tracktable' exists in buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "#\\+NAME:\s*" org-tt-table-name) nil t)))

(defun org-tt-last-entry-today-p ()
  "Check if the last entry in the tracktable was made today."
  (let ((last-entry (substring-no-properties
                     (org-table-get-remote-range org-tt-table-name "@>$2") 1 11))
        (today (format-time-string "%F"
                     (time-subtract (current-time) (seconds-to-time (* 60 60 org-tt-day-delay))))))
    (string= last-entry today)))

(defun org-tt-current-line-empty-p ()
  "Check if point is at an empty line before inserting the table."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun org-tt-written-today ()
  "Calculate words written today.
It does this by substracting last entry that isn't from today from
current word count."
  (let ((current-wc (org-tt-word-count (point-min) (point-max)))
        (last-entry (org-table-get-remote-range org-tt-table-name "@>$4" ))
        (second-last-entry (org-table-get-remote-range org-tt-table-name "@>>$4" )))
    (if (org-tt-last-entry-today-p)
        (- current-wc (string-to-number second-last-entry))
      (- current-wc (string-to-number last-entry)))))

(defun org-tt-current-count ()
  "Reports total number of words in the buffer.
This function is used in the table formula."
   (let ((wc (org-tt-word-count (point-min) (point-max))))
     (format "%d" wc)))

(defun org-tt-stamp ()
    "Make a timestamp for today delayed by `org-tt-day-delay'.
This function is used in the table formula."
    (org-insert-time-stamp
     (time-subtract
      (current-time) (seconds-to-time (* 60 60 org-tt-day-delay))) nil t))

;;;###autoload
(defun org-tt-insert-table ()
  "Insert the a table with the name defined by `org-tt-table-name'."
  (interactive)
  (if (not (org-tt-tracktable-exists-p))
      (progn
        (unless (org-tt-current-line-empty-p) (newline))
        (insert (format "#+NAME: %s
|---+------+-----+-----+-------+---------|
| ! | date | beg | end | total | comment |
|---+------+-----+-----+-------+---------|
|   |      |     |     |       |         |
|---+------+-----+-----+-------+---------|
#+TBLFM: @2$2=initial count::$2='(org-tt-stamp)::@2$3=0::$3=(@-1$4)::$4='(org-tt-current-count)::$5=$4-$3"
                        org-tt-table-name))
        (previous-logical-line)
        (org-table-previous-field)
        (org-table-recalculate))
    (message "Tabel '%s' already exist." org-tt-table-name)))

;;;###autoload
(defun org-tt-status (beg end)
  "Report the number of words between positions BEG and END.
If a table is inserted with `org-tt-table-insert', shows words written today.
If `org-tt-daily-goal' is set to more than 0, show % of daily goal."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (message "%s" (concat (format "%d words in %s. "
                   (org-tt-word-count beg end)
                   (if (use-region-p) "region" "buffer"))
                   (when (org-tt-tracktable-exists-p)
                       (format "%d words written today. " (org-tt-written-today)))
                   (when (and (org-tt-tracktable-exists-p) (< 0 org-tt-daily-goal))
                       (format "%d%s of daily goal."
                               (round (* 100 (/ (org-tt-written-today)
                                                (float org-tt-daily-goal))))
                               "%")))))

;;;###autoload
(defun org-tt-write ()
  "Write progress to the tracktable.
If the last entry is from today, this entry will be updated.
Otherwise a new entry will be made.  It is only necessary to call this function
when you're done writing for the day."
  (interactive)
  (if (org-tt-tracktable-exists-p)
      (let ((tabel (concat "#\\+NAME:\s*" org-tt-table-name)))
        (org-mark-ring-push)
        (goto-char (point-min))
        (re-search-forward tabel nil t)
        (show-subtree)
        (goto-char (org-table-end))
        (previous-logical-line 2)
        (org-table-goto-column 6)
        (if (org-tt-last-entry-today-p)
            (progn (org-table-recalculate)
            (message "Last entry updated. Comments go here. Go back with C-c &."))
           (progn (org-table-next-row)
           (org-table-recalculate)
           (message "New entry added. Comments go here. Go back with C-c &."))))
    (message "Tabel '%s' doesn't exist." org-tt-table-name)))


(defun org-tt-word-count (beg end)
  "Report the number of words between positions BEG and END.
Ignores: heading lines, comments and folded drawers, and any
heading with the tag 'nowc' or 'noexport.'
LaTeX macros are counted as 1 word."
  (let ((wc 0)
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
                 (cl-incf wc)))))))
    wc))

(provide 'org-tracktable)
;;; org-tracktable.el ends here
