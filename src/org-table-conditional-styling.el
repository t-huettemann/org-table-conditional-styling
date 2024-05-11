;;; org-table-conditional-styling.el --- Conditional formatting for org-mode table cells -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Thomas Huettemann
;;
;; Author: Thomas Huettemann <thuette@>
;; Maintainer: Thomas Huettemann <thuette@>
;; Created: May 11, 2024
;; Modified: May 11, 2024
;; Version: 0.1.0
;; Keywords: emacs org-mode table elisp
;; Homepage: https://github.com/t-huettemann/org-table-conditional-styling
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Conditional formatting for org-mode table cells.
;;
;;  Add background and foreground colours for cells matching regular expressions.
;;  Also allows for "computed" font attributes, for example based on numerical content of table cells.
;;
;;  See the Github README for full instructions.
;;
;;; Code:
(require 'org-table)

;;;###autoload
(defun org-table-conditional-styling (&optional d)
  (interactive)
  (save-excursion
    (when (org-at-table-p)
      (org-table-goto-line 1)
      (org-table-goto-column 1)
      (org-table-analyze)
      (let* (
             (cols org-table-current-ncol)
             (rows (1- (length org-table-dlines)))
	     (table          (org-element-property  :parent (org-element-at-point)))
	     (tbeg           (org-element-property  :begin table))
	     (tend           (org-element-property  :end   table))
	     (bg-rules       (read (concat "(" (org-export-read-attribute :attr_cs table :bg) ")")))
	     (fg-rules       (read (concat "(" (org-export-read-attribute :attr_cs table :fg) ")")))
	     (custom-rules   (read (concat "(" (org-export-read-attribute :attr_cs table :custom) ")")))
	     (striped        (org-export-read-attribute :attr_cs table :striped))
	     ;; read the "computed" rules, and add code for "stiped" table if requested
	     (computed-rules (car
			      (read-from-string
			       (format "(%s %s)"
				     (when striped "\"(when (cl-evenp row) '(:background \\\"azure2\\\"))\"")
				     (org-export-read-attribute :attr_cs table :computed)
				     )
			       )
			      )
			     )
	     (list-of-rules (list bg-rules fg-rules custom-rules))
	     )

	;; remove old overlays
	(remove-overlays tbeg tend 'conditional-styling t)

	;; now loop over the cells in the table
	;; and apply the styles as prescribed
	(cl-loop
	 for row in (number-sequence 1 rows)
	 do
	 (cl-loop
	  for col in (number-sequence 1 cols)
	  do
	  (org-table-goto-line row)
	  (org-table-goto-column col)
	  (when-let* (
		      (cell  (org-table-get row col))
		      (start (1- (point)))
		      (nsep  (search-forward "|" nil t))
		      (end   (1- (point)))
		      (face  (org-table-conditional-styling--compute-cell-style
			      row col cell
			      list-of-rules
			      computed-rules))
		      (over  (make-overlay start end))
		      )
	    (overlay-put over 'face face)
	    (overlay-put over 'conditional-styling t)
	    )
	  )
	 )
	)
      )
    nil
    )
  )


;;;###autoload
(defun org-table-conditional-styling--compute-cell-style (row col cell list-of-rules computed-rules)
  "Apply the regexps for :fg and:bg, return first match in each case."
  (append
   (cl-loop
    for rule in computed-rules
    append
    (funcall
     (eval (car(read-from-string (concat "(lambda (row col cell)" rule ")"))))
     row col cell
     ) into computed
    finally return computed
    )
   (cl-loop
    for rule in (nth 0 list-of-rules)
    when (org-table-conditional-styling--is-applicable
	  row  (nth 3 rule)
	  col  (nth 2 rule)
	  cell (nth 0 rule)
	  ) return (list :background (nth 1 rule))
    finally return nil
    )
   (cl-loop
    for rule in (nth 1 list-of-rules)
    when (org-table-conditional-styling--is-applicable
	  row  (nth 3 rule)
	  col  (nth 2 rule)
	  cell (nth 0 rule)
	  ) return (list :foreground (nth 1 rule))
    finally return nil
    )
   (cl-loop
    for rule in (nth 2 list-of-rules)
    append
    (when (org-table-conditional-styling--is-applicable
	   row  (nth 3 rule)
	   col  (nth 2 rule)
	   cell (nth 0 rule)
	   )
      (nth 1 rule)
      ) into custom
    finally return custom
    )
   )
  )


;;;###autoload
(defun org-table-conditional-styling--is-applicable (row rowres col colres cell regexp)
  (when
      (and
       (or
	(null colres)
	(and (listp colres) (member col colres))
	(and (integerp colres) (= col colres))
	)
       (or
	(null rowres)
	(and (listp rowres) (member row rowres))
	(and (integerp rowres) (= row rowres))
	)
       (cond
	((not regexp) (string= cell ""))
	((eq regexp t) (not (string= cell "")))
	(t (string-match regexp cell))
	)
       )
    t
    )
  )
  

(advice-add #'org-table-align         :after #'org-table-conditional-styling)
(advice-add #'org-table-insert-row    :after #'org-table-conditional-styling)
(advice-add #'org-table-kill-row      :after #'org-table-conditional-styling)
(advice-add #'org-table-insert-column :after #'org-table-conditional-styling)
(advice-add #'org-table-delete-column :after #'org-table-conditional-styling)

(provide 'org-table-conditional-styling)

;;; org-table-conditional-styling.el ends here
