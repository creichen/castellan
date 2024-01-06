;;; todo.el --- Castellan activity tracker: TODO management -*- lexical-binding: t -*-

;; Copyright (C) 2023 Christoph Reichenbach

;; Author: Christoph Reichenbach <creichen@creichen.net>
;; Maintainer: Christoph Reichenbach <creichen@creichen.net>
;; Created: 02 Jan 2024

;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (dash "2.0") (compat "29.1"))

;; This file is not part of GNU Emacs.

;; mu4e-tagging is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; mu4e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mu4e.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TBD

;; Requires `dash' and `compat-29'.

;; Terminology:
;; - tags: all the things managed by mu4e-tagging-mode, comprising
;;   - flags: toggles that are independent of each other
;;   - categories: selections that are mutually exclusive.
;;
;; Mails may violate the "categories" restriction and/or include
;; unknown tags; we handle these cases gracefully.
;;
;; - A mail is:
;;   - cagetorised ("categorized" in function/var definitions) iff it
;;     has at least one category
;;   - uncategorised ("uncategorized" in code) otherwise

;;; Code:

(require 'compat-29)
(require 'cal-iso)
(require 'dash)
(require 'org-ql)

;(require 'benchmark)
;(benchmark-elapse (org-ql-select (org-agenda-files)

;; default for new TODO items
(setq castellan-agenda-inbox creichen/org-agenda-inbox-file)
;; TODO buffers
(setq castellan-agenda-todos creichen/org-agenda-todo-files)
;; calendar buffers (only ASSIGNATIONS, i.e., elements that must not be auto-scheduled)
(setq castellan-agenda-calendars creichen/org-agenda-todo-files)

(setq castellan-calendars creichen/org-agenda-calendar-files)

(setq castellan-max-activity-length 8)

(defun castellan--org-ql-action ()
              ;; At each heading, collect the necessary information
              (let* ((headline-raw (org-get-heading))
		     (headline (org-element-headline-parser headline-raw))
		     (level (org-current-level))
                     (buffer (current-buffer))
                     (file-path (buffer-file-name))
                     (position (point))
		     (properties (org-entry-properties)))
                (list :headline-props (cadr headline)
                      :buffer buffer
                      :file file-path
                      :position position
		      :properties properties)
                ))

(defun castellan--get-all-org-items ()
  (org-ql-select (cons castellan-agenda-inbox castellan-agenda-todos)
    t
    :action #'castellan--org-ql-action))

(defun castellan--get-all-calendar-items (&optional today)
  (unless today
    (setq today (datetime-org-today)))
  (-filter (lambda (item) (or (castellan--calendar-item-on-or-after today item :scheduled)
			      (castellan--calendar-item-on-or-after today item :deadline)))
	   (org-ql-select castellan-calendars
	     t
	     :action #'castellan--org-ql-action)
	   ))

(defun castellan--headline-get-activity (headline properties)
  (let ((act (or (alist-get "AGENDA-GROUP" properties nil nil #'string=)
		 (if (string-prefix-p "=" headline) ;; Old-style duration annotations: ignore (temp workaround)
		     nil
		   (let ((split (string-split headline "|")))
		     (when (cdr split)
		       (string-clean-whitespace (car split))))))))
    (when act
      (substring act 0 (min castellan-max-activity-length
			    (length act))))))

(defun castellan--headline-get (headline)
  (string-clean-whitespace
   (let ((split (string-split headline "|")))
     (if (cdr split)
	 ;; there was an activity?  Strip it out
	 (substring headline (+ 1 (length (car split))))
       ;; Keep unchanged
       headline))))

(defun castellan--org-file-strip (filename)
  (file-name-base filename))

(defun datetime<= (datetime-1 datetime-2)
  (let* ((dt1 (datetime-to-time datetime-1))
	 (dt2 (datetime-to-time datetime-2)))
    (or (equal dt1 dt2)
	(time-less-p dt1 dt2))))

(defun datetime-to-time (datetime)
  (apply #'encode-time (mapcar (lambda (x) (or x 0)) datetime)))

(defun datetime-has-time (datetime)
  (and datetime
       (car datetime)
       (cadr datetime)
       (caddr datetime)))

(defun datetime-has-date (datetime)
  (and datetime
       (-let [date (cdddr datetime)]
	 (and
	  (car datetime)
	  (cadr datetime)
	  (caddr datetime)))))

(setq castellan-time-locale "C")

(defun datetime-format (datetime &optional format)
  "Format a DATETIME via FORMAT, as in `format-time-string'.

Formatting will use `castellan-time-locale', if set, instead of
`system-time-locale'."
  (unless format
    (if (datetime-has-time datetime)
	(setq format "%Y-%m-%d %H:%M")
      (setq format "%Y-%m-%d")))
  (if datetime
      (let ((system-time-locale (or castellan-time-locale system-time-locale)))
	(format-time-string format  (datetime-to-time datetime)))
    ""))

(defun castellan-todo--scheduled-p (item)
  (-let [(_ . (&plist :headline headline :scheduled (datetime weekspec))) item]
    (or (datetime-has-date datetime)
	(castellan--weekspec-complete-p weekspec))))

(defun castellan-todo--repeater (item)
  "Returns NIL if ITEM has no repeater or (TYPE UNIT VALUE) if it has.

VALUE is an int, and UNIT can be 'day or 'week."
  (-let [(_ . (&plist :repeater repeater)) item]
    repeater))

(defun castellan-todo--repeater (item)
  "Returns NIL if ITEM has no repeater or (TYPE UNIT VALUE) if it has.

VALUE is an int, and UNIT can be 'day or 'week."
  (-let [(_ . (&plist :repeater repeater)) item]
    repeater))

(defun castellan-todo--schedule-datetime (item)
  (-let [(_ . (&plist :scheduled (datetime weekspec))) item]
    (cond ((datetime-has-date datetime)
	   datetime)
	  ((castellan--weekspec-complete-p weekspec)
	   (castellan--weekspec-to-datetime weekspec)))))

(defun castellan-todo--schedule< (item1 item2)
  ;; move repeaters before normal events
  (if (and (castellan-todo--repeater item1)
	   (not (castellan-todo--repeater item2)))
      t
    (time-less-p (datetime-to-time (castellan-todo--schedule-datetime item1))
		 (datetime-to-time (castellan-todo--schedule-datetime item2)))))

(defun weekdatetime-format (weekdatetime &optional format)
  (-let [(datetime weekspec) weekdatetime]
    (if (datetime-has-date datetime)
	;; priority for datetime
	  (datetime-format datetime format)
      (castellan--weekspec-format weekspec format))))


(defun datetime-week (datetime)
  "For DATETIME in 9-tuple format, compute (WEEK-YEAR WEEK WEEKDAY-KEY WEEKDAY).

9-tuple format expects (SEC MIN HOUR DAY MON YEAR DOW DST TZ).
WEEK is the ISO week number, and WEEK-YEAR is the year that the
ISO week number belongs to.  WEEK-YEAR may be different from YEAR
for the last week of a year-- for example, the 2023-01-01 counts
as Sunday of week 52 of week-year 2022.

WEEKDAY-KEY is a localised order over the weekdays; e.g., Sunday
is 0 in countries where weeks start on Sundays and 8 in countries
where weeks start on Mondays, to simplify ordering.  WEEKDAY is a
weekday number from 0-6 inclusive.  Beware: 0 is Sunday here; to
turn weekdays into strings while avoiding confusion, consider
using `datetime-format'."
  (when datetime
    (-let* (((_ _ _ day month year _ _ _) datetime)
	    ((week weekday year)
	     (calendar-iso-from-absolute
	      ;; calendar-absolute-from-gregorian has a long-standing bug
	      ;; that flips month/day order, so we need to work around that:
	      (calendar-absolute-from-gregorian (list month day year)))))
      (list year
	    week
	    (if (< weekday calendar-week-start-day)
		(+ 7 weekday)
		weekday)
	    weekday))))

(defun datetime-org-today ()
  "Returns org-today in 9-tuple format.

Returns (nil nil nil DAY MON YEAR _ _ _).  The last three values
are currently nil but might change in the future."
  ;; Work around bug in calendar-absolute-from-gregorian:
  (-let [(m d y) (calendar-iso-from-absolute (org-today))]
    (list nil nil nil d m y nil nil nil)))

(defun castellan--weekspec-to-datetime (weekspec)
  (-let* (((week-year week weekday-key weekday) weekspec)
	  ((m d y) (calendar-gregorian-from-absolute
                    (calendar-iso-to-absolute (list week (or weekday 1) week-year)))))
      (list nil nil nil d m y weekday nil nil)))

(defun castellan--weekspec-complete-with-weekday-p (weekspec)
  (and (castellan--weekspec-complete-p weekspec)
       (caddr weekspec)
       (cadddr weekspec)))

(defun castellan--weekspec-complete-p (weekspec)
  (and (car weekspec)
       (cadr weekspec)))

(defun castellan--weekspec-format (weekspec &optional format)
  "Format a WEEKSPEC (Y W WD-KEY WD) to a string using FORMAT.

Allows for WD-KEY and WD to be NIL."
  (unless format
    (if (castellan--weekspec-complete-with-weekday-p weekspec)
	(setq format "%G W%V: %a")
      (setq format "%G W%V")))
  (if (castellan--weekspec-complete-p weekspec)
      (-let [(year week weekday-key weekday) weekspec]
	(datetime-format (castellan--weekspec-to-datetime (list year week weekday-key (or weekday 1)))
			 format))
    ""))

(defun castellan-todo--parse-week (header &optional weekspec-plist)
  (unless weekspec-plist
    (setq weekspec-plist (list nil nil nil nil)))
  (-let* ((header-max-5  (substring header 0 (min 5
						  (length header))))
	  ((default-week-year default-week default-weekday-key default-weekday) weekspec-plist)
	  (weekday  (alist-get
		     header-max-5
		     '(("## mo" . 1)
		       ("## tu" . 2)
		       ("## we" . 3)
		       ("## th" . 4)
		       ("## fr" . 5)
		       ("## sa" . 6)
		       ("## su" . 0))
		     default-weekday
		     nil
		     #'string-equal-ignore-case))
	  (weekday-key (when weekday
			 (if (< weekday calendar-week-start-day)
			      (+ weekday 7)
			    weekday)))
	  (week (or (castellan--extract-prefix-number "# W" header)
		    default-week))
	  (week-year (or (castellan--extract-prefix-number "#" header)
			 default-week-year)))
    (list week-year week weekday-key weekday)))

(defun castellan--week-info (datetime)
  (when datetime
    (list
     (calendar-day-of-week (cdddr datetime))
     (calendar-iso-from-absolute datetime))))

(defun castellan--aggregate-org-items (items)
  (let ((last-file nil)
	(last-level 0)
	(activity nil)
	(prefixes nil)
	(activities-stack nil)
	(weekspec-stack nil)
	(weekspec nil)
	(last-headline nil)
	(marker-pos nil)
	(last-pos nil)
	(last-was-todo nil)
	(results nil))
    (dolist (item items)
      (-let* (((&plist :headline-props headline-props
		       :buffer buffer
		       :file file
		       :position position
		       :properties properties) item)
	      ((&plist :level level
		       :priority priority
		       :todo-type todo-type
		       :todo-keyword todo-status
		       :scheduled (_ (&plist
				      :repeater-type schedule-repeater-type
				      :repeater-unit schedule-repeater-unit
				      :repeater-value schedule-repeater-value))
		       :raw-value unsplit-headline) headline-props)
	      ((&alist "SCHEDULED" scheduled) properties)
	      (headline (castellan--headline-get unsplit-headline))
	      (new-activity (castellan--headline-get-activity unsplit-headline properties)))
	(when scheduled
	  (setq scheduled
		(org-parse-time-string scheduled t)))
	;;(message "scheduled : %s :converted %s :secs %s" scheduled (org-time-string-to-time scheduled) (org-parse-time-string scheduled)))
	(when (not (string= file last-file))
	  (setq last-file file)
	  (setq last-level 0)
	  (setq activity nil)
	  (setq prefixes nil)
	  (setq weekspec nil)
	  (setq marker-pos (list buffer position))
	  (setq weekspec-stack nil)
	  (setq last-headline (castellan--org-file-strip file)))
	(while (> last-level level)
	  ;; level down
	  (setq last-level (- last-level 1))
	  (setq activity (pop activities-stack))
	  (setq weekspec (pop weekspec-stack))
	  (pop prefixes)
	  )
	(while (< last-level level)
	  ;; level up
	  (setq last-level (+ last-level 1))
	  (push activity activities-stack)
	  (push last-headline prefixes)
	  (push weekspec weekspec-stack)
	  (setq last-headline nil)
	  )

	(when new-activity
	  (setq activity new-activity))
	(setq weekspec
	      (castellan-todo--parse-week headline weekspec))

	(setq last-headline headline)
	;; produce output:
	(let* ((castellan-id (cons headline prefixes))
	       (marker (cons 'MARKER (list :castellan-id prefixes :activity activity :level level :pos marker-pos :path prefixes))))
	  (if todo-status
	      (progn
		(push (cons 'ITEM (list :castellan-id castellan-id :headline headline
					:scheduled (list scheduled weekspec)
					:todo-type todo-type ; 'todo or ...
					:todo-status todo-status
					:priority priority
					:activity activity
					:level level
					:pos (list buffer file position)
					:properties properties
					:path prefixes
					:repeater (and schedule-repeater-type
						       schedule-repeater-unit
						       schedule-repeater-value
						       (list schedule-repeater-type schedule-repeater-unit schedule-repeater-value))
					:marker marker))
		      results)
		(setq last-was-todo t))
	    ;; not a TODO item (incl, DONE items)?
	    (progn
	      (setq last-was-todo nil)
	      (setq marker-pos (list buffer position))
	      )))
	;; Finally, retain the previous position
	(setq last-pos (list buffer file position))
	)
      )
    (reverse results)))

(defun castellan--propertize-todo-keyword (keyword)
  (-let [props (alist-get keyword org-todo-keyword-faces nil nil #'string=)]
    (if props
	(propertize keyword 'face props)
      ;; fall back to unpropertized
      keyword
      ))
  )

(defun castellan--propertize-priority (priority)
  (cond
   ((not priority)
    " ")
   ((eq ?A priority)
    (propertize "A" 'face 'castellan-todo-priority-A))
   ((eq ?A priority)
    (propertize "B" 'face 'castellan-todo-priority-B))
   (t (propertize
       (format "%c" priority) 'face 'castellan-todo-priority-C))
  ))


(defun castellan--insert-aggregate-items (items)
  (let ((last-marker nil))
    (dolist (item items)
      (-let* (((type . item-properties) item)
	      ((&plist :castellan-id castellan-id :marker marker :level level :pos pos :path path :activity activity-raw) item-properties)
	      (activity (if activity-raw
			    activity-raw
			  ""))
	      (marker-line (if (not (equal marker last-marker))
			       (propertize (propertize (format "%-8s %s\n" activity (propertize (mapconcat 'identity (reverse path) "/")
										     'face 'castellan-context-info))
						       'castellan
						       marker)
					   'castellan-id
					   (plist-get (cdr marker) :castellan-id))
			     ""))
	      )
	(insert marker-line)
	(setq last-marker marker)
	(-let [next-line
	       (-let* (((&plist :todo-status todo-status :priority priority :scheduled scheduled :properties properties :headline headline) item-properties)
		       (prio (if priority
				 (concat (castellan--propertize-priority priority) ":")
			       ""))
		       (indentation (make-string level ?>)))
		 (format (concat "%-" (format "%d" castellan-max-activity-length) "s  %-16s %9s %2s%s %s\n")
			 activity
			 (propertize (weekdatetime-format scheduled) 'face 'castellan-todo-scheduled-time)
			 (castellan--propertize-todo-keyword todo-status)
			 prio
			 (propertize indentation 'face 'castellan-todo-indentation)
			 (propertize headline 'face 'castellan-todo-headline)))
	       ]
	  (insert (propertize (propertize next-line
					  'castellan item)
			      'castellan-id castellan-id)))
	))))

(defun castellan--insert-aggregate-scheduled-items (items)
  (-let* ((last-year nil)
	  (last-week nil)
	  (last-weekday nil))
    (dolist (item items)
      (-let* (((type . item-properties) item)
	      ((&plist :castellan-id castellan-id :repeater repeater :scheduled scheduled :marker marker :level level :pos pos :path path :activity activity-raw) item-properties)
	      (activity (if activity-raw
			    activity-raw
			  ""))
	      ((year week _ _) (datetime-week (castellan-todo--schedule-datetime item)))
	      (weekday (weekdatetime-format scheduled "%Y-%m-%d  %A")))
	(when repeater
	  (setq year "Recurring Tasks")
	  (setq month nil)
	  (setq day nil))
	(unless (eq last-year year)
	  (insert (propertize
		   (concat " " (propertize (format " %s\n" year) 'face 'castellan-year-header))
		  'castellan-id `(! -YEAR ,year)))
	  (setq last-year year))

	(unless (or repeater
		    (eq last-week week))
	  (insert (propertize
		   (concat " " (propertize (format "  %d                W %02d\n" year week) 'face 'castellan-week-header))
		  'castellan-id `(! -WEEK ,year ,week)))
	  (setq last-week week))

	(unless (or repeater
		    (equal last-weekday weekday))
	  (insert (propertize
		   (concat " " (propertize (format "  %d                W %02d -- %s\n" year week weekday) 'face 'castellan-day-header))
		  'castellan-id `(! -WEEKDAY ,year ,week , weekday)))
	  (setq last-weekday weekday))

	(-let [next-line
	       (-let* (((&plist :todo-status todo-status :priority priority :scheduled scheduled :properties properties :headline headline) item-properties)
		       (prio (if priority
				 (concat (castellan--propertize-priority priority) ":")
			       ""))
		       (indentation (make-string level ?>)))
		 (format (concat "  %-" (format "%d" castellan-max-activity-length) "s  %-5s %9s %2s%s %s\n")
			 activity
			 (propertize (weekdatetime-format scheduled "%H:%M") 'face 'castellan-todo-scheduled-time)
			 (castellan--propertize-todo-keyword todo-status)
			 prio
			 (propertize indentation 'face 'castellan-todo-indentation)
			 (propertize headline 'face 'castellan-todo-headline)))
	       ]
	  (insert (propertize (propertize next-line
					  'castellan item)
			      'castellan-id castellan-id)))
	))))

(defgroup castellan nil
  "Customization group for the 'castellan' package."
  :prefix "castellan-"
  :group 'applications)

(defun castellan--extract-prefix-number (prefix str)
  "Extract number from STR if it starts with PREFIX and is followed by '<number>'."
  (when (string-match (concat prefix "\\([0-9]+\\)") str)
    (string-to-number (match-string 1 str))))

(defun castellan-todo--debug ()
  (interactive)
  (message (format "%s"
		   ;(castellan--week-info (plist-get (castellan-todo--info-at-point) :scheduled))
		   (castellan-todo--info-at-point)
		   ;(get-text-property (point) 'castellan-id)
)))



(defun castellan-todo--info-at-point (&optional required-type buf-point)
  "Returns the plist that describes the item or marker at point.

If REQUIRED-TYPE is not nil, it should be 'ITEM or 'MARKER.
In that case, if the entry at point is not of type
REQUIRED-TYPE, this function returns nil."
  (unless buf-point
    (setq buf-point (point)))
  (-let [(type . prop)  (get-text-property buf-point 'castellan)]
    (when (or (not required-type)
	      (eq required-type type))
      prop)))


(defun castellan-todo-jump-to-item ()
  "Jump to position of the TODO item at point."
  (interactive)
  (-let [(&plist :pos (buffer filename position)) (castellan-todo--info-at-point)]
    (when filename
      (find-file filename)
      (when position
	(goto-char position)))
    ))

(defmacro castellan-todo--in-org-buffer (pos &rest body)
  "Execute BODY in buffer POS"
  `(-let [(buffer filename position) pos]
    (with-current-buffer (find-file filename)
      (goto-char position)
      ,@body
      )
    ))

(defun castellan-todo--all-windows ()
  "Returns a list of all windows that are displaying either of the todo buffers."
  (append (get-buffer-window-list (castellan-todo--buffer))
	  (get-buffer-window-list (castellan-todo--schedule-buffer))))

;; ;; This version assumes that it needs to distinguish windows
;;
;; (defmacro castellan-todo--update (&rest body)
;;   "Execute body and then update the TODO buffer"
;;   `(-let* ((windows (castellan-todo--all-windows))
;; 	   (old-buffer (current-buffer))
;; 	   ;; remember where we were
;; 	   (window-pos-alist (mapcar (lambda (window)
;; 				       (-let [(&plist :castellan-id castellan-id)
;; 					      (with-current-buffer (window-buffer window)
;; 						(castellan-todo--info-at-point nil (window-point window)))]
;; 					 (cons window castellan-id)))
;; 				     windows)))
;;      ,@body
;;      ;; recover position
;;      (castellan-todo--refresh)
;;      (castellan-todo--schedule-refresh)
;;      (dolist (window-castellan-id window-pos-alist)
;;        (-let [(window . castellan-id) window-castellan-id]
;; 	 (message "looking in [%s] for [%s]" window castellan-id)
;; 	 (with-current-buffer  (window-buffer window)
;; 	   (castellan-todo--goto-id castellan-id)
;; 	   (set-window-point window (point)))))
;;      (switch-to-buffer old-buffer)
;;      ))

(defmacro castellan-todo--update (&rest body)
  "Execute body and then update the TODO buffer"
  `(-let* ((old-buffer (current-buffer))
	   ;; remember where we were
	   (buffer-pos-alist (mapcar (lambda (buffer)
				   (-let [(&plist :castellan-id castellan-id)
					  (with-current-buffer buffer
					    (castellan-todo--info-at-point nil (point)))]
				     (cons buffer castellan-id)))
				 (list (castellan-todo--buffer)
				       (castellan-todo--schedule-buffer)))))
     ,@body
     ;; recover position
     (castellan-todo--refresh)
     (castellan-todo--schedule-refresh)
     (dolist (buffer-castellan-id buffer-pos-alist)
       (-let [(buffer . castellan-id) buffer-castellan-id]
	 (switch-to-buffer buffer)
	 (castellan-todo--goto-id castellan-id)
	 (let ((new-point (point)))
	   (dolist (window (get-buffer-window-list buffer))
	     (set-window-point window new-point))
	   )))
     (switch-to-buffer old-buffer)
     ))


(defun castellan-todo--goto-id (id)
  (interactive)
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))  ; While not found and not at end of buffer
      (if (equal id
		 (get-text-property (point) 'castellan-id))
          (progn
	    (setq found t)
	    )
	(forward-line 1))
      )
    (unless found
      (message "Item has disappeared"))))

(defun castellan-todo--item-update (mode)
  (interactive)
  (castellan-todo--update
   (-let* ((info (castellan-todo--info-at-point 'ITEM))
	   ((&plist :pos pos) info))
     (castellan-todo--in-org-buffer pos
				    ;; disable notes
				    (let ((current-prefix-arg 0)) (org-todo mode))
				    ))
   ))

(defun castellan-todo-item-done ()
  (interactive)
  (castellan-todo--item-update "DONE"))

(defun castellan-todo-item-todo ()
  (interactive)
  (castellan-todo--item-update "TODO"))

(defun castellan-todo-item-cancel ()
  (interactive)
  (castellan-todo--item-update "CANCELLED"))

(defvar-keymap castellan-todo-mode-map
  :doc "Keymap for castellan TODO management."
  "RET"           #'castellan-todo-jump-to-item
  "?"             #'castellan-todo--debug
  "+"             #'castellan-todo-item-done
  "-"             #'castellan-todo-item-todo
  "<backspace>"   #'castellan-todo-item-cancel
  )

(define-derived-mode castellan-todo--mode fundamental-mode "castellan:Todo"
  "Castellan activity manager: Integrated TODO mode.

\\{castellan-todo-mode-map}"
  (castellan-todo--mode-setup)
  :mode-class 'special
  :group 'castellan
  :keymap castellan-todo-mode-map
  )

(defun castellan-todo--mode-setup ()
  (hl-line-mode 1)    ; Enable line highlighting
  (use-local-map castellan-todo-mode-map)  ; Activate our keymap
  )

(defun castellan-todo--buffer ()
  (get-buffer-create "*Castellan TODO*"))

(defun castellan-todo--refresh ()
 (let ((buffer (castellan-todo--buffer)))
   (with-current-buffer buffer
     (read-only-mode)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (castellan-todo--mode)
       (castellan--insert-aggregate-items
	(castellan--aggregate-org-items
	 (castellan--get-all-org-items)))
       ))
   buffer))

(defun castellan-todo--schedule-buffer ()
  (get-buffer-create "*Castellan Scheduled TODO*"))

(defun castellan-todo--schedule-refresh ()
 (let ((buffer (castellan-todo--schedule-buffer)))
   (with-current-buffer buffer
     (read-only-mode)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (castellan-todo--mode)
       (castellan--insert-aggregate-scheduled-items
	(sort
	 (-filter
	  #'castellan-todo--scheduled-p
	  (castellan--aggregate-org-items
	   (append
	    (castellan--get-all-calendar-items)
	    (castellan--get-all-org-items))
	   ))
	 #'castellan-todo--schedule<))))
   buffer))

;; NASTY BUT APPARENTLY NECESSARY
(defun org-add-log-note ())


; --------------------------------------------------------------------------------

(defface castellan-todo-headline
  '((t :inherit org-level-3))
  "Face for TODO item headlines in castellan."
  :group 'castellan)

(defface castellan-todo-indentation
  '((t :inherit org-indent))
  "Face for TODO item indentation markers in castellan."
  :group 'castellan)

(defface castellan-todo-effort
  '((t :inherit org-duration))
  "Face for TODO item effort/estimated duration in castellan."
  :group 'castellan)

(defface castellan-todo-priority-A
  '((t :inherit org-upcoming-deadline))
  "Face for TODO item headlines in castellan."
  :group 'castellan)

(defface castellan-todo-priority-B
  '((t :inherit org-distant-deadline))
  "Face for TODO item headlines in castellan."
  :group 'castellan)

(defface castellan-todo-priority-C
  '((t :inherit org-priority))
  "Face for TODO item headlines in castellan."
  :group 'castellan)

(defface castellan-todo-scheduled-time
  '((t :inherit org-scheduled))
  "Face for TODO item scheduled start time in castellan."
  :group 'castellan)

(defface castellan-todo-delegated
  '((t :inherit org-tag))
  "Face for delegated TODO items in castellan."
  :group 'castellan)

(defface castellan-todo-delegatee-name
  '((t :inherit font-lock-function-name-face))
  "Face for the name of the person a task is delegated to in castellan."
  :group 'castellan)

(defface castellan-todo-waiting
  '((t :inherit org-warning))
  "Face for TODO items waiting for someone in castellan."
  :group 'castellan)

(defface castellan-todo-waiting-name
  '((t :inherit font-lock-variable-name-face))
  "Face for the name of the person a task is waiting for in castellan."
  :group 'castellan)

(defface castellan-context-info
  '((t :inherit org-document-info))
  "Face for context information lines in castellan."
  :group 'castellan)

(defface castellan-year-header
  '((t :inherit org-level-1
       :extend t))
  "Face for year separator markers for scheduled TODOs"
  :group 'castellan)

(defface castellan-week-header
  '((t :inherit org-level-2
       :extend t))
  "Face for week separator markers for scheduled TODOs"
  :group 'castellan)

(defface castellan-day-header
  '((t :inherit org-document-info))
  "Face for weekday separator markers for scheduled TODOs"
  :group 'castellan)

;; --------------------------------------------------------------------------------

(defun castellan-todo-find-first ()
  "Jumps to the first incomplete TODO item at or below point.

If there is no such item, moves to the end of the buffer."
  (interactive)
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))  ; While not found and not at end of buffer
      (let ((item (castellan-todo--info-at-point 'ITEM)))
	(message "Considering %s" item)
	(when item
	  (-let [(&plist :todo-type todo-type :repeater repeater) item]
	    (message "  -- repeater is %s" repeater)
	    (message "  -- todo-type is %s" todo-type)
	    (when (and (not repeater)
		     (string= todo-type "todo"))
	      (setq found t)))))
      (forward-line 1))
    )
  )

(defun castellan-todo ()
  "Shows both castellan TODO buffers and switch to the scheduled one."
  (interactive)
  (pop-to-buffer (castellan-todo--refresh))
  (pop-to-buffer (castellan-todo--schedule-refresh))
  (castellan-todo-find-first))