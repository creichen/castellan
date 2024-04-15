;;; castellan.el --- Castellan activity tracker: TODO management -*- lexical-binding: t -*-

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

;; Special automatic scheduling format:
;; * 2023
;; ** # W01
;; *** ## Mon
;; **** TODO =12:00 | lunch at noon
;; *** ## Tuesday


;;; Code:

(require 'compat-29)
(require 'cal-iso)
(require 'dash)
(require 'org-ql)

;(require 'benchmark)
;(benchmark-elapse (org-ql-select (org-agenda-files)

(defgroup castellan nil
  "Customization group for the 'castellan' package."
  :prefix "castellan-"
  :group 'applications)

(defcustom castellan-agenda-inbox nil
  "Name of a buffer that stores the org TODO inbox"
  :type 'string
  ;; :set setter-function
  :group 'castellan)

(defcustom castellan-agenda-todos nil
  "List of names of files that contain TODO items"
  :type '(repeat string)
  ;; :set setter-function
  :group 'castellan)

(defcustom castellan-agenda-calendars nil
  "List of names of files that contain calendar items.

Items in these files won't show up in the (regular) TODO, only in
the Scheduled TODO.  Intended for schedules that might be
auto-generated and spammy."
  :type '(repeat string)
  ;; :set setter-function
  :group 'castellan)


(defcustom castellan-max-activity-length 8
  "Maximum string length to allow for activity names"
  :type 'number
  ;; :set setter-function
  :group 'castellan)

(defvar castellan-current-activity nil
  "Current castellan activity, or nil.")

(defvar castellan-all-activities nil
  "List of all known activities, for auto-completion.")

(defvar castellan-time-locale "C"
  "Locale for printing weekday names")

(defvar castellan-agenda-auto-revert nil
  "Whether to mark castellan agenda buffers as  auto-revert")


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

(defun castellan--all-agenda-todos ()
  "All agenda files from castellan-agenda-todos plus castellan-agenda-inbox, if non-nil."
    (append (when castellan-agenda-inbox (list castellan-agenda-inbox))
	    castellan-agenda-todos)
  )

(defun castellan--get-all-org-items ()
  (org-ql-select (castellan--all-agenda-todos)
    t
    :action #'castellan--org-ql-action))

(defun castellan--calendar-item-on-or-after (today item &optional selector)
  (unless selector
    (setq :scheduled selector))
  (-let [(_ (&plist :year-start year-start
		    :month-start month-start
		    :day-start day-start
		    :year-end year-end
		    :month-end month-end
		    :day-end day-end))
	 (plist-get (plist-get item :headline-props) selector)]
    (let* ((start (list 0 0 0 day-start month-start year-start nil nil nil))
	   (end   (list 0 0 0 day-end   month-end   year-end   nil nil nil)))
      (and (datetime-has-date start)
	   ;; hasn't started yet or is starting just today?
	   (or (datetime<= today start)
	       ;; or hasn't ended yet?
	       (and (datetime-has-date end)
		    (datetime<= today end)))))))

(defun castellan--get-all-calendar-items (&optional today)
  (when castellan-agenda-calendars
    (unless today
      (setq today (datetime-org-today)))
    (-filter (lambda (item) (or (castellan--calendar-item-on-or-after today item :scheduled)
				(castellan--calendar-item-on-or-after today item :deadline)))
	     (org-ql-select castellan-agenda-calendars
	       t
	       :action #'castellan--org-ql-action)
	     )))

(defun castellan--headline-get-activity-or-time-string (headline properties)
  "Given a HEADLINE with a '|', extract the activity or time string
from the bar's lhs.

Returns NIL if not present, otherwise a pair whose car is
'activity or 'time-string."
  (let ((act (or (alist-get "AGENDA-GROUP" properties nil nil #'string=)
		 (if (string-prefix-p "=" headline) ;; quick-schedule annotations
		     (cons 'time-string (substring headline 1))
		   (let ((split (string-split headline "|")))
		     (when (cdr split)
		       (string-clean-whitespace (car split))))))))
    (if (stringp act)
      (cons 'activity (substring act 0 (min castellan-max-activity-length
					    (length act))))
      ;; else: time string
      act)))

(defun castellan--activity< (item1 item2)
  (let* ((act1 (castellan--activity item1))
	 (act2 (castellan--activity item2)))
    (and (equal act1 castellan-current-activity)
	 (not (equal act2 castellan-current-activity)))))

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
	  (car date)
	  (cadr date)
	  (caddr date)))))

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

(defun castellan--scheduled-p (item)
  (-let [(_ . (&plist :headline headline :scheduled (datetime weekspec))) item]
    (or (datetime-has-date datetime)
	(castellan--weekspec-complete-p weekspec))))

(defun castellan--activity (item)
  "Returns the activity that ITEM belongs to, or NIL."
  (-let [(_ . (&plist :activity activity)) item]
    activity))

(defun castellan--repeater (item)
  "Returns NIL if ITEM has no repeater or (TYPE UNIT VALUE) if it has.

VALUE is an int, and UNIT can be 'day or 'week."
  (-let [(_ . (&plist :repeater repeater)) item]
    repeater))

(defun castellan--repeater (item)
  "Returns NIL if ITEM has no repeater or (TYPE UNIT VALUE) if it has.

VALUE is an int, and UNIT can be 'day or 'week."
  (-let [(_ . (&plist :repeater repeater)) item]
    repeater))

(defun castellan--schedule-datetime (item)
  "The 9-tuple datetime value for ITEM's scheduled time.

Combines the internal datetime and weekspec specifications as
appropriate."
  (-let [(_ . (&plist :scheduled (datetime weekspec))) item]
    (cond ((datetime-has-date datetime)
	   datetime)
	  ((castellan--weekspec-complete-p weekspec)
	   (let ((week-datetime (castellan--weekspec-to-datetime weekspec)))
	     (if (datetime-has-time datetime)
		 (append (take 3 datetime) (cdddr week-datetime))
	       week-datetime))))))

(defun castellan--schedule< (item1 item2)
  ;; move repeaters before normal events
  (if (and (castellan--repeater item1)
	   (not (castellan--repeater item2)))
      t
    (let* ((datetime1 (castellan--schedule-datetime item1))
	   (datetime2 (castellan--schedule-datetime item2))
	   ;; items w/o time last
	   (datetime1-mod (if (datetime-has-time datetime1) datetime1
			    (append '(0 0 25) (cdddr datetime1))))
	   (datetime2-mod (if (datetime-has-time datetime2) datetime2
			    (append '(0 0 25) (cdddr datetime2)))))
	(time-less-p (datetime-to-time datetime1-mod)
		     (datetime-to-time datetime2-mod)))))

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
  ;; calendar-gregorian-from-absolute: work around bug that swaps month and day
  (-let [(m d y) (calendar-gregorian-from-absolute (org-today))]
    (list nil nil nil d m y nil nil nil)))

(defun castellan--weekspec-to-datetime (weekspec)
  (-let* (((week-year week weekday-key weekday) weekspec)
	  ;; calendar-gregorian-from-absolute: work around bug that swaps month and day
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

(defun castellan--parse-week (header &optional weekspec-plist)
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


(defun datetime-parse-time (time-string &optional default)
  "Parse TIME-STRING into 9-tuple format.

If DEFUALT is set, use values from there to fill in any missing
parts of the result."
  (unless default
    (setq default (list nil nil nil nil nil nil nil nil nil)))
  (let ((result (parse-time-string time-string)))
    (mapcar (lambda (p) (or (car p) (cadr p)))
	    (-zip-lists result default))))


(defun castellan--week-info (datetime)
  (when datetime
    (list
     (calendar-day-of-week (cdddr datetime))
     (calendar-iso-from-absolute datetime))))

;; ================================================================================
;; Automatic updating

(defcustom castellan-auto-update-idle-timer-delay 2
  "Time in seconds after tracked file is updated before updating Castellan view.
   Should be greater than zero to avoid spurious refreshes when synchronising files."
  :type 'number
  :group 'castellan)

(setq castellan--auto-update-idle-refresh-timer nil)

(defun castellan--auto-update-idle-refresh-run ()
  (castellan-refresh)
  (setq castellan--auto-update-idle-refresh-timer nil))

(defun castellan--auto-update-request-idle-refresh ()
  (unless castellan--auto-update-idle-refresh-timer
    (setq castellan--auto-update-idle-refresh-timer
	  (run-with-idle-timer castellan-auto-update-idle-timer-delay nil
			       #'castellan--auto-update-idle-refresh-run))))

(defun castellan--auto-update-check (&rest args)
  "Callback for hooks that report updates on tracked org files."
  (when (castellan--auto-update-track-current-buffer-p)
    (castellan--auto-update-request-idle-refresh)))

(defun castellan--auto-update-track-current-buffer-p ()
  "Checks if hte current buffer should be tracked for automatic updates"
  (and (local-variable-p 'castellan--track-this-file)
       castellan--track-this-file))

(defun castellan--auto-update-setup ()
  (add-hook 'after-revert-hook #'castellan--auto-update-check)
  (add-hook 'after-change-functions #'castellan--auto-update-check)
  ;; Iterate over all buffers-- implicitly tags for update tracking
  (dolist (buf (castellan--all-org-buffers))))

;; ================================================================================
;; org item processing prior to visualisation

(defun castellan--aggregate-org-items (items)
  (let ((last-file nil)
	(last-buffer nil)
	(last-level 0)
	(prefixes nil)
	(ctx-stack '(:activity nil :weekspec nil))
	(ctx-weekspec nil)
	(ctx-activity nil)
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
	      (activity-or-time-string (castellan--headline-get-activity-or-time-string unsplit-headline properties))
	      (new-ctx-activity (when (eq 'activity (car activity-or-time-string))
			      (cdr activity-or-time-string)))
	      (scheduled-timespec (when (eq 'time-string (car activity-or-time-string))
				    (cdr activity-or-time-string))))
	(when scheduled
	  (setq scheduled
		(org-parse-time-string scheduled t)))
	(when scheduled-timespec
	  (setq scheduled
		(datetime-parse-time scheduled-timespec scheduled))
	  (unless (or (datetime-has-date scheduled)
		      (datetime-has-time scheduled))
	    (setq scheduled nil)))
	(when (not (and (string= file last-file)
			(eq buffer last-buffer)))
	  (setq last-file file)
	  (setq last-buffer buffer)
	  (setq last-level 0)

	  (setq ctx-activity nil)
	  (setq ctx-weekspec nil)
	  (setq ctx-stack '(:activity nil :weekspec nil))

	  (setq prefixes nil)
	  (setq marker-pos (list buffer file position))
	  (setq last-headline (if file
				  (castellan--org-file-strip file)
				"<no-file-name>")))
	(cond
	 ((= last-level level)
	  ;; same level
	  (progn
	    (-let [(:activity a :weekspec ws) (car ctx-stack)]
	      (setq ctx-activity a)
	      (setq ctx-weekspec ws))))
	 ((> last-level level)
	  ;; level down
	  (while (> last-level level)
	    (setq last-level (- last-level 1))
	    (pop ctx-stack)
	    (-let [(:activity a :weekspec ws) (car ctx-stack)]
	      (setq ctx-activity a)
	      (setq ctx-weekspec ws))
	    (pop prefixes)))
	 ((< last-level level)
	  ;; level up
	  (while (< last-level level)
	    (setq last-level (+ last-level 1))
	    (push (list :activity ctx-activity
			:weekspec ctx-weekspec)
		  ctx-stack)
	    (push last-headline prefixes)
	    (setq last-headline nil))))

	(when new-ctx-activity
	  (unless (or (equal new-ctx-activity ctx-activity)
		      (member new-ctx-activity castellan-all-activities))
	    (push new-ctx-activity castellan-all-activities))
	  (setq ctx-activity new-ctx-activity))
	(setq ctx-weekspec
	      (castellan--parse-week headline ctx-weekspec))

	(setq last-headline headline)
	;; produce output:
	(let* ((castellan-id (cons headline prefixes))
	       (marker (cons 'MARKER (list :castellan-id prefixes :activity ctx-activity :level level :pos marker-pos :path prefixes))))
	  (if todo-status
	      (progn
		(push (cons 'ITEM (list :castellan-id castellan-id :headline headline
					:scheduled (list scheduled ctx-weekspec)
					:todo-type todo-type ; 'todo or ...
					:todo-status todo-status
					:priority priority
					:activity ctx-activity
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
    (propertize "A" 'face 'castellan-priority-A))
   ((eq ?A priority)
    (propertize "B" 'face 'castellan-priority-B))
   (t (propertize
       (format "%c" priority) 'face 'castellan-priority-C))
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
			 (propertize (weekdatetime-format scheduled) 'face 'castellan-scheduled-time)
			 (castellan--propertize-todo-keyword todo-status)
			 prio
			 (propertize indentation 'face 'castellan-indentation)
			 (propertize headline 'face 'castellan-headline)))
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
	      ((&plist :castellan-id castellan-id :repeater repeater :marker marker :level level :pos pos :path path :activity activity-raw) item-properties)
	      (activity (if activity-raw
			    activity-raw
			  ""))
	      (scheduled (castellan--schedule-datetime item))
	      ((year week _ _) (datetime-week scheduled))
	      (weekday (datetime-format scheduled "%Y-%m-%d  %A")))
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
	       (-let* (((&plist :todo-status todo-status :priority priority :properties properties :headline headline) item-properties)
		       (prio (if priority
				 (concat (castellan--propertize-priority priority) ":")
			       ""))
		       (indentation (make-string level ?>)))
		 (format (concat "  %-" (format "%d" castellan-max-activity-length) "s  %-5s %9s %2s%s %s\n")
			 activity
			 (if (datetime-has-time scheduled)
			     (propertize (datetime-format scheduled "%H:%M") 'face 'castellan-scheduled-time)
			   (propertize "--:--" 'face 'castellan-context-info))
			 (castellan--propertize-todo-keyword todo-status)
			 prio
			 (propertize indentation 'face 'castellan-indentation)
			 (propertize headline 'face 'castellan-headline)))
	       ]
	  (insert (propertize (propertize next-line
					  'castellan item)
			      'castellan-id castellan-id)))
	))))

(defun castellan--extract-prefix-number (prefix str)
  "Extract number from STR if it starts with PREFIX and is followed by '<number>'."
  (when (string-match (concat prefix "\\([0-9]+\\)") str)
    (string-to-number (match-string 1 str))))

(defun castellan--debug ()
  (interactive)
  (message (format "%s"
		   ;(castellan--week-info (plist-get (castellan--info-at-point) :scheduled))
		   (castellan--info-at-point)
		   ;(get-text-property (point) 'castellan-id)
)))

(defun castellan--find-visiting-create (file-or-buf)
  "Gets the buffer for an org file, creating it if necessary.

Marks the buffer for update tracking."
  (-let [buf (if (and (bufferp file-or-buf)
		      (buffer-live-p file-or-buf))
		 file-or-buf
	       (or (find-buffer-visiting file-or-buf)
		   (find-file-noselect file-or-buf)))]
    (with-current-buffer buf
      (setq-local castellan--track-this-file t))
    buf
    ))

(defun castellan--info-at-point (&optional required-type buf-point)
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

(defconst castellan--quick-view-buffer-name "*Castellan Quick View*"
  "Quick-view buffer for Castellan")

(defvar castellan--current-quick-view-item nil
  "Curent quick view item")

(defun castellan-show-item ()
  "Show the item at point in a temp buffer"
  (interactive)
  (-if-let* ((cinfo (castellan--info-at-point))
	       ((&plist :pos (buffer filename position)) cinfo)
	       (node-body (save-excursion
			    (set-buffer (castellan--find-visiting-create filename))
			    (goto-char position)
			    (org-get-entry)))
	       (_ filename)
	       (_ position)
	       (_ (not (eq cinfo castellan--current-quick-view-item)))
	       (temp-buffer (get-buffer-create castellan--quick-view-buffer-name)))
      (progn
	(setq castellan--current-quick-view-item cinfo)
	(with-temp-buffer-window temp-buffer
	    nil
	    nil
	  (with-current-buffer temp-buffer
	    (insert node-body)
	    (org-mode)
	    ;; make configurable?
	    (local-set-key (kbd "q") #'kill-buffer-and-window)
	    (add-hook 'kill-buffer-hook (lambda ()
					  (setq castellan--current-quick-view-item nil)))
	    (org-show-subtree)
	    (org-cycle-hide-drawers 'all))))
    ;; else close the info window
    (-when-let (window (get-buffer-window castellan--quick-view-buffer-name))
      (setq castellan--current-quick-view-item nil)
      (delete-window window))))

(defun castellan-jump-to-item ()
  "Jump to position of the TODO item at point."
  (interactive)
  (-let [(&plist :pos (buffer filename position)) (castellan--info-at-point)]
    (when filename
      ;;(find-file filename)
      (let ((filebuf (find-file-noselect filename)))
	(select-window (display-buffer filebuf))
	(when position
	  (goto-char position)))
    )))

(defmacro castellan--in-org-buffer (pos &rest body)
  "Execute BODY in buffer POS"
  `(-let [(buffer filename position) pos]
     (with-current-buffer (or (and (buffer-live-p buffer)
				   buffer)
			      (castellan--find-visiting-create filename))
       (goto-char position)
       ,@body
       )
     ))

(defun castellan--all-windows ()
  "Returns a list of all windows that are displaying either of the todo buffers."
  (append (get-buffer-window-list (castellan--activity-agenda-buffer))
	  (get-buffer-window-list (castellan--schedule-agenda-buffer))))

(defmacro castellan--update (refresh &rest body)
  "Execute BODY and then refresh the agenda buffers.

If REFRESH is 'activity or 'schedule, then only the corresponding
buffer will be updated."
  `(-let* ((old-buffer (current-buffer))
	   ;; remember where we were
	   (buffer-pos-alist (mapcar (lambda (buffer)
				   (-let [(&plist :castellan-id castellan-id)
					  (with-current-buffer buffer
					    (castellan--info-at-point nil (point)))]
				     (cons buffer castellan-id)))
				 (list (castellan--activity-agenda-buffer)
				       (castellan--schedule-agenda-buffer)))))
     ,@body
     ;; recover position
     (unless (eq ,refresh 'schedule)
       (castellan--activity-refresh))
     (unless (eq ,refresh 'activity)
       (castellan--schedule-refresh))
     (dolist (buffer-castellan-id buffer-pos-alist)
       (-let [(buffer . castellan-id) buffer-castellan-id]
	 (switch-to-buffer buffer)
	 (castellan--goto-id castellan-id)
	 (let ((new-point (point)))
	   (dolist (window (get-buffer-window-list buffer))
	     (set-window-point window new-point))
	   )))
     (switch-to-buffer old-buffer)
     ))

(defun castellan--id (&rest pos)
  (-let [point (if pos
		   (car pos)
		 (point))]
    (get-text-property point 'castellan-id)))

(defun castellan--goto-id (id)
  "Returns nil on failure, otherwise point"
  (interactive)
  (goto-char (point-min))
  (when id
    (let ((found nil))
      (while (and (not found) (not (eobp)))  ; While not found and not at end of buffer
	(if (equal id
		   (get-text-property (point) 'castellan-id))
            (progn
	      (setq found t))
	  (forward-line 1)))
      (if found
	  (progn
	    (point))
	  ; else
	(progn (message "Item has disappeared")
	       nil)))))

(defun castellan--item-update (mode)
  (interactive)
  (castellan--update t
   (-let* ((info (castellan--info-at-point 'ITEM))
	   ((&plist :pos pos) info))
     (castellan--in-org-buffer pos
			       ;; disable notes
			       (let ((current-prefix-arg 0)) (org-todo mode))
			       ))
   ))

(defun castellan-set-activity (new-activity)
  "Sets the current focus activity for the activity agenda"
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Activity: " (cons "" castellan-all-activities)))))
  (unless (equal new-activity castellan-current-activity)
    (setq castellan-current-activity (unless (string= "" new-activity)
				       new-activity))
    (with-current-buffer (castellan--activity-refresh)
      (castellan-find-first))))

(defun castellan-item-done ()
  "Mark TODO item at point as DONE"
  (interactive)
  (castellan--item-update "DONE"))

(defun castellan-item-todo ()
  "Mark TODO item at point as TODO"
  (interactive)
  (castellan--item-update "TODO"))

(defun castellan-item-cancel ()
  "Mark TODO item at point as CANCELLED"
  (interactive)
  (castellan--item-update "CANCELLED"))

(defun castellan-refresh ()
  "Refresh castellan buffers, but don't display them if not already shown."
  (interactive)
  (let* ((start-buffer (current-buffer))
	 (activity-buffer (castellan--activity-refresh))
	 (schedule-buffer (castellan--schedule-refresh)))
    (switch-to-buffer start-buffer)))
    ;; (when (or (equal start-buffer activity-buffer)
    ;; 	      (equal start-buffer schedule-buffer))
    ;;   (castellan-find-first))))

(defvar-keymap castellan-mode-map
  :doc "Keymap for castellan TODO management."
  "A"             #'castellan-set-activity
  "<tab>"         #'castellan-show-item
  "RET"           #'castellan-jump-to-item
  "g"             #'castellan-refresh
  "?"             #'castellan--debug
  "+"             #'castellan-item-done
  "-"             #'castellan-item-todo
  "<backspace>"   #'castellan-item-cancel
  )

(define-derived-mode castellan--mode fundamental-mode "castellan:Todo"
  "Castellan activity manager: Integrated TODO mode.

\\{castellan-mode-map}"
  (castellan--mode-setup)
  :mode-class 'special
  :group 'castellan
  :keymap castellan-mode-map
  )

(defun castellan--all-org-files ()
  (append
   (castellan--all-agenda-todos)
   castellan-agenda-calendars))

(defun castellan--all-org-buffers ()
  (mapcar #'castellan--find-visiting-create
	  (castellan--all-org-files)))

(defun castellan--only-noncastellan-buffers-advice (orig-fun &rest args)
  (unless (castellan--owned-buffer-p (current-buffer))
    (apply orig-fun args)))

(defun castellan--mode-setup ()
  (hl-line-mode 1)    ; Enable line highlighting
  (use-local-map castellan-mode-map)  ; Activate our keymap
  ; prevent org-add-log-note from trying to access Castellan buffers
  (unless
      (advice-member-p #'castellan--only-noncastellan-buffers-advice 'org-add-log-note)
    (advice-add 'org-add-log-note :around #'castellan--only-noncastellan-buffers-advice))
  ; Auto revert setup
  (when castellan-agenda-auto-revert
    (dolist (buffer (castellan--all-org-buffers))
      (with-current-buffer buffer
	(auto-revert-mode 1)))))

(defconst castellan--activity-agenda-buffer-name "*Castellan Activity TODO*"
  "Castellan buffer name for the activity agenda.")
(defconst castellan--schedule-agenda-buffer-name "*Castellan Scheduled TODO*"
  "Castellan buffer name for the schedule agenda.")

(defun castellan--activity-agenda-buffer ()
  (get-buffer-create castellan--activity-agenda-buffer-name))

(defun castellan--schedule-agenda-buffer ()
  (get-buffer-create castellan--schedule-agenda-buffer-name))

(defun castellan--owned-buffer-p (buf)
  "Checks if BUF is one of the Castellan display buffers"
  (let* ((b1 (get-buffer castellan--activity-agenda-buffer-name))
	 (b2 (get-buffer castellan--schedule-agenda-buffer-name)))
    (or (and b1 (equal b1 buf))
	(and b2 (equal b2 buf)))))

(defmacro castellan--save-excursion (buffer-expr &rest body)
  `(-let* [(buffer ,buffer-expr)
	   (result nil)
	   (old-buffer-point nil)
	   (window-pos-map nil)]
     (with-current-buffer buffer (progn
       (setq old-buffer-point (point))
       (dolist (window (get-buffer-window-list buffer nil nil))
	 (push (cons window (list (castellan--id (window-point window))))
	       window-pos-map))
       ,@body
       (dolist (window (get-buffer-window-list buffer nil nil))
	 (-let* [((_ id) (assoc window window-pos-map))
		 (pos (castellan--goto-id id))]
	   (when (numberp pos)
	     (set-window-point window pos))
	   ))
       (goto-char old-buffer-point)))))

(defun castellan--activity-refresh ()
  "Update activity agenda with recomputed activity items"
 (let ((buffer (castellan--activity-agenda-buffer)))
   (castellan--save-excursion buffer
     (read-only-mode)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (castellan--mode)
       (castellan--insert-aggregate-items
	(castellan--activity-items))))
   buffer))

(defun castellan--activity-items ()
  "Sorted items for the activity agenda"
  (let ((items (castellan--aggregate-org-items
		(castellan--get-all-org-items))))
    (if castellan-current-activity
	(sort items #'castellan--activity<)
      items)))

(defun castellan--schedule-refresh ()
  "Update schedule agenda with recomputed schedule items"
  (let ((buffer (castellan--schedule-agenda-buffer)))
    (castellan--save-excursion buffer
       (read-only-mode)
       (let ((inhibit-read-only t))
	 (erase-buffer)
	 (castellan--mode)
	 (castellan--insert-aggregate-scheduled-items
	  (castellan--schedule-items)))
       (window--adjust-process-windows))
    buffer))

(defun castellan--schedule-items ()
  "Sorted items for the schedule agenda"
  (sort
   (-filter
    #'castellan--scheduled-p
    (castellan--aggregate-org-items
     (append
      (castellan--get-all-calendar-items)
      (castellan--get-all-org-items)
      )))
   #'castellan--schedule<))

;; ================================================================================
;; NASTY BUT APPARENTLY NECESSARY
;; This completely disables org note-taking
(defun org-add-log-note ())
;; ================================================================================

; --------------------------------------------------------------------------------

(defface castellan-headline
  '((t :inherit org-level-3))
  "Face for TODO item headlines in castellan."
  :group 'castellan)

(defface castellan-indentation
  '((t :inherit org-indent))
  "Face for TODO item indentation markers in castellan."
  :group 'castellan)

(defface castellan-effort
  '((t :inherit org-duration))
  "Face for TODO item effort/estimated duration in castellan."
  :group 'castellan)

(defface castellan-priority-A
  '((t :inherit org-upcoming-deadline))
  "Face for TODO item headlines in castellan."
  :group 'castellan)

(defface castellan-priority-B
  '((t :inherit org-distant-deadline))
  "Face for TODO item headlines in castellan."
  :group 'castellan)

(defface castellan-priority-C
  '((t :inherit org-priority))
  "Face for TODO item headlines in castellan."
  :group 'castellan)

(defface castellan-scheduled-time
  '((t :inherit org-scheduled))
  "Face for TODO item scheduled start time in castellan."
  :group 'castellan)

(defface castellan-delegated
  '((t :inherit org-tag))
  "Face for delegated TODO items in castellan."
  :group 'castellan)

(defface castellan-delegatee-name
  '((t :inherit font-lock-function-name-face))
  "Face for the name of the person a task is delegated to in castellan."
  :group 'castellan)

(defface castellan-waiting
  '((t :inherit org-warning))
  "Face for TODO items waiting for someone in castellan."
  :group 'castellan)

(defface castellan-waiting-name
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

(defun castellan-find-first ()
  "Jumps to the first incomplete TODO item at or below point.

If there is no such item, moves to the end of the buffer."
  (interactive)
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))  ; While not found and not at end of buffer
      (let ((item (castellan--info-at-point 'ITEM)))
	(when item
	  (-let [(&plist :todo-type todo-type :repeater repeater) item]
	    (when (and (not repeater)
		     (string= todo-type "todo"))
	      (setq found t)))))
      (unless found
	(forward-line 1)))
    )
  )

(defun castellan ()
  "Shows both castellan TODO buffers and switch to the scheduled one."
  (interactive)
  (-let [already-had-schedule-buffer  (get-buffer castellan--schedule-agenda-buffer-name)]
    (castellan--auto-update-setup)
    (pop-to-buffer (castellan--activity-refresh))
    (pop-to-buffer (castellan--schedule-refresh))
    (unless already-had-schedule-buffer
      (castellan-find-first))))

(provide 'castellan)
