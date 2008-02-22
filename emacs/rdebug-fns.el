;;; rdebug-fns.el --- Ruby debugger miscellaneous functions

;; Copyright (C) 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2008 Anders Lindgren

;; $Id: rdebug-frames.el 711 2008-02-20 07:09:17Z andersl $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; See the manual and the file `rdebug.el' for more information.

;; This file contains code dealing with the frames secondary buffer.

;;; Code:

(require 'gud)
(require 'rdebug-vars)

(defun chomp(string &optional multiple)
  "Remove trailing \n if it's there"
  (if multiple
      (progn
	(while (and (> (length string) 0)
		    (eq (elt string (- (length string) 1)) ?\n))
	  (setq string (substring string 0 -1)))
	string)
    (if (> (length string) 0)
	(let ((s string))
	  (if (string= "\n" (substring s -1))
	      (substring s 0 -1)
	    s))
      "")))

(defun rdebug-dead-process-p ()
  "Return true if the rdebug comint-process is dead or exited."
  ;; FIXME? Use a variable in gud-comint-buffer's status?
  (or (not gud-comint-buffer) 
      (null (get-buffer-process gud-comint-buffer))))

(defun rdebug-add-location-to-ring (frame location-history-ring)
  "Add FRAME to LOCATION-HISTORY-RING if we are on the top frame and have a frame to add."
  ;; Switching frames shouldn't save a new ring
  ;; position. Also make sure no position is different.
  ;; Perhaps duplicates should be controlled by an option.
  (unless (and (not (ring-empty-p location-history-ring))
	       (equal (ring-ref location-history-ring 0) frame))
    (ring-insert location-history-ring  frame)))

(defun rdebug-set-frame-top-arrow (buf)
  "Set the fringe arrow in BUF to indicate the top frame."
  (with-current-buffer buf
    (setq fringe-indicator-alist
	  '((overlay-arrow . right-triangle)))))

(defun rdebug-set-frame-not-top-arrow (buf)
  "Set the fringe arrow in BUF to indicate a frame other than the top frame."
  (with-current-buffer buf
    (setq fringe-indicator-alist
	  '((overlay-arrow . hollow-right-triangle)))))

(defun rdebug-set-frame-arrow (buf)
  "Set the fringe arrow in buffer BUF."
  (if (equal 0 rdebug-frames-current-frame-number)
      (rdebug-set-frame-top-arrow buf)
    (rdebug-set-frame-not-top-arrow buf)))

(provide 'rdebug-fns)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-fns.el ends here
