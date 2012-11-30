;;; couchtunes.el --- Couchtunes client for GNU Emacs

;; Author: Axel Johnsson <johnsson.axel@gmail.com>
;; URL: https://github.com/axeljohnsson/couchtunes-emacs
;; Version: 0.0.1

;; Copyright (C) 2012  Axel Johnsson

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'json)
(require 'url)

(defvar couchtunes-buffer-name "*Couchtunes*")
(defvar couchtunes-database-url "http://127.0.0.1:5984/couchtunes")
(defvar couchtunes-player-current-track nil)
(defvar couchtunes-player-process "couchtunes-player-process")

(defvar couchtunes-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "+" 'couchtunes-import)
    (define-key map "b" 'undo)
    map)
  "Keymap for `couchtunes-mode'.")

(defun couchtunes ()
  (interactive)  
  (couchtunes-view "tracks")
  (couchtunes-mode))

(defun couchtunes-mode ()
  "Major mode for interacting with Couchtunes"
  (interactive)
  (kill-all-local-variables)
  (use-local-map couchtunes-mode-map)
  (setq major-mode 'couchtunes-mode
	mode-name "Couchtunes")
  (run-mode-hooks 'couchtunes-mode-hook))

(defun couchtunes-player-play (id)
  "Start playback process by song ID"
  (start-process-shell-command
   couchtunes-player-process
   couchtunes-buffer-name
   (couchtunes-player-command id))
  (set-process-sentinel
   (get-process couchtunes-player-process)
   'couchtunes-player-process-sentinel)
  (setq couchtunes-player-current-track id))

(defun couchtunes-player-stop ()
  "Stop playback process"
  (delete-process couchtunes-player-process)
  (setq couchtunes-player-current-track nil))

(defun couchtunes-player-toggle (id)
  (if (get-process couchtunes-player-process)      
      (if (equal couchtunes-player-current-track id)
	  (couchtunes-player-stop)
	(couchtunes-player-stop)
	(couchtunes-player-play id))
    (couchtunes-player-play id)))

(defun couchtunes-player-process-sentinel (process event)
  (cond ((equal event "finished\n")
	 (couchtunes-player-play
	  (car (couchtunes-player-queue couchtunes-player-current-track))))
	((equal event "killed: 9\n"))
	(t (message event))))

(defun couchtunes-player-queue (id)
  (save-excursion
    (goto-char (point-min))
    (forward-line)
    (let (i over under flag)
      (while (button-at (point))
	(setq i (button-get (button-at (point)) 'id))
	(if flag
	    (setq under (cons i under))
	  (if (equal i id)
	      (setq flag t)
	    (setq over (cons i over))))
	(forward-line))
      (append (reverse under) (reverse over)))))


(defun couchtunes-player-command (id)
  (concat "curl -s " couchtunes-database-url "/"
	  id "/data.flac | play -q -v 0.2 -"))


(defun couchtunes-view (name)
  (set-buffer (get-buffer-create couchtunes-buffer-name))
  (couchtunes-view-format (couchtunes-view-rows name))
  (switch-to-buffer (current-buffer)))

(defun couchtunes-view-format (xs)
  (save-excursion
    (delete-region (point-min) (point-max))
    (couchtunes-view-format-header)
    (dolist (x xs)
      (couchtunes-view-format-row (cdr (assoc 'doc x))))))

(defun couchtunes-view-format-header ()
  (insert "Song")
  (indent-to 40 1)
  (insert "Album")
  (indent-to 60 1)
  (insert "Artist")
  (newline))

(defun couchtunes-view-format-row (doc)
  (insert-text-button
   (couchtunes-truncate (cdr (assoc 'title doc)) 35)
   'id (cdr (assoc '_id doc))
   'action 'couchtunes-view-button-track)
  (indent-to 40 1)
  (insert-text-button
   (couchtunes-truncate (cdr (assoc 'album doc)) 15)
   'action 'couchtunes-view-reduce)
  (indent-to 60 1)
  (insert-text-button
   (couchtunes-truncate (cdr (assoc 'artist doc)) 15)
   'action 'couchtunes-view-reduce)
  (newline))

(defun couchtunes-view-rows (name)
  (let ((body (couchtunes-database-request (couchtunes-view-url name))))
    (append (cdr (assoc 'rows body)) nil)))

(defun couchtunes-view-url (name)
  (concat "/_design/app/_view/" name "?include_docs=true"))

(defun couchtunes-view-button-track (b)
  (couchtunes-player-toggle (button-get (button-at (point)) 'id)))

(defun couchtunes-view-reduce (b)
  (let ((string (button-label b)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward string)
      (beginning-of-line)
      (delete-region (point-min) (point))
      (couchtunes-view-format-header)
      (end-of-buffer)
      (re-search-backward string)
      (end-of-line)
      (delete-region (point) (point-max))))
  (goto-char (point-min)))


(defun couchtunes-database-request (url)
  (json-read-from-string
   (couchtunes-http-request
    (concat couchtunes-database-url url))))


(defun couchtunes-http-request (url)
  "Make HTTP request."
  (save-excursion
    (set-buffer (url-retrieve-synchronously url))
    (couchtunes-http-response)))

(defun couchtunes-http-response ()
  "Handles HTTP response."
  (prog1 (couchtunes-http-response-body) (kill-buffer)))

(defun couchtunes-http-response-headers ()
  "Extract headers from HTTP response."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (forward-char)
    (buffer-substring (point-min) (point))))

(defun couchtunes-http-response-body ()
  "Extract body from HTTP response."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (forward-char)
    (decode-coding-string
     (buffer-substring (point) (point-max)) 'utf-8)))

;; (defun couchtunes-http-response-data (start end))
;;   "Extract data between START and END from HTTP response."


(defun couchtunes-truncate (string len)
  (if (> (length string) len)
      (concat (substring string 0 (- len 2)) "...")
    string))


(provide 'couchtunes)

;;; couchtunes.el ends here
