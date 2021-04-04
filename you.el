;;; -*- lexical-binding: t; -*-

;;; json-pointer.el --- JSON pointer implementation in Emacs Lisp

;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-json-pointer
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'request)
(require 'json)

(defun json-pointer--parse-path (path)
  (let ((paths (split-string path "/" t)))
    (cl-loop for path in paths
             for p1 = (replace-regexp-in-string "~1" "/" path)
             for p2 = (replace-regexp-in-string "~0" "~" path)
             collect
             (if (string-match-p "[[:digit:]]" p2)
                 (string-to-number p2)
               (intern p2)))))

(defun json-pointer-get (json path)
  (let ((data (cl-copy-list json))
        (paths (json-pointer--parse-path path)))
    (cl-loop for p in paths
             if (and (consp data) (assoc p data))
             do
             (setq data (assoc-default p data))
             else

             if (and (vectorp data) (integerp p) (> (length data) p))
             do
             (setq data (aref data p))
             else
             return nil

             finally return data)))



;; from https://stackoverflow.com/a/44834833/6908755
(defun read-from-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (read (current-buffer))))

(defun print-to-file (filename data)
  (with-temp-file filename
    (prin1 data (current-buffer))))

;; inkove by (you)
;; all functions are prefixed with you-
;; make sure that you-seen-file-name exists on the file system with nil as its initial content
;; if the file is empty, this will break because it is not valid Elisp!

;; from YouTube Project in dmytro.butemann@gmail.com Google Developer Console
(defconst you-api-key  "AIz...8c")
(defconst you-seen-file-name "/Users/Gira/dotfiles/emacs/scripts/youtube-seen-files.el")

(defconst you-get-channels-to-check
  (list
   '("UCUORv_qpgmg8N5plVqlYjXg" "Medical Medium")))

(defvar you-videos-process-count 0)

(defvar you-buffer (get-buffer-create "*you*"))

;; initialized in (you) to a progress reporter
(defvar you-download-progress nil)

;; --- config ends


(defun you-get-url-for-channel (channel-id)
  (format "https://www.googleapis.com/youtube/v3/activities?key=%s&channelId=%s&part=contentDetails,snippet"
	  you-api-key channel-id))

(defun you-increase-download-count ()
  (setq you-videos-process-count (1+ you-videos-process-count))
  (progress-reporter-update you-download-progress you-videos-process-count))

(defun you-call-http (channel channel-count full-url)
  ;; apply-partially is used to pass channel to the lambda, otherwise it will not work!
  (request
    full-url
    :parser 'json-read
    :success (apply-partially
	      (cl-function
	       (lambda (channel channel-count &key data &allow-other-keys)
		 (you-extract-videos-and-process channel (reverse (append (assoc-default 'items data) nil)))
		 (you-increase-download-count)
		 (when (eq you-videos-process-count channel-count)
		   (beginning-of-buffer)
		   (progress-reporter-done you-download-progress))))
	      channel channel-count)
    :error (cl-function (lambda (&rest args &key thrown-error &allow-other-keys)
			  (message "Got error: %s" thrown-error)))))

(defun you-extract-videos-and-process (channel items)
  (dolist (item items)
    (let ((video-title (json-pointer-get item "/snippet/title")))
      (unless (you-has-seen-video channel video-title)
	(you-print-video-and-mark-seen channel video-title
				       (json-pointer-get item "/contentDetails/upload/videoId")))))
  (print-to-file you-seen-file-name you-seen-videos))

(defun you-has-seen-video (channel video-title)
  (or (member (you-to-seen-video-format channel video-title) you-seen-videos)))

(defun you-to-seen-video-format (channel video-title)
  (concat channel "|" video-title))

(defun you-print-video-and-mark-seen (channel video-title id)
  (princ (format "[[https://www.youtube.com/watch?v=%s][%s: %s]]\n" id channel video-title) you-buffer)
  (setq you-seen-videos (append you-seen-videos (list (you-to-seen-video-format channel video-title)))))

(defun you-prepare-buffers ()
  (switch-to-buffer "*you*")
  (erase-buffer)
  ;; to quickly being able to click the links via enter
  (org-mode))

(defun you-update-date-in-file ()
  (setq you-seen-videos (cons (format-time-string "%Y-%m-%d") (cdr you-seen-videos)))
  (print-to-file you-seen-file-name you-seen-videos))

(defun you-main ()
  (you-update-date-in-file)
  (you-prepare-buffers)
  (setq you-videos-process-count 0)
  (setq you-download-progress (make-progress-reporter
			       (format "Checking %d channels..." (length you-get-channels-to-check)
				       0  (length you-get-channels-to-check))))
  (dolist (element you-get-channels-to-check)
    (let ((url (you-get-url-for-channel (car element)))
	  (channel-title (nth 1 element)))
      (you-call-http channel-title (length you-get-channels-to-check) url))))

(defun you ()
  (interactive)
  (setq you-seen-videos (read-from-file you-seen-file-name))
  (if (string= (car you-seen-videos) (format-time-string "%Y-%m-%d"))
      (message "you was already called today!")
    (you-main)))
