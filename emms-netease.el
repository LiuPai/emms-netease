;;; emms-netease.el -- EMMS source for Netease Musicbox platform
;; Copyright (C) 2014 Liu Pai

;; Author: Liu Pai
;; Keywords: emms, netease
;; URL: https://github.com/LiuPai/emms-netease
;; X-Original-Version: 0.1
;; Package-Requires: ((emms "20131016") (json "1.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; After you've configured EMMS, load this file and give a Netease Playlist set
;; to the relevant EMMS source commands.

;; Example usage:
;;    M-x emms-add-netease-album RET
;;    Enter ALBUM_ID RET

;;; Code:

(require 'url)
(require 'json)
(require 'emms)
(require 'emms-browser)
(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-lyrics)
(require 'emms-info)

(defgroup emms-netease nil  "EMMS Netease musicbox Source"
  :group 'emms-source)

(defcustom emms-netease-display-header t
  "Non-nil means we display artwork and title for the set in the playlist."
  :type 'boolean
  :group 'emms-source-netease)

(defcustom emms-netease-quality 'mMusic
  "EMMS Netease Musicbox quality."
  :type '(choice (symbol :tag "Low quality" lMusic)
		 (symbol :tag "Middle quality" mMusic)
		 (symbol :tag "High quality" hMusic)
		 (symbol :tag "Original quality" bMusic))
  :group 'emms-source-netease)


(defun emms-netease-dfsId-encrypt (dfsId)
  "Encrypt DFSID to a string used in music resource url path."
  (let* ((key "3go8&$8*3*3h0k(2)2")
	 (key-length (length key))
	 (id-length (length dfsId))
	 (temp (make-string id-length 0))
	 (i 0)
	 (result ""))
    (while (< i id-length)
      (aset temp i (logxor (get-byte i dfsId)
			   (get-byte (% i key-length) key)))
      (setq i (1+ i)))
    (setq result (base64-encode-string (secure-hash 'md5 temp nil nil t)))
    (setq result (replace-regexp-in-string "/" "_" result))
    (setq result (replace-regexp-in-string "+" "-" result))
    result))

(defun emms-netease-request (method action &optional query)
  "Get and parse JSON from given METHOD and ACTION url with QUERY data."
  (let ((url-request-extra-headers '(("Host" . "music.163.com")
                                     ("Accept" . "*/*")
                                     ("Content-Type" . "application/x-www-form-urlencoded")
                                     ("Accept-Encoding" . "deflate")
                                     ("Cookie" . "os=pc; appver=1.4.1.67140;")))
	url)
    ;; Build require url
    (if (string= method "GET")
	(progn
	  (setq url-request-method "GET")
	  (if query
	      (setq url (concat action "?" query))
	    (setq url action)))
      (if (string= method "POST")
	  (progn
	    (setq url-request-method "POST")
	    (setq url-request-data query)
	    (setq url action))))
    ;; Get response and parse json
    (with-current-buffer (url-retrieve-synchronously url)
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let* ((result (json-read))
	     (code (cdr (assoc 'code result))))
      (unless (eq  code 200)
	(error "Undefined Code" result))
      result))))

(defun emms-netease-convert-song-to-track (song)
  "Read Netease Musicbox SONG into an emms-track."
  (let* ((default-stream-url (cdr (assoc 'mp3Url song)))
	 (id (number-to-string (cdr (assoc 'id song))))
	 (title (cdr (assoc 'name song)))
	 (artists (cdr (assoc 'artists song)))
	 (main-artist (cdr (assoc 'name (aref artists 0))))
	 (album (cdr (assoc 'album song)))
	 (album-name (cdr (assoc 'name album)))
	 (publish-time (seconds-to-time (/ (cdr (assoc 'publishTime album)) 1000)))
	 (publish-year (number-to-string (nth-value 5 (decode-time publish-time))))
	 (source (cdr (assoc emms-netease-quality song)))
	 (play-time (/ (cdr (assoc 'playTime source)) 1000))
	 (extension (cdr (assoc 'extension source)))
	 (dfsId (number-to-string (cdr (assoc 'dfsId source))))
	 (stream-url (concat "http://m1.music.126.net/"
			     (emms-netease-dfsId-encrypt dfsId)
			     "/" dfsId "." extension))
	 (emms-track (emms-track 'url stream-url)))
    (emms-track-set emms-track 'id id)
    (emms-track-set emms-track 'info-title title)
    (emms-track-set emms-track 'info-playing-time play-time)
    (emms-track-set emms-track 'info-album album-name)
    (emms-track-set emms-track 'info-artist main-artist)
    (emms-track-set emms-track 'info-year publish-year)
    emms-track))

(defun emms-netease-insert-artwork (url)
  "Download the given artwork URL and insert it into the active playlist."
  (let* ((filename (cadr (split-string
			  (elt (url-generic-parse-url url) 6) "/" t)))
	 (directory (concat temporary-file-directory
			    (file-name-as-directory "netease-artwork")))
	 (path (concat directory filename)))
    (when (not (file-exists-p directory))
      (make-directory directory t))
    (when (not (file-exists-p path))
      (url-copy-file url path))
    (emms-browser-insert-cover path)))


;;;###autoload (autoload 'emms-play-netease-album "emms-netease" nil t)
;;;###autoload (autoload 'emms-add-netease-album "emms-netease" nil t)
(define-emms-source netease-album (album-id)
  "An EMMS source for Netease album ALBUM-ID."
  (interactive (list (read-string "Album ID: ")))
  (let* ((album (emms-netease-get-album album-id))
	 (title (cdr (assoc 'name album)))
	 (artwork-url (cdr (assoc 'blurPicUrl album)))
	 (songs (cdr (assoc 'songs album)))
	 (emms-tracks (mapcar #'emms-netease-convert-song-to-track songs)))
    (when emms-netease-display-header
      (emms-netease-insert-artwork artwork-url)
      (insert " " (propertize title 'face 'emms-browser-album-face)
	      "\n"))
    (mapc #'emms-playlist-insert-track emms-tracks)))

(defun emms-netease-get-album (album-id)
  "Get Netease Musicbox album infomation of ALBUM-ID."
  (let* ((result (emms-netease-request
		  "GET"
		  (concat "http://music.163.com/api/album/" album-id))))
    (cdr (assoc 'album result))))

(defun emms-netease-search (s type offset limit)
  "Search name contain string S of TYPE from OFFSET max to LIMIT results.
Argument List:

S     The search string
TYPE  :song | :album | :artist | :playlist | :mylist, for the search type
LIMIT max results request."
  (let* ((action "http://music.163.com/api/search/pc")
	 (type (cond
		((eq type :song) 1)
		((eq type :album) 10)
		((eq type :artist) 100)
		((eq type :playlist) 1000)
		((eq type :mylist) 1002)))
	 (query (format "type=%s&s=%s&offset=%s&total=true&limit=%s"
			type (url-encode-url s) offset limit)))
    (emms-netease-request "POST" action query)))
(provide 'emms-netease)
;;; emms-netease.el ends here
