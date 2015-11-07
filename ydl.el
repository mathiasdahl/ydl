;;; ydl.el --- YouTube download monitor thing

;;; Commentary:
;;

;;; Code:

(defvar ydl-monitor-folder "f:/Dropbox/common/dat/ydl")
(defvar ydl-mp3-folder "f:/dat/gfx/mp3/ydl")
(defvar ydl-monitor-timer nil)
(defvar ydl-process-count 0)

(defun ydl-log-text (text)
  "Log TEXT to `log.txt' in `ydl-monitor-folder'."
  (with-temp-buffer
    (insert (format "\n%s: %s" (format-time-string "%Y-%m-%d %H:%M:%S") text))
    (append-to-file (point-min) (point-max) (format "%s/log.txt" ydl-monitor-folder))))

(defun ydl-get-title (file)
  "Get title from filename of FILE."
  (substring file 0 (string-match "-\\([^-]+\\)$" (file-name-sans-extension file))))

(defun ydl-inprogress-folder ()
  "Get the inprogress folder."
  (format "%s/inprogress" ydl-monitor-folder))

(defun ydl-add-id3 (file)
  "Add id3 mp3 tags to FILE."
  (call-process "id3" nil nil nil
                        "-a" "YouTube"
                        "-l" "YouTube Downloads"
                        "-t" (ydl-get-title file)
                        (format "%s/%s" (ydl-inprogress-folder) file))
  (ydl-log-text (format "Added id3 tags to %s" file)))

(defun ydl-move-mp3 (file)
  "Move mp3 file FILE from inprogress folder to `ydl-mp3-folder'."
  (let ((from (format "%s/%s" (ydl-inprogress-folder) file))
        (to (format "%s/%s (ydl).mp3" ydl-mp3-folder (file-name-sans-extension file))))
    (rename-file from to t)
    (ydl-log-text (format "Moved mp3 from \"%s\" to \"%s\"" from to))))

(defun ydl-sentinel (process event)
  "Process sentinel for PROCESS and EVENT.
Checks for successful or unsuccessful downloads and takes
necessary actions to move files around."
  (save-excursion
    (set-buffer (process-buffer process))
    (when (string-match "finished" event)
      (message "ydl process is finished")
      (when (search-backward-regexp "Destination: \\(.+\\)$" nil t)
        (let ((new-file (match-string 1)))
          (message "Saved file %s" new-file)
          (ydl-add-id3 new-file)
          (ydl-move-mp3 new-file)
          (ydl-move-file (process-get process :urlfile) 'done))
        (kill-buffer (process-buffer process))))
    (when (string-match "exited abnormally" event)
      (message "ydl process finished with error")
      (ydl-move-file (process-get process :urlfile) 'error)
      (kill-buffer (process-buffer process)))))

(defun ydl-get-process-name ()
  "Get a unique process name using a simple counter."
  (format "ydl-%d"
          (setq ydl-process-count (1+ ydl-process-count))))

(defun ydl (file)
  "Download and extract audio from YouTube URL found in FILE.
Creates a new process which is handled by a sentinel that manages
the downloaded file and the input file."
  (let* ((process-name (ydl-get-process-name))
         (buffer-name (format "*%s*" process-name))
         (default-directory (file-name-as-directory (ydl-inprogress-folder)))
         (url (ydl-get-url-from-file file))
         process)
    (if (not url)
        (ydl-move-file file 'error)
      (set-buffer (get-buffer-create buffer-name))
      (erase-buffer)
      (setq process
            (start-process process-name buffer-name
                           "youtube-dl"
                           url "-x" "--audio-format" "mp3"))
      (process-put process :urlfile file)
      (set-process-sentinel process 'ydl-sentinel))))

(defun ydl-list-incoming ()
  "List incoming files."
  (directory-files (format "%s/incoming" ydl-monitor-folder) nil "^[^.]"))

(defun ydl-monitor ()
  "Monitor and process incoming files."
  (let ((files (ydl-list-incoming)))
    (when files
      (dolist (file files)
        (ydl-process-file file)))))

(defun ydl-start-monitor ()
  "Start monitoring the incoming folder for files with YouTube URLs."
  (interactive)
  (setq ydl-monitor-timer (run-with-timer 20 5 'ydl-monitor))
  (ydl-log-text "Started monitoring"))

(defun ydl-stop-monitor ()
  "Stop monitoring."
  (interactive)
  (cancel-timer ydl-monitor-timer)
  (ydl-log-text "Stopped monitoring"))

(defun ydl-get-url-from-file (file)
  "Get URL defined in FILE."
  (let ((url (with-temp-buffer
               (insert-file-contents (format "%s/inprogress/%s" ydl-monitor-folder file))
               (if (search-forward-regexp "^URL=\\(.+\\)$" nil t)
                   (match-string 1)))))
    (ydl-log-text (if url
                      (format "Found URL %s in %s" url file)
                    (format "Did not find any URL in %s" file)))
    url))

(defvar ydl-from-to-names
  '((inprogress . incoming)
    (done . inprogress)
    (error . inprogress)))

(defun ydl-build-full-file-name (file folder)
  "Return the full filename based on the filename FILE and the symbol FOLDER."
  (format "%s/%s/%s" ydl-monitor-folder (symbol-name folder) file))

(defun ydl-move-file (file target)
  "Move control file FILE to TARGET.
TARGET is a symbol representing the target folder and can be one
of `inprogress', `done' or `error'."
  (let* ((from-folder (cdr (assoc target ydl-from-to-names)))
         from-file to-file)
    (when from-folder
      (setq from-file (ydl-build-full-file-name file from-folder)
            to-file (ydl-build-full-file-name file target))
      (rename-file from-file to-file t)
      (ydl-log-text (format "Moved %s to %s" from-file to-file)))))

(defun ydl-process-file (file)
  "Process control file FILE.
Moves file into the `inprogress' folder and starts processing
it."
  (ydl-log-text (format "Found file %s" file))
  (ydl-move-file file 'inprogress)
  (ydl file))

(provide 'ydl)

;;; ydl.el ends here
