;;; go-back.el -- Emacs location history navigation.

;; Copyright (C) 2014 Andrzej Pronobis <a.pronobis@gmail.com>

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

;;; Commentary:
;; Please see https://github.com/pronobis/go-back for a documentation.

;;; Code:


(defgroup go-back nil
  "Emacs location history navigation."
  :group 'tools)


(defcustom go-back-commands '(self-insert-command
                                newline
                                sp-delete-char
                                sp-backward-delete-char
                                yank
                                )
  "A list of commands after which we remember the buffer and position."
  :type '(choice (const :tag "Any command" t)
                 (const :tag "Self insert command" '(self-insert-command))
                 (repeat :tag "Commands" function))
  :group 'go-back)


(defcustom go-back-min-distance 1000
  "Minimum distance between point that will be registered as a new event in the history."
  :type 'integer
  :group 'go-back
  )


(defcustom go-back-hist-length 30
  "Maximum length of the history."
  :type 'integer
  :group 'go-back
  )


(defcustom go-back-boring-buffer-regexp-list
  '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf" "\\*Ibuffer\\*" "\\*RTags\\*")
  "The regexp list that matches boring buffers."
  :type  '(repeat (choice regexp))
  :group 'go-back)


(defvar go-back-hist (list)
  "The buffers and positions in the history.")


(defvar go-back-future (list)
  "The buffers and positions in the future wrt to current history.")


(defun go-back--check-buffer (buf)
  "Return true if BUF is a supported buffer."
  (let ((buf-name (buffer-name buf)))
    (not (cl-loop for regexp in go-back-boring-buffer-regexp-list
             thereis (string-match regexp buf-name)))))


(defun go-back--post-command ()
  "Post-command hook used by the mode."
  (when (and (memq this-command go-back-commands)
             (go-back--check-buffer (current-buffer)))
    (go-back--add-to-hist (current-buffer) (point))
  ))


(defadvice push-mark (after go-back--advice-push-mark (&optional location nomsg activate) activate)
  "Advice capturing new mark in the 'mark-ring'."
  (let ((p (or location (point))))
    (go-back--add-to-hist (current-buffer) p)))


(defadvice switch-to-buffer (after go-back--advice-switch-to-buffer (buffer-or-name &optional norecord force-same-window) activate)
  "Advice capturing switching to a new buffer."
  (when (go-back--check-buffer (current-buffer))
    (go-back--add-to-hist (current-buffer) (point))))


(defun go-back--add-to-hist (buf point)
  "Add the BUF and POINT to the stack."
  (go-back--clean-hist)   ; Clean-up the history first
  (let* ((last-event (pop go-back-hist))
         (last-buf (car last-event))
         (last-point (cdr last-event)))
    ;; Should we replace the last event with this one?
    (if (or (not last-event)
            (and (eq last-buf buf)
                 (< (abs (- last-point point)) go-back-min-distance)))
        ;; Yes, add only the new one
        (setq go-back-hist (cons (cons buf point) go-back-hist))
      ;; No, do not replace, but add new element on top of previous one
      (progn
        (setq go-back-hist (cons last-event go-back-hist)) ; Re-add the last event
        (setq go-back-hist (cons (cons buf point) go-back-hist))
        ;; Also, clear any future since we are now overwriting it
        (setq go-back-future (list))))))  ; Add new one


(defun go-back--clean-hist ()
  "Remove killed buffers from the history and future."
  (setq go-back-hist
        (cl-loop for event in go-back-hist
                 with n = 0 do (setq n (1+ n))
                 while (<= n go-back-hist-length)
                 if (buffer-live-p (car event))
                 ;; Reduce the position to the size of the buffer
                 collect (cons (car event)
                               (min (cdr event)
                                    (with-current-buffer (car event)
                                        (1+ (buffer-size)))))))
  (setq go-back-future
        (cl-loop for event in go-back-future
                 if (buffer-live-p (car event))
                 ;; Reduce the position to the size of the buffer
                 collect (cons (car event)
                               (min (cdr event)
                                    (with-current-buffer (car event)
                                      (1+ (buffer-size))))))))


(defun go-back-setup ()
  "Setup the go-back module."
  (interactive)
  (add-hook 'post-command-hook
            'go-back--post-command
            t))


(defun go-back-go-to-last ()
  "Go to the last position in the history."
  (interactive)
  (let* ((last-event (car go-back-hist))
         (last-buf (car last-event))
         (last-point (cdr last-event)))
    (ad-deactivate 'switch-to-buffer)
    (switch-to-buffer last-buf)
    (ad-activate 'switch-to-buffer)
    (goto-char last-point)))


(defun go-back-go-backward ()
  "Move back in history."
  (interactive)
  ;; Clean-up the history first
  (go-back--clean-hist)
  ;; If there is no future, remember the current position before going back
  (when (and (= (length go-back-future) 0)
             (go-back--check-buffer (current-buffer)))
    (go-back--add-to-hist (current-buffer) (point)))
  ;; Go-back
  (let* ((last-event (pop go-back-hist))
         (last-buf (car last-event))
         (last-point (cdr last-event)))
    ;; Only act if there is something in the history
    (when last-event
        ;; Are we already in the last point in history
        (if (and (eq last-buf (current-buffer))
                 (eq last-point (point)))
            ;; Yes, now check if there is something left in history
            (if (car go-back-hist)
                ;; Yes , go to the next element in history
                (progn
                  ;; Add last event to future and move to the new last
                  (setq go-back-future (cons last-event go-back-future))
                  (go-back-go-to-last))
              ;; No, do nothing, just re-add the removed element
              (setq go-back-hist (cons last-event go-back-hist))
              )
          ;; No, go there
          (progn
            ;; Re-add the removed element to hist
            (setq go-back-hist (cons last-event go-back-hist))
            (go-back-go-to-last)))))
  (go-back-display-status))


(defun go-back-go-forward ()
  "Move forward in history."
  (interactive)
  (go-back--clean-hist)   ; Clean-up the history first
  (when (car go-back-future)  ; If future holds something for us
    (let ((future-event (pop go-back-future)))
      (setq go-back-hist (cons future-event go-back-hist)))
    (go-back-go-to-last))
  (go-back-display-status))


(defun go-back-clear-history ()
  "Clears the history."
  (interactive)
  (setq go-back-hist (list))
  (setq go-back-future (list)))


(defun go-back-display-status ()
  "Displays the current status and position in the history."
  (interactive)
  (let ((len-hist (length go-back-hist))
        (len-future (length go-back-future)))
    (message "%s%s%s%s%s"
             (propertize "[" 'face '(:foreground "yellow"))
             (propertize (make-string (max 0 (- len-hist 1)) ?•) 'face '(:foreground "white"))
             (propertize (if (> len-hist 0) "✱" "")  'face '(:foreground "red"))
             (propertize (make-string len-future ?•) 'face '(:foreground "white"))
             (propertize "]" 'face '(:foreground "yellow"))
             )))

(provide 'go-back)
;;; go-back.el ends here
