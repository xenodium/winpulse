;;; winpulse.el --- Momentary window background flash animation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/xenodium/winpulse
;; Version: 0.0.1

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Support this work https://github.com/sponsors/xenodium
;;
;; `winpulse-mode' momentarily flashes the background of a window
;; whenever it gains focus, providing a visual cue for the active window.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)

(defgroup winpulse nil
  "Momentary window background flash animation."
  :group 'convenience)

(defcustom winpulse-brightness 20
  "How much to shift the color (0-255 range) at peak flash."
  :type 'integer
  :group 'winpulse)

(defcustom winpulse-duration 0.62
  "Total animation duration in seconds."
  :type 'number
  :group 'winpulse)

(defcustom winpulse-step-interval 0.05
  "Seconds between animation frames."
  :type 'number
  :group 'winpulse)

(defcustom winpulse-ignore-minibuffer-focus nil
  "When non-nil, ignore minibuffer focus changes."
  :type 'boolean
  :group 'winpulse)

(defcustom winpulse-excluded-buffer-patterns '("\\` \\*Minibuf-")
  "List of regexps matched against buffer names to skip flashing."
  :type '(repeat regexp)
  :group 'winpulse)

(defvar winpulse--last-selected-window nil
  "The previously selected window, used to detect actual changes.")

(define-minor-mode winpulse-mode
  "Flash the focused window background whenever window focus changes."
  :global t
  :lighter nil
  (if winpulse-mode
      (add-hook 'window-selection-change-functions
                #'winpulse--on-window-selection-change)
    (remove-hook 'window-selection-change-functions
                 #'winpulse--on-window-selection-change)
    (setq winpulse--last-selected-window nil)))

(defun winpulse--on-window-selection-change (_frame)
  "Flash the newly selected window when focus change."
  (let ((win (selected-window)))
    (when (and (not (eq win winpulse--last-selected-window))
               ;; No need to flash when there's only one window.
               (> (length (window-list)) 1))
      (unless (and winpulse-ignore-minibuffer-focus
                   (minibufferp (window-buffer win)))
        (setq winpulse--last-selected-window win))
      (winpulse-window win))))

(defun winpulse-window (window)
  "Flash WINDOW by momentarily shifting its background color.
Lightens the background for dark themes, darkens it for light themes,
then animates back to the original color over several frames.
Only the specified window is affected, even if other windows show
the same buffer."
  (let* ((buffer (window-buffer window))
         (background `((:r . ,(nth 0 (color-values (face-background 'default nil t))))
                       (:g . ,(nth 1 (color-values (face-background 'default nil t))))
                       (:b . ,(nth 2 (color-values (face-background 'default nil t))))))
         (n (max 2 (round (/ winpulse-duration winpulse-step-interval))))
         (steps (mapcar (lambda (i)
                          (expt (- 1.0 (/ (float i) (1- n))) 2))
                        (number-sequence 0 (1- n))))
         (step-index 0)
         (timer nil))
    (when (and (window-live-p window)
               (not (seq-some (lambda (regexp)
                                (string-match-p regexp (buffer-name buffer)))
                              winpulse-excluded-buffer-patterns)))
      (winpulse--cleanup-window window)
      (set-window-parameter window 'winpulse--active t)
      (with-current-buffer buffer
        (set-window-parameter
         window 'winpulse--cookie
         (face-remap-add-relative
          'default
          (winpulse--make-face :rgb background
                               :shift (round (* winpulse-brightness (nth 0 steps))))))
        (setq timer
              (run-at-time winpulse-step-interval winpulse-step-interval
                           (lambda ()
                             (cond
                              ((not (window-live-p window))
                               (cancel-timer timer))
                              (t
                               (setq step-index (1+ step-index))
                               (if (>= step-index (length steps))
                                   (winpulse--cleanup-window window)
                                 (with-current-buffer buffer
                                   (face-remap-remove-relative (window-parameter window 'winpulse--cookie))
                                   (set-window-parameter
                                    window 'winpulse--cookie
                                    (face-remap-add-relative
                                     'default
                                     (winpulse--make-face :rgb background
                                                          :shift (round (* winpulse-brightness (nth step-index steps))))))))))))))
      (set-window-parameter window 'winpulse--timer timer))))

(cl-defun winpulse--shift-rgb (&key rgb shift)
  "Shift RGB values by SHIFT amount based on theme brightness.
RGB is an alist, e.g. ((:r . 10280) (:g . 10794) (:b . 13878)).
SHIFT is in 8-bit units, e.g. 40 means 40*256=10240 added per component.
Returns an RGB alist with shifted values, clamped to 0-65535."
  (let ((direction (if (eq (frame-parameter nil 'background-mode) 'dark)
                       1
                     -1)))
    (map-apply (lambda (key val)
                 (cons key (max 0 (min 65535 (+ val (* direction shift 256))))))
               rgb)))

(cl-defun winpulse--to-hex (&key rgb)
  "Convert RGB alist to a hex color string.
RGB is an alist, e.g. ((:r . 10280) (:g . 10794) (:b . 13878))."
  (format "#%02x%02x%02x"
          (/ (map-elt rgb :r) 256)
          (/ (map-elt rgb :g) 256)
          (/ (map-elt rgb :b) 256)))

(cl-defun winpulse--make-face (&key rgb shift)
  "Build a filtered face spec for the flash background at SHIFT intensity.
RGB is the base color alist.  SHIFT is the raw shift amount."
  `(:filtered (:window winpulse--active t)
              (:background ,(winpulse--to-hex :rgb (winpulse--shift-rgb
                                                    :rgb rgb
                                                    :shift shift)))))

(defun winpulse--cleanup-window (win)
  "Remove flash state from WIN."
  (let ((timer (window-parameter win 'winpulse--timer))
        (cookie (window-parameter win 'winpulse--cookie)))
    (when (timerp timer)
      (cancel-timer timer))
    (when cookie
      (with-current-buffer (window-buffer win)
        (face-remap-remove-relative cookie)))
    (set-window-parameter win 'winpulse--active nil)
    (set-window-parameter win 'winpulse--timer nil)
    (set-window-parameter win 'winpulse--cookie nil)))

(provide 'winpulse)
;;; winpulse.el ends here
