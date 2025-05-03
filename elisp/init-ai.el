;;; Package --- init-ai.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Integrate AI Tools

;;; Code:

(defun get-openrouter-api-key ()
  "Get OpenRouter API Key from 1password."
  (string-trim
   (shell-command-to-string "op read op://AI/OpenRouter-Gemini/credential")))

(defun get-gemini-api-key ()
  "Get OpenRouter API Key from 1password."
  (string-trim
   (shell-command-to-string "op read 'op://AI/Google AI Studio API/credential'")))

(defun get-deepseek-api-key ()
  "Get DeepSeek API Key from 1password."
  (string-trim
   (shell-command-to-string "op read op://AI/deepseek-api-key/password")))

(use-package aidermacs
  :straight t
  :defer t
  :config
  ;; (setenv "OPENROUTER_API_KEY" (get-openrouter-api-key))
  ;; (setenv "GEMINI_API_KEY" (get-gemini-api-key))
  (setenv "DEEPSEEK_API_KEY" (get-deepseek-api-key))
  :custom
  (aidermacs-use-architect-mode t)
  ;; (aidermacs-default-model "openrouter/google/gemini-2.5-pro-exp-03-25:free")
  ;; (aidermacs-default-model "openrouter/google/gemini-2.5-pro-exp-03-25:free")
  (aidermacs-default-model "deepseek/deepseek-chat")
  :init
  (leader-def
    "a"  '(:ignore t :which-key "aidermacs")
    "aa" '(aidermacs-transient-menu :which-key "Popup Aidermacs menu")))

(use-package whisper
  :straight (:host github :repo "natrys/whisper.el" :branch "master")
  :bind ("C-s-r" . whisper-run)
  :config
  (setq whisper-install-directory "~/.local/bin/"
        whisper-model "base"
        whisper-language "en"
        whisper-translate nil
        whisper-use-threads (/ (num-processors) 2)
        whisper--ffmpeg-input-device ":0"))

(defun whisper/get-ffmpeg-device ()
  "Gets the list of devices available to ffmpeg.
Credit to @rksm:
The output of the ffmpeg command is pretty messy, e.g.
  [AVFoundation indev @ 0x7f867f004580] AVFoundation video devices:
  [AVFoundation indev @ 0x7f867f004580] [0] FaceTime HD Camera (Built-in)
  [AVFoundation indev @ 0x7f867f004580] AVFoundation audio devices:
  [AVFoundation indev @ 0x7f867f004580] [0] Cam Link 4K
  [AVFoundation indev @ 0x7f867f004580] [1] MacBook Pro Microphone
so we need to parse it to get the list of devices.
The return value contains two lists, one for video devices and one for audio
devices.Each list contains a list of cons cells, where the car is the device
 number and the cdr is the device name."
  (unless (string-equal system-type "darwin")
    (error "This function is currently only supported on macOS"))

  (let ((lines (string-split (shell-command-to-string "ffmpeg -list_devices true -f avfoundation -i dummy || true") "\n")))
    (cl-loop with at-video-devices = nil
             with at-audio-devices = nil
             with video-devices = nil
             with audio-devices = nil
             for line in lines
             when (string-match "AVFoundation video devices:" line)
             do (setq at-video-devices t
                      at-audio-devices nil)
             when (string-match "AVFoundation audio devices:" line)
             do (setq at-audio-devices t
                      at-video-devices nil)
             when (and at-video-devices
                       (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
             do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) video-devices)
             when (and at-audio-devices
                       (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
             do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) audio-devices)
             finally return (list (nreverse video-devices) (nreverse audio-devices)))))


(defun whisper/find-device-matching (string type)
  "Get the devices from and look for a device matching `STRING'.
`TYPE' can be :video or :audio."
  (let* ((devices (whisper/get-ffmpeg-device))
         (device-list (if (eq type :video)
                          (car devices)
                        (cadr devices))))
    (cl-loop for device in device-list
             when (string-match-p string (cdr device))
             return (car device))))


(defcustom whisper/default-audio-device 0
  "The default audio device to use for whisper.el and outher audio processes."
  :type 'integer)

(defun whisper/select-default-audio-device (&optional device-name)
  "Interactively select an audio device.
Select audio device to use for whisper.el and other audio processes.
If `DEVICE-NAME' is provided, it will be used instead of prompting the user."
  (interactive)
  (let* ((audio-devices (cadr (whisper/get-ffmpeg-device)))
         (indexes (mapcar #'car audio-devices))
         (names (mapcar #'cdr audio-devices))
         (name (or device-name (completing-read "Select audio device: " names nil t))))
    (setq whisper/default-audio-device (whisper/find-device-matching name :audio))
    (unless (boundp 'whisper--ffmpeg-input-device)
      (setq whisper--ffmpeg-input-device (format ":%s" whisper/default-audio-device)))))

(provide 'init-ai)
;;; init-ai.el ends here.
