;;; org-marked-text-overview.el --- Minor mode for overview of marked text in Org/Markdown -*- lexical-binding: t; -*-

;; Copyright (C) 2024 lijigang

;; Author: lijigang <i@lijigang.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.4"))
;; Keywords: outlines
;; URL: https://github.com/lijigang/org-marked-text-overview

;;; Commentary:

;; This package provides a minor mode for Org or Markdown buffers that
;; displays an overview of marked text (bold, verbatim, underline, etc.)
;; in a separate buffer.
;; The overview is presented as a read-only bullet list, and clicking on an
;; item jumps to its location in the original buffer.

;;; Code:

(require 'org)
(require 'org-element)
(require 'markdown-mode nil t)

(defgroup org-marked-text-overview nil
  "Overview of marked text in Org or Markdown buffers."
  :group 'org
  :prefix "org-marked-text-overview-")

(defcustom org-marked-text-overview-buffer-name "*Org Marked Text Overview*"
  "Name of the buffer for displaying the marked text overview."
  :type 'string
  :group 'org-marked-text-overview)

(defvar-local org-marked-text-overview-source-buffer nil
  "Buffer from which the overview was generated.")

(defun org-marked-text-overview-collect-elements-org ()
  "Collect marked text elements from the current Org buffer."
  (let ((marked-elements '()))
    (org-element-map (org-element-parse-buffer) '(bold code underline verbatim strike-through italic)
      (lambda (element)
        (let* ((begin (org-element-property :begin element))
               (end (org-element-property :end element))
               (raw-text (buffer-substring-no-properties begin end))
               (clean-text (org-marked-text-overview-clean-text raw-text (org-element-type element))))
          (push (list :text clean-text :begin begin) marked-elements))))
    (nreverse marked-elements)))

(defun org-marked-text-overview-clean-text (text type)
  "Remove markup from TEXT based on element TYPE.

This function previously called `string-trim' with raw markup
characters which are treated as regular expressions.  Characters
such as `*' or `~' have special meaning in regexps and caused an
`invalid-regexp' error when used directly.  Escape the characters
so that `string-trim' removes exactly one leading and trailing
markup character."
  (let ((regexp (lambda (s) (regexp-quote s))))
    (pcase type
      ('bold (string-trim text (funcall regexp "*") (funcall regexp "*")))
      ('code (string-trim text (funcall regexp "~") (funcall regexp "~")))
      ('underline (string-trim text (funcall regexp "_") (funcall regexp "_")))
      ('verbatim (string-trim text (funcall regexp "=") (funcall regexp "=")))
      ('strike-through (string-trim text (funcall regexp "+") (funcall regexp "+")))
      ('italic (string-trim text (funcall regexp "/") (funcall regexp "/")))
      (_ text))))

(defun org-marked-text-overview-collect-elements-markdown ()
  "Collect marked text elements from a Markdown buffer."
  (let ((marked-elements '())
        (patterns '(("\\*\\*\\([^*]+\\)\\*\\*" . bold)
                    ("__\\([^_]+\\)__" . underline)
                    ("`\\([^`]+\\)`" . code)
                    ("~~\\([^~]+\\)~~" . strike-through)
                    ("\\(^\\|[^*]\\)\\*\\([^*]+\\)\\*\\([^*]\\|$\\)" . italic)))
        match-begin)
    (dolist (pat patterns)
      (goto-char (point-min))
      (while (re-search-forward (car pat) nil t)
        (setq match-begin (match-beginning 0))
        (push (list :text (match-string-no-properties 1)
                    :begin match-begin) marked-elements)))
    (nreverse marked-elements)))

(defun org-marked-text-overview-collect-elements ()
  "Collect marked text elements based on current major mode."
  (cond
   ((derived-mode-p 'org-mode)
    (org-marked-text-overview-collect-elements-org))
   ((derived-mode-p 'markdown-mode 'gfm-mode)
    (org-marked-text-overview-collect-elements-markdown))
   (t
    (user-error "Unsupported major mode"))))

(defun org-marked-text-overview-update ()
  "Update the marked text overview buffer."
  (interactive)
  (when (derived-mode-p 'org-mode 'markdown-mode 'gfm-mode)
    (let ((overview-buffer (get-buffer-create org-marked-text-overview-buffer-name))
          (current-buffer (current-buffer))
          (marked-elements (org-marked-text-overview-collect-elements)))
      (with-current-buffer overview-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (if (derived-mode-p 'markdown-mode 'gfm-mode)
              (markdown-mode)
            (org-mode))
          (insert "#+title: Marked Text Overview\n\n")
          (dolist (element marked-elements)
            (let ((text (plist-get element :text))
                  (begin (plist-get element :begin)))
              (insert "- ")
              (let ((start (point)))
                (insert text)
                (put-text-property start (1+ start) 'org-marked-text-position begin))
              (insert "\n")))
          (goto-char (point-min))
          (setq-local org-marked-text-overview-source-buffer current-buffer)
          (setq buffer-read-only t)))
      (org-marked-text-overview-display-buffers))))

(defun org-marked-text-overview-display-buffers ()
  "Display the overview buffer side by side with the source buffer."
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (switch-to-buffer org-marked-text-overview-buffer-name)
  (other-window 1))

(defun org-marked-text-overview-jump-to-original ()
  "Jump to the original location of the marked text."
  (interactive)
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (pos (text-property-not-all line-start line-end 'org-marked-text-position nil))
         (jump-pos (and pos (get-text-property pos 'org-marked-text-position)))
         (buffer org-marked-text-overview-source-buffer))
    (if (and jump-pos buffer (buffer-live-p buffer))
        (progn
          (pop-to-buffer buffer)
          (goto-char jump-pos)
          (org-fold-show-context 'mark-goto))
      (message "Unable to jump: %s" (cond
                                     ((not jump-pos) "No position found")
                                     ((not buffer) "Source buffer not set")
                                     (t "Source buffer no longer exists"))))))

(define-minor-mode org-marked-text-overview-mode
  "Minor mode for displaying an overview of marked text.
Supports Org and Markdown buffers."
  :lighter " OrgMTO"
  (if org-marked-text-overview-mode
      (org-marked-text-overview-update)
    (when (get-buffer org-marked-text-overview-buffer-name)
      (kill-buffer org-marked-text-overview-buffer-name))))

(provide 'org-marked-text-overview)

;;; org-marked-text-overview.el ends here
