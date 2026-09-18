;;; agent-shell-dashboard-projectile.el --- Projectile support for agent-shell-dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Wanderson Ferreira

;; Author: Wanderson Ferreira
;; Maintainer: Wanderson Ferreira
;; URL: https://github.com/wandersoncferreira/agent-shell-dashboard
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (projectile "2.0.0"))
;; Keywords: convenience, tools, project

;; This file is not part of GNU Emacs.

;; Released under the MIT License; see the LICENSE file for details.

;;; Commentary:

;; Optional Projectile integration for `agent-shell-dashboard'.  Enabling
;; `agent-shell-dashboard-projectile-mode' groups live sessions by Projectile
;; project.  Opening a session also switches to its Doom workspace when Doom's
;; workspace API is available.
;;
;; Usage:
;;
;;   (require 'agent-shell-dashboard-projectile)
;;   (agent-shell-dashboard-projectile-mode 1)
;;
;; Workspace switching follows Doom's ibuffer behavior: prefer a workspace
;; which already owns the live session buffer, then one associated with the
;; Projectile root, and finally create or reuse the project's workspace.

;;; Code:

(require 'agent-shell-dashboard)
(require 'projectile)
(require 'seq)
(require 'subr-x)

(declare-function +workspace-contains-buffer-p nil (buffer &optional workspace))
(declare-function +workspace-list nil ())
(declare-function +workspace-switch nil (name &optional auto-create-p))
(declare-function +workspaces-switch-to-project-h nil (&optional dir))
(declare-function persp-add-buffer "persp-mode" (buffer-or-name &optional persp))
(declare-function persp-name "persp-mode" (persp))
(declare-function persp-parameter "persp-mode" (parameter &optional persp))

(defvar +workspaces-on-switch-project-behavior)
(defvar +workspaces-switch-project-function)
(defvar persp-mode)

(defgroup agent-shell-dashboard-projectile nil
  "Projectile integration for `agent-shell-dashboard'."
  :group 'agent-shell-dashboard
  :prefix "agent-shell-dashboard-projectile-")

(defcustom agent-shell-dashboard-projectile-switch-workspace-function
  #'agent-shell-dashboard-projectile-switch-doom-workspace
  "Function called before opening a dashboard session.
The function receives PROJECT-ROOT and BUFFER.  BUFFER is nil when reopening a
closed session.  The default switches Doom workspaces when `persp-mode' and
Doom's workspace API are active; otherwise it does nothing."
  :type 'function)

(defun agent-shell-dashboard-projectile--root-for-directory (directory)
  "Return normalized Projectile root containing DIRECTORY, or nil."
  (when directory
    (when-let* ((root (ignore-errors (projectile-project-root directory))))
      (file-name-as-directory (expand-file-name root)))))

(defun agent-shell-dashboard-projectile--root (buffer)
  "Return normalized Projectile root for BUFFER, or nil."
  (when (buffer-live-p buffer)
    (agent-shell-dashboard-projectile--root-for-directory
     (agent-shell-dashboard--cwd buffer))))

(defun agent-shell-dashboard-projectile--name (root)
  "Return display name for Projectile ROOT."
  (if root
      (or (ignore-errors (projectile-project-name root))
          (file-name-nondirectory (directory-file-name root)))
    "Other"))

(defun agent-shell-dashboard-projectile--groups (buffers)
  "Group BUFFERS by Projectile root, sorted by project name.
Buffer order inside each group remains unchanged."
  (let (groups)
    (dolist (buffer buffers)
      (let* ((root (agent-shell-dashboard-projectile--root buffer))
             (group (assoc root groups)))
        (if group
            (setcdr group (append (cdr group) (list buffer)))
          (push (list root buffer) groups))))
    (sort groups
          (lambda (left right)
            (let ((left-root (car left))
                  (right-root (car right)))
              (cond
               ((null left-root) nil)
               ((null right-root) t)
               (t (string-lessp
                   (downcase (agent-shell-dashboard-projectile--name left-root))
                   (downcase (agent-shell-dashboard-projectile--name right-root))))))))))

(defun agent-shell-dashboard-projectile--insert-project-heading (root)
  "Insert project subheading for ROOT."
  (insert "\n  ")
  (agent-shell-dashboard--insert
   (agent-shell-dashboard-projectile--name root)
   'face 'agent-shell-dashboard-heading-projects)
  (when root
    (agent-shell-dashboard--insert
     (format "  %s" (abbreviate-file-name (directory-file-name root)))
     'face 'agent-shell-dashboard-dim))
  (insert "\n"))

(defun agent-shell-dashboard-projectile--insert-sessions (_original buffers)
  "Insert BUFFERS grouped by Projectile project.
_ORIGINAL is the advised `agent-shell-dashboard--insert-sessions' function."
  (agent-shell-dashboard--insert-heading
   "Sessions" 'agent-shell-dashboard-heading-sessions "— grouped by project")
  (if (null buffers)
      (agent-shell-dashboard--insert
       "  No agent-shell sessions. Press c to start one.\n"
       'face 'agent-shell-dashboard-dim)
    (dolist (group (agent-shell-dashboard-projectile--groups buffers))
      (agent-shell-dashboard-projectile--insert-project-heading (car group))
      (dolist (buffer (cdr group))
        (agent-shell-dashboard--insert-session-row buffer)))))

(defun agent-shell-dashboard-projectile--workspace-name (workspace)
  "Return name of Doom WORKSPACE, or nil."
  (when (fboundp 'persp-name)
    (persp-name workspace)))

(defun agent-shell-dashboard-projectile--workspace-for-buffer (buffer)
  "Return Doom workspace containing BUFFER, asking if several contain it."
  (when (and buffer
             (fboundp '+workspace-list)
             (fboundp '+workspace-contains-buffer-p))
    (let ((workspaces
           (seq-filter
            (lambda (workspace)
              (+workspace-contains-buffer-p buffer workspace))
            (+workspace-list))))
      (cond
       ((null workspaces) nil)
       ((null (cdr workspaces)) (car workspaces))
       (t
        (let* ((choices
                (mapcar (lambda (workspace)
                          (cons (agent-shell-dashboard-projectile--workspace-name
                                 workspace)
                                workspace))
                        workspaces))
               (name (completing-read "Select workspace: " choices nil t)))
          (cdr (assoc name choices))))))))

(defun agent-shell-dashboard-projectile--same-root-p (left right)
  "Return non-nil when LEFT and RIGHT name same project root."
  (and left right
       (or (ignore-errors (file-equal-p left right))
           (string-equal
            (directory-file-name (expand-file-name left))
            (directory-file-name (expand-file-name right))))))

(defun agent-shell-dashboard-projectile--workspace-for-root (root)
  "Return Doom workspace associated with Projectile ROOT, or nil."
  (when (and root (fboundp '+workspace-list) (fboundp 'persp-parameter))
    (seq-find
     (lambda (workspace)
       (when-let* ((workspace-root
                    (persp-parameter '+workspace-project workspace)))
         (agent-shell-dashboard-projectile--same-root-p workspace-root root)))
     (+workspace-list))))

(defun agent-shell-dashboard-projectile-switch-doom-workspace (root buffer)
  "Switch to Doom workspace for Projectile ROOT and optional live BUFFER.
Do nothing outside Doom's `persp-mode' workspace setup."
  (when (and root
             (bound-and-true-p persp-mode)
             (fboundp '+workspace-switch))
    (let ((workspace
           (or (agent-shell-dashboard-projectile--workspace-for-buffer buffer)
               (agent-shell-dashboard-projectile--workspace-for-root root))))
      (cond
       (workspace
        (+workspace-switch
         (agent-shell-dashboard-projectile--workspace-name workspace)))
       ((fboundp '+workspaces-switch-to-project-h)
        ;; already opens selected session, so force project workspace creation
        ;; and suppress Doom's extra project-open action.
        (let ((+workspaces-on-switch-project-behavior t)
              (+workspaces-switch-project-function #'ignore))
          (+workspaces-switch-to-project-h root))))
      (when (and buffer (fboundp 'persp-add-buffer))
        (persp-add-buffer buffer)))))

(defun agent-shell-dashboard-projectile--switch-before-open (&rest _)
  "Switch to selected session's project workspace before dashboard opens it."
  (let* ((buffer (agent-shell-dashboard--buffer-at-point))
         (session (agent-shell-dashboard--session-at-point))
         (root (if buffer
                   (agent-shell-dashboard-projectile--root buffer)
                 (and session
                      (agent-shell-dashboard-projectile--root-for-directory
                       (plist-get session :cwd))))))
    (when (and root
               (functionp
                agent-shell-dashboard-projectile-switch-workspace-function))
      (funcall agent-shell-dashboard-projectile-switch-workspace-function
               root buffer))))

;;;###autoload
(define-minor-mode agent-shell-dashboard-projectile-mode
  "Group dashboard sessions by Projectile project and switch workspaces.
This global mode advises dashboard rendering and session opening."
  :global t
  :group 'agent-shell-dashboard-projectile
  (if agent-shell-dashboard-projectile-mode
      (progn
        (unless (advice-member-p
                 #'agent-shell-dashboard-projectile--insert-sessions
                 'agent-shell-dashboard--insert-sessions)
          (advice-add 'agent-shell-dashboard--insert-sessions :around
                      #'agent-shell-dashboard-projectile--insert-sessions))
        (unless (advice-member-p
                 #'agent-shell-dashboard-projectile--switch-before-open
                 'agent-shell-dashboard-open)
          (advice-add 'agent-shell-dashboard-open :before
                      #'agent-shell-dashboard-projectile--switch-before-open)))
    (advice-remove 'agent-shell-dashboard--insert-sessions
                   #'agent-shell-dashboard-projectile--insert-sessions)
    (advice-remove 'agent-shell-dashboard-open
                   #'agent-shell-dashboard-projectile--switch-before-open))
  (when (get-buffer agent-shell-dashboard-buffer-name)
    (agent-shell-dashboard-refresh)))

(provide 'agent-shell-dashboard-projectile)
;;; agent-shell-dashboard-projectile.el ends here
