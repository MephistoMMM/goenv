;;; goenv.el --- provide funcs and mode for creating temporary workspace environment

;; Copyright (C) 2017-2018 Mephis Pheies <mephistommm@gmail.com>

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/goenv
;; Version: 1.0
;; Keywords: golang, environment, tools

;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a simple global minor mode which will modify the GOPATH inside Emacs.

;; The main entry points are `goenv-activate', which queries the user
;; for a go workspace environment directory to activate.

;;; Code:
(defgroup goenv nil
  "Golang Temporary Workspace Environment Interface."
  :prefix "goenv-"
  :group 'languages)

(defvar goenv-temporary-env nil
  "The current go template workspace environment.

Do not set this variable directly; use `goenv-activate' or
`goenv-workon'.")

;; TODO show temporary workspace name.
(defvar goenv-temporary-env-name nil
  "The name of the current temporary environment.

This is usually the base name of `goenv-temporary-env'.")

(defvar goenv-old-process-environment nil
  "The old process environment before the last activate.")

(defvar goenv-mode-line-indicator '(goenv-temporary-env-name
                                     ("[" goenv-temporary-env-name "] "))
  "How `goenv-mode' will indicate the current environment in the mode line.")

;;;###autoload
(defun goenv-activate (directory)
  "Activate the go temporary workspace environment in DRECTORY."
  (interactive "DActivate goenv: ")
  (setq directory (expand-file-name directory)
        goenv-temporary-env-name (file-name-nondirectory directory))
  (goenv-deactivate)
  (setq goenv-old-process-environment process-environment)
  (let ((path-items (split-string (getenv "GOPATH") ":")))
    (setq process-environment (append
                               (list
                                (format "GOPATH=%s"
                                        (mapconcat 'identity
                                                   (append (list (car path-items))
                                                           (list directory)
                                                           (cdr path-items))
                                                   ":")))
                               process-environment))
    )
  (setq goenv-temporary-env t)
  )

;;;###autoload
(defun goenv-deactivate ()
  "Deactivate any current go temporary workspace environment."
  (interactive)
  (when goenv-old-process-environment
    (setq process-environment goenv-old-process-environment
          goenv-old-process-environment nil))
  (setq goenv-temporary-env nil)
  )

(provide 'goenv)
;;; goenv.el ends here
