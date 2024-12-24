;;; d2.el --- D2 diagram support -*- lexical-binding: t -*-

;; Copyright (C) 2024 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Maintainer: liuyinz <liuyinz95@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: d2 languages tree-sitter
;; Homepage: https://github.com/liuyinz/d2.el

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; See https://d2lang.com/
;; See https://codeberg.org/p8i/tree-sitter-d2.git

;;; Code:

(require 'seq)
(require 'treesit)
(require 'transient)

(declare-function treesit-parser-create "treesit.c")

(defgroup d2 nil
  "Toolchain for d2 diagram language."
  :group 'languages)

(defcustom d2-executable "d2"
  "The d2 executable path."
  :type 'string
  :group 'd2)

(defcustom d2-output-format "svg"
  "The d2 render picture format."
  :type '(choice (const :tag "render diagram in svg" "svg")
                 (const :tag "render diagram in png" "png")
                 (const :tag "render diagram in pdf" "pdf"))
  :group 'd2)

(defvar d2-format-list '("svg" "png" "pdf")
  "Supported formats in rendering output.")


;;; d2-ts-mode config

(defvar d2--treesit-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for D2 files.")

(defvar d2--treesit-font-lock-settings
  (treesit-font-lock-rules
   :default-language 'd2
   :feature 'comment
   '([(line_comment)
      (block_comment)]
     @font-lock-comment-face)

   ;; TODO wait for treesit parser nodes for d2-config
   :feature 'builtin
   '((source_file
      (container
       (container_key) @vars
       (:equal "vars" @vars)
       (block
        (container
         (container_key) @font-lock-builtin-face
         (:equal "d2-config" @font-lock-builtin-face)
         (block (shape (shape_key) @font-lock-property-name-face
                       (label) @font-lock-constant-face)))))))

   :feature 'keyword
   :override t
   `([(keyword_style)
      (keyword_classes)
      (keyword_class)
      (keyword_underscore)]
     @font-lock-keyword-face)

   :feature 'key
   '([(shape_key) (container_key)] @font-lock-function-name-face
     (attr_key) @font-lock-property-name-face)

   :feature 'operator
   '((arrow) @font-lock-operator-face)

   :feature 'number
   '([(float) (integer)] @font-lock-number-face)

   :feature 'constant
   '((boolean) @font-lock-constant-face
     ((attr_value) @font-lock-constant-face
      (:equal @font-lock-constant-face "null")))

   :feature 'variable
   '((class_name) @font-lock-variable-name-face)

   :feature 'escape
   '((escape_sequence) @font-lock-escape-face)

   :feature 'delimiter
   '([(dot) (colon) ";" "|"] @font-lock-delimiter-face)

   :feature 'bracket
   '(["[" "]" "{" "}"] @font-lock-bracket-face)

   :feature 'string
   '((label) @font-lock-type-face
     (language) @font-lock-preprocessor-face
     [(string) (attr_value) (label)] @font-lock-string-face
     (container_key
      (string
       (string_fragment) @font-lock-string-face))
     (shape_key
      (string
       (string_fragment) @font-lock-string-face)))

   :feature 'error
   '([(reserved) (ERROR)] @font-lock-warning-face))
  "Tree-sitter font-lock settings.")

(defvar d2--treesit-indent-rules
  `((d2
     ((node-is "}") parent-bol 0)
     ((parent-is "block") parent-bol 2)))
  "Tree-sitter indent rules.")

;;;###autoload
(define-derived-mode d2-ts-mode prog-mode "D2"
  "Major mode for editing D2 files, using tree-sitter library.

\\{d2-ts-mode-map}"
  :syntax-table d2--treesit-syntax-table

  (unless (treesit-ready-p 'd2)
    (error "Tree-sitter for D2 isn't available"))

  (setq-local treesit-primary-parser (treesit-parser-create 'd2))

  ;; Comments.
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local comment-end "")

  ;; Indent.
  (setq-local treesit-simple-indent-rules d2--treesit-indent-rules)

  ;; Font-lock.
  (setq-local treesit-font-lock-settings d2--treesit-font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment)
                (builtin keyword string)
                (key constant number operator delimiter variable)
                (escape bracket error)))

  (treesit-major-mode-setup))

(if (treesit-ready-p 'd2)
    (add-to-list 'auto-mode-alist
                 '("\\.d2\\'" . d2-ts-mode)))


;;; d2 functions

(defvar d2-builtin-themes
  '(("Neutral default"               0 light)
    ("Neutral Grey"                  1 light)
    ("Flagship Terrastruct"          3 light)
    ("Cool classics"                 4 light)
    ("Mixed berry blue"              5 light)
    ("Grape soda"                    6 light)
    ("Aubergine"                     7 light)
    ("Colorblind clear"              8 light)
    ("Vanilla nitro cola"          100 light)
    ("Orange creamsicle"           101 light)
    ("Shirley temple"              102 light)
    ("Earth tones"                 103 light)
    ("Everglade green"             104 light)
    ("Buttered toast"              105 light)
    ("Terminal"                    300 light)
    ("Terminal Grayscale"          301 light)
    ("Origami"                     302 light)
    ("Dark Mauve"                  200 dark)
    ("Dark Flagship Terrastruct"   201 dark))
  "Builtin themes in d2 in format of (name id style).")

(defun d2--executable-path ()
  "Return d2 executable path if found."
  (when-let* ((path (or (and (file-exists-p d2-executable) d2-executable)
                        (executable-find "d2"))))
    (abbreviate-file-name path)))

(defun d2--output-file ()
  "Return render file name for d2."
  (when-let* ((name (buffer-file-name))
              ((equal (file-name-extension name) "d2"))
              (base (file-name-base name)))
    (concat base "." d2-output-format)))

(defun d2-menu--set-port (prompt &rest _)
  "Set listening port with PROMPT."
  (when-let* ((port (read-number prompt 0)))
    (number-to-string port)))

(defun d2-menu--set-theme (prompt &rest _)
  "Set theme for d2 menu with PROMPT."
  (let ((all-themes (mapcar #'car d2-builtin-themes)))
    (when-let* ((theme (completing-read prompt all-themes)))
      (number-to-string (cadr (assoc theme d2-builtin-themes))))))

(defun d2-menu--set-dark-theme (prompt &rest _)
  "Set dark theme for d2 menu with PROMPT."
  (let ((dark-themes (mapcar #'car (seq-filter (lambda (s) (eq (nth 2 s) 'dark))
                                               d2-builtin-themes))))
    (when-let* ((theme (completing-read prompt dark-themes)))
      (number-to-string (cadr (assoc theme d2-builtin-themes))))))

(defun d2-menu--set-pad (prompt &rest _)
  "Set pixels padded around the rendered diagram with PROMPT."
  (when-let* ((pad (read-number prompt 100)))
    (number-to-string pad)))

(defun d2-menu--set-animate-interval (prompt &rest _)
  "Set transitions interval with PROMPT."
  (when-let* ((interval (read-number prompt 0)))
    (number-to-string interval)))

(defun d2-menu--set-timeout (prompt &rest _)
  "Set timeout before  D2 runs and exiting if diagram is too large with PROMPT."
  (when-let* ((timeout (read-number prompt 120)))
    (number-to-string timeout)))

(defun d2-menu--set-scale (prompt &rest _)
  "Set svg output scale with PROMPT.
0.5 to halve the default size.
-1 to fit to screen.
1 to turns off SVG fitting to screen.
all others will use their default render size."
  (when-let* ((scale (read-number prompt -1)))
    (number-to-string scale)))

(defun d2-menu--set-target (prompt &rest _)
  "Set output target with PROMPT."
  (read-string prompt))

(defun d2-menu--format-description ()
  "Show output format description."
  (concat "Toggle output format "
          (mapconcat (lambda (s) (if (string-equal s d2-output-format)
                                     (propertize s 'face 'transient-value)
                                   (propertize s 'face 'transient-key-noop)))
                     d2-format-list
                     (propertize "|" 'face 'transient-inactive-value))))


;;; Transient args

(transient-define-infix d2-menu--arg-host ()
  :argument "--host="
  :class 'transient-option
  :prompt "Set listening host (default: localhost): ")

(transient-define-infix d2-menu--arg-port ()
  :argument "--port="
  :class 'transient-option
  :prompt "Set listening port:"
  :reader #'d2-menu--set-port)

(transient-define-infix d2-menu--arg-theme ()
  :argument "--theme="
  :class 'transient-option
  :prompt "Set theme: "
  :reader #'d2-menu--set-theme)

(transient-define-infix d2-menu--arg-dark-theme ()
  :argument "--dark-theme="
  :class 'transient-option
  :prompt "Set dark theme: "
  :reader #'d2-menu--set-dark-theme)

(transient-define-infix d2-menu--arg-pad ()
  :argument "--pad="
  :class 'transient-option
  :prompt "Set padding in px: "
  :reader #'d2-menu--set-pad)

(transient-define-infix d2-menu--arg-animate-interval ()
  :argument "--animate-interval="
  :class 'transient-option
  :prompt "Set transitions interval in milliseconds: "
  :reader #'d2-menu--set-animate-interval)

(transient-define-infix d2-menu--arg-timeout ()
  :argument "--timeout="
  :class 'transient-option
  :prompt "Set timeout in seconds: "
  :reader #'d2-menu--set-timeout)

(transient-define-infix d2-menu--arg-scale ()
  :argument "--scale="
  :class 'transient-option
  :prompt "Set output scale: "
  :reader #'d2-menu--set-scale)

(transient-define-infix d2-menu--arg-target ()
  :argument "--target="
  :class 'transient-option
  :prompt "Set render target (default '*'): "
  :reader #'d2-menu--set-target)

(transient-define-infix d2-menu--var-bin ()
  :class 'transient-lisp-variable
  :description "Set executable path "
  :prompt "D2 excetuable path: "
  :variable 'd2-executable)

(transient-define-suffix d2-menu--toggle-format ()
  "Toggle `d2-output-format'."
  :transient t
  :description 'd2-menu--format-description
  (interactive)
  (setq d2-output-format
        (let* ((fmt d2-format-list)
               (cur-index (seq-position fmt d2-output-format)))
          (nth (mod (+ cur-index 1) (length fmt)) fmt))))

(transient-define-suffix d2-menu--run ()
  "Run d2 cmd with ARGS."
  (interactive)
  (let* ((cmd (d2--executable-path))
         ;; TODO support region render
         (input (file-name-nondirectory (buffer-file-name)))
         (output (d2--output-file)))
    (cond
     ((not cmd) (user-error "Can not find d2 executable"))
     ((not output) (user-error "Current file is not d2 file"))
     (t (async-shell-command
         (string-join (flatten-list (list cmd (transient-args 'd2-menu)
                                          input output)) " "))))))

(dolist (sym '(d2-menu--run d2-menu--toggle-format))
  (put sym 'completion-predicate #'ignore))

;;;###autoload (autoload 'd2-menu "d2" nil t)
(transient-define-prefix d2-menu ()
  "Transient interface for the d2 command-line tool."
  [:description "D2 menu\n"
   :class transient-subgroups
   ["Watch"
    ("-w" "Watch for changes and live reload" "--watch")
    ("-h" "Listen host" d2-menu--arg-host)
    ("-p" "Listen port" d2-menu--arg-port)
    ("-I" "Image cached" "--img-cache=")]
   ["Styles"
    ("-s" "Sketch style to render diagram" "--sketch")
    ("-l" "Layout engine" "--layout=" :choices (dagre elk tala))
    ("-t" "Theme ID" d2-menu--arg-theme)
    ("-D" "Dark theme ID" d2-menu--arg-dark-theme)
    ("-P"  "Padding in px" d2-menu--arg-pad)
    ("-R" "Target board to render" d2-menu--arg-target)]
   ["Other"
    ("-O" "Maximum number of seconds d2 runs" d2-menu--arg-timeout)
    ("-B" "Select browser to open" "--browser=")
    ("-d" "print debug logs" "--debug")]
   ["Svg"
    ;; :if (lambda () (string-equal d2-output-format "svg"))
    ("-b" "Bundle all assets and layers when output svg" "--bundle")
    ("-c" "Center the SVG in the containing viewbox" "--center")
    ("-F" "Add appendix to SVG" "--force-appendix")
    ("-A" "Set transitions interval" d2-menu--arg-animate-interval)
    ("-S" "Scale the SVG output" d2-menu--arg-scale)]
   [["Commands"
     ("b" d2-menu--var-bin)
     ("t" d2-menu--toggle-format)
     ("r" "Run d2" d2-menu--run)]]])

(provide 'd2)
;;; d2.el ends here
