;;; company-tern.el --- Tern backend for company-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2013-2016 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/company-tern
;; Package-Version: 20161004.1147
;; Version: 0.3.0
;; Package-Requires: ((company "0.8.0") (tern "0.0.1") (dash "2.8.0") (dash-functional "2.8.0") (s "1.9.0") (cl-lib "0.5.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the README for more details.

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'tern)
(require 'dash)
(require 'dash-functional)
(require 's)

(defgroup company-tern nil
  "Tern backend for company-mode"
  :group 'languages
  :prefix "company-tern-")

(defcustom company-tern-property-marker " ○"
  "A string to indicate an object's own properties.
This also can be nil to disable property markers."
  :type '(choice (string :tag "Property suffix")
                 (const :tag "None" nil))
  :group 'company-tern)

(defcustom company-tern-meta-as-single-line nil
  "Trim candidate type information to frame width?"
  :type 'boolean
  :group 'company-tern)

(defvar company-tern--debug-print-enabled nil
  "Non-nil enables debug print.")

(defun company-tern-in-string-or-comment ()
  "Return non-nil if point is inside a string or comment."
  (let* ((lst (parse-partial-sexp (point-min) (point)))
         ;; start of comment or string
         (pos (nth 8 lst))
         (ch (and pos (char-after pos))))
    (cond
     ((nth 4 lst)
      ;; We are inside comment.
      t)
     ((null (nth 3 lst))
      ;; We are outside a string.
      nil)
     ;; Now, we are inside a string.
     ((and pos (not (char-equal ch ?`)))
      ;; " or '
      t)
     ((and pos
           (char-equal ch ?`)
           (company-tern--inside-template-p
            (buffer-substring-no-properties (1+ pos) (point))))
      ;; Looks like we have "`...${" in fron of us.
      nil)
     (t
      t))))

(defun company-tern--inside-template-p (string)
  "Return non-nil if point is likely to be in template string."
  (cond
   ((not (string-match "${\\(.*\\)\\'" string))
    nil)
   (t
    ;; "`[...${......]"
    ;;         ^^^^^^
    ;;         If this portion contains unbalanced "\"" or "'", we
    ;;         assume we are in a template.
    (let* ((s (match-string-no-properties 1 string))
           (lst (split-string s "" t))
           (double-quote-cnt (cl-count "\"" lst :test #'string=))
           (single-quote-cnt (cl-count "'" lst :test #'string=)))
      (and (cl-evenp double-quote-cnt)
           (cl-evenp single-quote-cnt))))))

(defun company-tern-prefix ()
  "Grab prefix for tern."
  (and tern-mode
       (not (company-tern-in-string-or-comment))
       (or (company-grab-symbol-cons "\\." 1)
           'stop)))

(defun company-tern-candidates-query (prefix callback)
  "Retrieve PREFIX completion candidates from tern.
Use CALLBACK function to display candidates."
  (tern-run-query
   (lambda (data)
     (funcall callback
              (company-tern-sort-by-depth
               (company-tern-format-candidates data))))
   '((type . "completions")
     (includeKeywords . t)
     (depths . t)
     (types . t)
     (docs . t))
   (point)))

(defun company-tern-format-candidates (data)
  "Grab candidates with properties from tern DATA."
  (let ((completions (cdr (assq 'completions data)))
        (property-p (assq 'isProperty data)))
    (mapcar
     (lambda (completion)
       (let ((candidate (cdr (assq 'name completion))))
         (dolist (prop (push property-p completion))
           (put-text-property 0 1 (car prop) (cdr prop) candidate))
         candidate))
     completions)))

(defun company-tern-sort-by-depth (candidates)
  "Sort CANDIDATES list by completion depth."
  (-sort (-on '< 'company-tern-depth) candidates))

(defun company-tern-depth (candidate)
  "Return depth attribute for CANDIDATE."
  (get-text-property 0 'depth candidate))

(defun company-tern-property-p (candidate)
  "Return t if CANDIDATE is object own property."
  (and (null (eq json-false (get-text-property 0 'isProperty candidate)))
       (eq 0 (company-tern-depth candidate))))

(defun company-tern-keyword-p (candidate)
  "Return t if CANDIDATE is a keyword."
  (get-text-property 0 'isKeyword candidate))

(defun company-tern-function-p (candidate)
  "Return t if CANDIDATE is a function."
  (--when-let (get-text-property 0 'type candidate)
    (s-starts-with? "fn(" it)))

(defun company-tern-doc (candidate)
  "Return documentation buffer for CANDIDATE."
  (--when-let (get-text-property 0 'doc candidate)
    (company-doc-buffer it)))

(defun company-tern-meta (candidate)
  "Return short documentation string for chosen CANDIDATE."
  (--when-let (get-text-property 0 'type candidate)
    (if company-tern-meta-as-single-line
        (s-left (frame-width) it)
      it)))

(defun company-tern-annotation (candidate)
  "Return type annotation for chosen CANDIDATE."
  (--when-let (company-tern-get-type candidate)
    (concat it (and (company-tern-property-p candidate)
                    company-tern-property-marker))))

(defun company-tern-get-type (candidate)
  "Analyze CANDIDATE type."
  (unless (company-tern-keyword-p candidate)
    (if (company-tern-function-p candidate)
        (company-tern-function-type candidate)
      (company-tern-variable-type candidate))))

(defun company-tern-function-type (candidate)
  "Get CANDIDATE type as a function."
  (-when-let* ((type (get-text-property 0 'type candidate))
               (annot (if company-tooltip-align-annotations "fn(%s)" "(%s)")))
    (->> (list (cons 'type type))
      (tern-parse-function-type)
      (cadr)
      (--map (car it))
      (-interpose ", ")
      (apply 'concat)
      (format annot))))

(defun company-tern-variable-type (candidate)
  "Get CANDIDATE type as a variable."
  (-when-let* ((type (get-text-property 0 'type candidate))
               (annot (if company-tooltip-align-annotations "%s" " -> %s")))
    (format annot type)))

;;;###autoload
(defun company-tern (command &optional arg &rest _args)
  "Tern backend for company-mode.
See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tern))
    (prefix (company-tern-prefix))
    (annotation (company-tern-annotation arg))
    (meta (company-tern-meta arg))
    (doc-buffer (company-tern-doc arg))
    (ignore-case t)
    (sorted t)
    (candidates (cons :async
                      (lambda (callback)
                        (company-tern-candidates-query arg callback))))))

;; fn(glob: store, opt: store) -> DestroyableTransform|fn(override: ?) -> Through2
(defun company-tern---split-string-by-toplevel-comma (s)
  "Split string S by toplevel SEPARATORS.

Examples:
\"a, g(x, y), b\" => (\"a\" \"g(x, y)\" \"b\")
"
  (cl-loop for ch in (split-string s "" t)
           with depth = 0
           with acc = nil
           with ret = nil
           with separator = "[,]"
           do (cond
               ((string-match-p separator ch)
                (cond
                 ((zerop depth)
                  (when acc (push (cl-reduce #'concat (nreverse acc)) ret))
                  (setq acc nil))
                 ((> depth 0)
                  (push ch acc))))
               ((string-match-p "[(]" ch)
                (incf depth)
                (push ch acc))
               ((string-match-p "[)]" ch)
                (decf depth)
                (push ch acc))
               (t
                (push ch acc)))
           finally (progn
                     (when acc
                       (push (cl-reduce #'concat (nreverse acc)) ret))
                     (return (mapcar #'s-trim (nreverse ret))))))

;; gulp.src(glob: store, opt: store -> DestroyableTransform|fn(override: ?})
(cl-defun company-tern--debug-print (format-string &rest args &key (force nil) &allow-other-keys)
  ;; We do not need keyword argument because we bother to manually
  ;; parse args to find out whether FORCE is speficed or not. However,
  ;; to indicate that you can pass `:force' to force debug print, we
  ;; put it in the argument list, which ends up in forcing us to put
  ;; `allow-other-keys' keyword in the argument list, OR I just do not
  ;; know another way to implement the same functionality.
  (let* ((pos (cl-position :force args))
         (force (and pos (< (1+ pos) (length args)) (nth (1+ pos) args)))
         (args (if pos (append (cl-subseq args 0 pos) (cl-subseq args (+ pos 2))) args)))
    (when (or company-tern--debug-print-enabled force)
      (message "[company-tern]: %s" (apply #'format format-string args)))))

(defun company-tern--expand-snippet (candidate)
  "Construct a template for yasnippet from CANDIDATE and expand it."
  (ignore-errors
    (let* ((type (get-text-property 0 'type candidate))
           (template nil)
           (ix 0))
      (company-tern--debug-print "type=%s" type)
      (cond
       ((string-match "^fn(\\(.*\\))" type)
        (setq template
              (concat
               "("
               (cl-reduce
                (lambda (a b)
                  (concat a
                          (if (string-match-p "[0-9A-Za-z_]\\?:" b)
                              ;; this argument is optional, so we make the
                              ;; separator itself, (which is ", "), a field in
                              ;; template.  In this way, we can skip the rest of
                              ;; arguments with "C-d" when we do not need those
                              ;; optional arguments.
                              (concat (if (zerop ix)
                                          ""
                                        (format "${%d:, }" (incf ix)))
                                      (format "${%d:%s}" (incf ix) b))
                            (format "%s${%d:%s}"
                                    (if (> (length a) 0) ", " "")
                                    (incf ix) b))))
                (company-tern---split-string-by-toplevel-comma
                 (company-tern--extract-arguments type))
                :initial-value "")
               ")$0"))
        (company-tern--debug-print "template=%s" template)
        (yas-expand-snippet template))
       (t
        nil)))))

;; fn(glob: store, opt: store) -> DestroyableTransform|fn(override: ?) -> Through2
;; => glob: store, opt: store
(defun company-tern--extract-arguments (type)
  (let ((beg nil))
    (with-temp-buffer
      (insert type)
      (goto-char (point-min))
      (cl-assert (search-forward "("))
      (backward-char 1)
      (setq beg (1+ (point)))
      ;; We are on the open paren.
      (buffer-substring beg (1- (scan-lists (point) 1 0))))))

;;;###autoload
(defun company-tern-with-yasnippet (command &optional arg &rest _args)
  "Another tern backend for company-mode. This backend assumes that
you have yasnippet installed and available on your environment.

On inserting a candidate having a signiture, such as functions, its
arguments will be inserted and expanded using yasnippet.

See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tern))
    (prefix (company-tern-prefix))
    (annotation (company-tern-annotation arg))
    (meta (company-tern-meta arg))
    (doc-buffer (company-tern-doc arg))
    (ignore-case t)
    (sorted t)
    (candidates (cons :async
                      (lambda (callback)
                        (company-tern-candidates-query arg callback))))
    (post-completion (company-tern--expand-snippet arg))))

(provide 'company-tern)

;;; company-tern.el ends here
