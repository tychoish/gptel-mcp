;;; gptel-mcp.el --- Gptel tool-calling bridge for mcpkit -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (mcpkit "0.1.0") (gptel "0.9.0"))
;; Keywords: tools, mcp, gptel, llm
;; URL: https://github.com/tychoish/gptel-mcp

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Bridges mcpkit.el service/tool registrations onto gptel's tool-calling
;; framework, so that tools defined via `mcpkit-register-tool' can be made
;; available to an LLM through `gptel-make-tool' without re-declaring their
;; argument schemas or handlers.
;;
;; Usage:
;;
;;   (gptel-mcp-register-service-tools 'emacs)
;;
;; registers a gptel tool (via `gptel-make-tool') for every `mcpkit-tool' on
;; the `emacs' mcpkit service, grouped under a gptel tool category matching
;; the service name.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mcpkit)
(require 'gptel)

(defun gptel-mcp--keyword-name (key)
  "Return the property name string for KEY (a keyword or symbol)."
  (if (keywordp key)
      (substring (symbol-name key) 1)
    (format "%s" key)))

(defun gptel-mcp--convert-schema-node (node)
  "Recursively convert JSON-schema :type string values in NODE to symbols.
NODE is a JSON-schema plist fragment (or vector/array of such fragments), as
used in `mcpkit-tool-input-schema'.  gptel requires :type values to be
symbols (e.g. `string', `integer') rather than the JSON-schema strings
mcpkit uses internally (e.g. \"string\", \"integer\")."
  (cond
   ((vectorp node)
    (vconcat (seq-map #'gptel-mcp--convert-schema-node node)))
   ((and (consp node) (keywordp (car node)))
    (let (result)
      (cl-loop for (k v) on node by #'cddr
               do (push k result)
                  (push (if (and (eq k :type) (stringp v))
                            (intern v)
                          (gptel-mcp--convert-schema-node v))
                        result))
      (nreverse result)))
   (t node)))

(defun gptel-mcp--schema-to-args (input-schema)
  "Convert mcpkit INPUT-SCHEMA (a JSON-schema plist) into gptel :args.
Returns a list of plists suitable for `gptel-make-tool's :args slot: one
plist per JSON-schema property, with :name, :type (a symbol), :description,
and any of :enum / :items / :properties carried over (with nested :type
strings converted to symbols).  A property is marked :optional t unless its
name string appears in the schema's :required list/vector."
  (let* ((properties (plist-get input-schema :properties))
         (required (append (plist-get input-schema :required) nil))
         args)
    (cl-loop for (key spec) on properties by #'cddr
             do (let* ((prop-name (gptel-mcp--keyword-name key))
                       (converted (gptel-mcp--convert-schema-node spec))
                       (optional (not (member prop-name required)))
                       (arg (append (list :name prop-name) converted)))
                  (when optional
                    (setq arg (append arg (list :optional t))))
                  (push arg args)))
    (nreverse args)))

(defun gptel-mcp--make-tool-function (tool args)
  "Build a gptel-async :function wrapper around mcpkit TOOL.
ARGS is the gptel :args plist-list built by `gptel-mcp--schema-to-args'
for TOOL; it fixes the order in which the generated function expects its
positional argument values."
  (let ((arg-names (mapcar (lambda (a) (plist-get a :name)) args)))
    (lambda (callback &rest arg-values)
      (let (handler-args)
        (cl-loop for name in arg-names
                 for val in arg-values
                 do (setq handler-args
                          (plist-put handler-args (intern (concat ":" name)) val)))
        (funcall (mcpkit-tool-handler tool)
                 handler-args
                 (lambda (error-string result)
                   (if error-string
                       (funcall callback (format "Error: %s" error-string))
                     (funcall callback result))))))))

;;;###autoload
(defun gptel-mcp-register-service-tools (service-name)
  "Register a gptel tool for every `mcpkit-tool' on SERVICE-NAME.
SERVICE-NAME is resolved via `mcpkit-get-service' (a symbol, string, or
`mcpkit-service' instance).  Each generated gptel tool wraps the underlying
mcpkit tool's handler, translating its JSON-schema `input-schema' into
gptel's :args format and its callback-based `handler' into gptel's
asynchronous :function calling convention.  Tools are grouped in gptel's UI
under a :category matching the service name.

Return the list of `gptel-tool' structs created."
  (let ((service (mcpkit-get-service service-name)))
    (unless service
      (user-error "mcpkit service `%s' not found" service-name))
    (let ((category (format "%s" (mcpkit-service-name service)))
          created)
      (maphash
       (lambda (_name tool)
         (let* ((args (gptel-mcp--schema-to-args (mcpkit-tool-input-schema tool)))
                (function (gptel-mcp--make-tool-function tool args))
                (gtool (gptel-make-tool
                        :name (mcpkit-tool-name tool)
                        :function function
                        :description (mcpkit-tool-description tool)
                        :args args
                        :async t
                        :category category)))
           (push gtool created)))
       (mcpkit-service-tools service))
      (nreverse created))))

(provide 'gptel-mcp)
;;; gptel-mcp.el ends here
