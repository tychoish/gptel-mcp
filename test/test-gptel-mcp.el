;;; test-gptel-mcp.el --- Tests for gptel-mcp -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'mcpkit)
(require 'gptel-mcp)

;;; JSON-schema -> gptel :args conversion

(ert-deftest test-gptel-mcp/schema-to-args-required-vs-optional ()
  "Required properties get no :optional, missing ones get :optional t."
  (let* ((schema '(:type "object"
                   :properties (:foo (:type "string" :description "a string")
                                :bar (:type "integer" :description "an int"))
                   :required ["foo"]))
         (args (gptel-mcp--schema-to-args schema))
         (foo (cl-find "foo" args :key (lambda (a) (plist-get a :name)) :test #'equal))
         (bar (cl-find "bar" args :key (lambda (a) (plist-get a :name)) :test #'equal)))
    (should (= (length args) 2))
    (should (eq (plist-get foo :type) 'string))
    (should (null (plist-get foo :optional)))
    (should (eq (plist-get bar :type) 'integer))
    (should (eq (plist-get bar :optional) t))))

(ert-deftest test-gptel-mcp/schema-to-args-types ()
  "Covers string/integer/boolean/array JSON-schema type conversion."
  (let* ((schema '(:type "object"
                   :properties (:a (:type "string" :description "s")
                                :b (:type "integer" :description "i")
                                :c (:type "boolean" :description "b")
                                :d (:type "array" :description "a"
                                    :items (:type "string")))
                   :required ["a" "b" "c" "d"]))
         (args (gptel-mcp--schema-to-args schema)))
    (should (eq (plist-get (cl-find "a" args :key (lambda (x) (plist-get x :name)) :test #'equal) :type) 'string))
    (should (eq (plist-get (cl-find "b" args :key (lambda (x) (plist-get x :name)) :test #'equal) :type) 'integer))
    (should (eq (plist-get (cl-find "c" args :key (lambda (x) (plist-get x :name)) :test #'equal) :type) 'boolean))
    (let ((d (cl-find "d" args :key (lambda (x) (plist-get x :name)) :test #'equal)))
      (should (eq (plist-get d :type) 'array))
      (should (eq (plist-get (plist-get d :items) :type) 'string)))))

(ert-deftest test-gptel-mcp/schema-to-args-no-required ()
  "When :required is absent, all properties are optional."
  (let* ((schema '(:type "object"
                   :properties (:only (:type "string" :description "s"))))
         (args (gptel-mcp--schema-to-args schema)))
    (should (= (length args) 1))
    (should (eq (plist-get (car args) :optional) t))))

;;; Round-trip registration and invocation

(ert-deftest test-gptel-mcp/round-trip-success ()
  "Register a synchronous mcpkit tool and invoke through the gptel wrapper."
  (let ((mcpkit-registry nil)
        (gptel--known-tools nil)
        (gptel-tools nil))
    (mcpkit-define-service 'gtest :description "gptel test service")
    (mcpkit-register-tool 'gt_add 'gtest
      :description "Add two integers."
      :input-schema '(:type "object"
                      :properties (:x (:type "integer" :description "first addend")
                                   :y (:type "integer" :description "second addend"))
                      :required ["x" "y"])
      (+ (plist-get args :x) (plist-get args :y)))
    (let* ((created (gptel-mcp-register-service-tools 'gtest))
           (gtool (car created))
           (result nil))
      (should (= (length created) 1))
      (should (equal (gptel-tool-name gtool) "gt_add"))
      (should (equal (gptel-tool-category gtool) "gtest"))
      (should (gptel-tool-async gtool))
      (let ((args-spec (gptel-tool-args gtool)))
        (should (= (length args-spec) 2)))
      (funcall (gptel-tool-function gtool)
               (lambda (r) (setq result r))
               3 4)
      (should (equal result 7)))))

(ert-deftest test-gptel-mcp/round-trip-error ()
  "A handler that reports an error via DONE surfaces a clear error string."
  (let ((mcpkit-registry nil)
        (gptel--known-tools nil)
        (gptel-tools nil))
    (mcpkit-define-service 'gtest-err :description "gptel error test service")
    (mcpkit-register-tool 'gt_fail 'gtest-err
      :description "Always fails."
      :input-schema '(:type "object"
                      :properties (:msg (:type "string" :description "input"))
                      :required ["msg"])
      :async t
      (funcall done "deliberate failure" nil))
    (let* ((created (gptel-mcp-register-service-tools 'gtest-err))
           (gtool (car created))
           (result nil))
      (funcall (gptel-tool-function gtool)
               (lambda (r) (setq result r))
               "irrelevant")
      (should (stringp result))
      (should (string-match-p "deliberate failure" result))
      (should (string-match-p "\\`Error" result)))))

(provide 'test-gptel-mcp)
;;; test-gptel-mcp.el ends here
