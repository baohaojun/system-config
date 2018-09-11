;;; Tests for ac-js2

(require 'ert)
(require 'skewer-mode)
(require 'js2-mode)
(require 'ac-js2)

;;; Must have a skewer client connected before running the tests
;; Need to call httpd-stop from main Emacs if running tests in batch mode
(unless skewer-clients
  (run-skewer))

(ert-deftest ac-js2-candidates-test ()
  "Test the major function that returns candidates for all frontends."
  (let (property
        property-dot
        func-call
        var)
    (with-temp-buffer
      (insert "
  var temp = function(param1, param2) {
    var localParam = 15;
    return param1 + param2;
  };

  var look;

temp.aFun = function(lolParam) {};
temp.anotherFunction = function() { return {about: 3};}")
      (setq ac-js2-evaluate-calls t)
      (setq ac-js2-external-libraries nil)

      (js2-mode)
      (ac-js2-mode t)
      (js2-parse)

      (insert "tem")
      (ac-js2-candidates)
      (setq var ac-js2-skewer-candidates)
      (delete-char -3)

      (insert "temp.")
      (js2-parse)
      (ac-js2-candidates)
      (setq property-dot ac-js2-skewer-candidates)
      (delete-char -5)

      (insert "temp.aF")
      (js2-parse)
      (ac-js2-candidates)
      (setq property ac-js2-skewer-candidates))

    (should (assoc 'anotherFunction property-dot))
    (print property)
    (should (assoc 'aFun property))
    (should (assoc 'temp var))))

(defmacro completion-frontend-test (test-name completion-function)
  "Utility for testing completion front ends.
TODO: cover more cases"
  `(ert-deftest ,test-name ()
     (let (var)
       (with-temp-buffer
         (insert "var testComplete = function(param1, param2) {};")

         (js2-mode)
         (ac-js2-mode t)
         (js2-parse)

         (insert "testComplet")
         (funcall ',completion-function)
         (setq var (thing-at-point 'word)))
       (should (string= var "testComplete")))))

(completion-frontend-test auto-complete-test auto-complete)
(completion-frontend-test completion-at-point-test completion-at-point)
