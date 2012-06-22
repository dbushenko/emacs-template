;; emacs-template.el -- moustache-like templating system.
;; Copyright (C) 2012 by Dmitry Bushenko (d.bushenko@gmail.com).
;; Published under BSD license.

;; Usage example:
;; (load-file "emacs-template.el")
;;
;; (setq templ "
;; public class {{class}} {
;; // Fields
;; {{#fields}}
;; private {{type}} {{name}};{{/fields}}
;;
;; // Accessors
;; {{#fields}}
;; {{>method}}
;; {{/fields}}
;;
;; // Methods
;; {{^toString}}
;; public String toString() {
;;   return {{#fields}}String.valueOf({{name}}){{^last}}+{{/last}}{{/fields}};
;; }
;; {{/toString}}
;;
;; {{#hidden}}
;; This section will never show up!
;; {{/hidden}}
;; }")
;;
;; (setq method "public {{type}} get{{name}}() {
;; return {{name}};
;; }
;;
;; public void set{{name}}({{type}} val}} {
;; {{name}} = val;
;; }")
;;
;; (render-template templ
;; 		  (hash-map "fields" (list
;; 				      (hash-map "name" "Name"
;; 						"type" "String")
;; 				      (hash-map "name" "Age"
;; 						"type" "int"
;; 						"last" t))
;; 			    "class" "Person"
;; 			    "method" method
;; 			    "toString" nil
;; 			    "hidden" nil
;; 			    ))


;; Constants

;; This delimiters are used within the templates
(setq *emtempl/left-delimiter* "{{")
(setq *emtempl/right-delimiter* "}}")

;; Utilities

(defun emtempl/left-delim-length ()
  (length *emtempl/left-delimiter*))

(defun emtempl/right-delim-length ()
  (length *emtempl/right-delimiter*))

(defun emtempl/sect-title (name)
  "Section title including delimiters"
  (concat *emtempl/left-delimiter* name *emtempl/right-delimiter*))

(defun emtempl/sect-length (name)
  "Section title length including delimiters"
  (length (emtempl/sect-title name)))

(defun emtempl/hash-keys (hashtable)
  "Return all keys in hashtable."
  (let (allkeys)
    (maphash (lambda (kk vv) (setq allkeys (cons kk allkeys))) hashtable)
    allkeys))

(defun emtempl/concat-hash-maps (hm1 hm2)
  "Takes two hash tables and returns one, containing all their keys and values."
  (let* ((new-hm (copy-hash-table hm1))
	 (keys (emtempl/hash-keys hm2))
	 (r (mapcar (lambda (k) (puthash k (gethash k hm2) new-hm)) keys)))
    new-hm))

(defun emtempl/range (n lst)
  "Range generator. Usage (emtempl/range 3 nil) -> '(1 2 3)"
  (if (zerop n)
      lst
    (emtempl/range (- n 1) (cons n lst))))

(defun emtempl/ends-with (s ending)
  "return non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
    (string= (substring s (- 0 elength)) ending)))

(defun emtempl/starts-with (s arg)
  "returns non-nil if string S starts with ARG.  Else nil."
  (cond ((>= (length s) (length arg))
	 (string-equal (substring s 0 (length arg)) arg))
	(t nil)))

(defun emtempl/make-hash-parameters (params)
  "Usage: (emtempl/make-hash-parameters '(1 2 3 4 5 6)) -> '((1 2) (3 4) (5 6))"
  (let* ((list1 (delq :no-value (mapcar* (lambda (p n) (if (zerop (% n 2)) :no-value p))
					 params (emtempl/range (length params) nil))))
	 (list2 (delq :no-value (mapcar* (lambda (p n) (if (zerop (% n 2)) p :no-value))
					 params (emtempl/range (length params) nil)))))
    (mapcar* 'list list1 list2)))

(defun emtempl/variablep (text)
  "Checks whether the title is variable"
  (and (not (emtempl/starts-with text "#"))
       (not (emtempl/starts-with text "/"))
       (not (emtempl/starts-with text "-"))
       (not (emtempl/starts-with text ">"))
       (not (emtempl/starts-with text "^"))))

(defun emtempl/beginp (text)
  "Checks whether the title is section beginning"
  (or (emtempl/starts-with text "#")
      (emtempl/starts-with text "^")))

(defun emtempl/endp (text)
  "Checks whether the title is section ending"
  (or (emtempl/starts-with text "/")
      (emtempl/starts-with text "-")))

(defun emtempl/includep (text)
  "Checks whether the title is section including"
  (emtempl/starts-with text ">"))

(defun emtempl/invertp (text)
  "Checks whether the title is section inverting"
  (emtempl/starts-with text "^"))

(defun emtempl/title (text)
  "Gets the section title"
  (if (or (emtempl/starts-with text "#")
	  (emtempl/starts-with text "/")
	  (emtempl/starts-with text "-")
	  (emtempl/starts-with text ">")
	  (emtempl/starts-with text "^"))
      (substring text 1)
    text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Template renderer functions

(defun emtempl/find-next-section (text)
  "Finds next section tag {{section}}"
  (let* ((left-length (emtempl/left-delim-length))
	 (right-length (emtempl/right-delim-length))
	 (start (string-match *emtempl/left-delimiter* text 0))
	 (end (if (not (null start))
		  (string-match *emtempl/right-delimiter* text (+ start left-length))
		nil)))
    (if (and (not (null start)) (not (null end)))
	(hash-map :title (substring text (+ start left-length) end)
		  :start start
		  :end (+ end right-length)))))

(defun emtempl/find-full-section (text name index)
  "Finds the correspoinding closing section tag {{/section}}"
  (let* ((ind (string-match (emtempl/sect-title name) text index)))
    (if (null ind) nil
      (hash-map :title name
		:start index
		:end (+ ind (emtempl/sect-length name))))))

(defun emtempl/process-variable (template sect params)
  "Substitutes the found variable with the specified parameter"
  (let* ((before (substring template 0 (gethash :start sect)))
	 (after (substring template (gethash :end sect)))
	 (var-name (gethash :title sect))
	 (var-value (gethash var-name params)))
    (concat before var-value after)))

(defun emtempl/process-include (template sect params)
  "Substitutes the found variable with the specified template"
  (let* ((before (substring template 0 (gethash :start sect)))
	 (after (substring template (gethash :end sect)))
	 (var-name (emtempl/title (gethash :title sect)))
	 (var-value (gethash var-name params)))
    (concat before var-value after)))

(defun emtempl/process-fragment-part (var-value params text)
  "Renders the fragment using the default parameters and the parameters of the section."
  (let* ((new-params (emtempl/concat-hash-maps params var-value))
	 (processed-text (render-template text new-params)))
    processed-text))

(defun emtempl/checked-full-sect (name)
  "Finds the nearest closing section."
  (let* ((fs1 (emtempl/find-full-section template (concat "/" name) (gethash :end sect)))
	 (fs2 (emtempl/find-full-section template (concat "-" name) (gethash :end sect))))
    (cond
     ((and (null fs2) (not (null fs1))) fs1)
     ((and (null fs1) (not (null fs2))) fs2)
     ((and (null fs1) (null fs2)) nil)
     ((> (gethash :end fs1) (gethash :end fs2)) fs2)
     ((< (gethash :end fs1) (gethash :end fs2)) fs1))))

(defun emtempl/process-fragment (template sect params inverted)
  "Substitutes the found variable with the specified parameter"
  ;; Check if it's a non-empty list -- run as list
  ;; Check if it's a hash -- run as hash
  ;; If nil -- do nothing
  ;;
  ;; Find corresponding ending. Create template from the text between.
  (let* ((var-name (gethash :title sect))
	 (name (emtempl/title var-name))
	 (var-value (gethash name params))
	 (full-sect (emtempl/checked-full-sect name))
	 (delta (if (string= (substring (gethash :title full-sect) 0 1) "-") -1 0))
	 (section-title-length (+ 1 (emtempl/sect-length name)))
	 (text (substring template
			  (gethash :start full-sect)
			  (- (+ (gethash :end full-sect) delta)
			     section-title-length)))
	 (before (substring template 0 (- (gethash :start full-sect) section-title-length)))
	 (after (substring template (gethash :end full-sect))))
    (cond
     ((and (null var-value) (not inverted))
      (concat before after))
     
     ((and (null var-value) inverted)
      (concat before text after))
     
     ((and (hash-table-p var-value) (not inverted))
      (concat before (emtempl/process-fragment-part var-value params text) after))
     
     ((and (listp var-value) (not inverted))
      (concat before
	      (reduce 'concat
		      (mapcar
		       (lambda (p) (emtempl/process-fragment-part p params text))
		       var-value))
	      after))

     (inverted (concat before after))     
     (t (concat before text after)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exported functions

(defun hash-map (&rest raw-params)
  "Creates the hash table from the parameters.
Usage: (hash-map :key1 111 :key2 222)"
  (let ((h (make-hash-table :test 'equal))
	(params (emtempl/make-hash-parameters raw-params)))
    (mapcar (lambda (p) (puthash (car p) (car (cdr p)) h)) params)
    h))

(defun render-template (template params)
  "Renders the template using the specified parameters."
  (let* ((sect (emtempl/find-next-section template))
	 (title (if (not (null sect)) (gethash :title sect))))
    (cond
     ((null sect)
      template)
     ((emtempl/variablep title)
      (render-template (emtempl/process-variable template sect params) params))
     ((and (emtempl/beginp title) (emtempl/invertp title))
      (render-template (emtempl/process-fragment template sect params t) params))
     ((and (emtempl/beginp title) (not (emtempl/invertp title)))
      (render-template (emtempl/process-fragment template sect params nil) params))
     ((emtempl/includep title)
      (render-template (emtempl/process-include template sect params) params))
     (t (error (concat "Invalid section: " title))))))

(defun set-emtempl-delimiters (left right)
  "Sets the delimiters"
  (setq *emtempl/left-delimiter* left)
  (setq *emtempl/right-delimiter* right))

(defun get-emtempl-delimiters ()
  "Gets the delimiters"
  (list *emtempl/left-delimiter* *emtempl/right-delimiter*))
