;; Example DSL for creating MVC-code for android project. See usage in 'example.el'.
;; Copyright (C) 2012 by Dmitry Bushenko. Distributed under BSD license.

(load-file "emacs-template.el")

(defun process-fields (args)
  (let* ((m (emtempl/make-hash-parameters args)))
    (cons 'list
	  (mapcar
	   (lambda (p)
	     (list 'hash-map "type" (symbol-name (car p))
		   "name" (symbol-name (cadr p))))
	   m))))

(defun process-accessors (args hm)
  (puthash "accessors" (process-fields args) hm)
  hm)

(defun process-controls (args hm)
  (puthash "controls" (process-fields args) hm)
  hm)

(defun process-actions (args hm)
  (puthash "actions" (process-fields args) hm)
  hm)

(defun process-mvc (part hm)
  (cond
   ((eq (first part) 'view) (puthash "view" (symbol-name (cadr part)) hm))
   ((eq (first part) 'model) (puthash "model" (symbol-name (cadr part)) hm))
   ((eq (first part) 'controller) (puthash "controller" (symbol-name (cadr part)) hm)))
  hm)

(defun process-definitions (params hm)
  (let ((arg (first params)))
    (cond
     ((null arg) hm)
     ((eq (first arg) 'accessors) (process-definitions
				   (cdr params)
				   (process-accessors (cdr arg) hm)))
     ((eq (first arg) 'actions) (process-definitions
				 (cdr params)
				 (process-actions (cdr arg) hm)))
     ((eq (first arg) 'view) (process-definitions
			      (cdr params)
			      (process-mvc arg hm)))
     ((eq (first arg) 'controller) (process-definitions
				    (cdr params)
				    (process-mvc arg hm)))
     ((eq (first arg) 'model) (process-definitions
			       (cdr params)
			       (process-mvc arg hm)))
     ((eq (first arg) 'controls) (process-definitions
				  (cdr params)
				  (process-controls (cdr arg) hm))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq model-templ
      "package my.app;

import java.util.Observable;
import com.adsl.demo.views.MyDemoActivityView;

public class {{class}} extends Observable {
{{#accessors}}
    private {{type}} _{{name}};{{/accessors}}

    public void updateAll() {
	this.setChanged();
	this.notifyObservers();
    }

    public boolean loadData({{view}} view) {
	// IMPLEMENT: Load data here.
	return true;
    }

{{#accessors}}
    public {{type}} get{{name}}() {
        return _{{name}};
    }

    public void set{{name}}({{type}} {{name}}) {
        this.{{name}} = {{name}};
    }
{{/accessors}}

}")

(setq view-templ
      "
package my.app;

import android.os.Bundle;
import android.view.View;
import android.app.Activity;
import java.util.Observable;
import java.util.Observer;
import android.widget.*;

import com.adsl.demo.R;
import com.adsl.demo.controllers.MyDemoActivityController;
import com.adsl.demo.models.MyDemoActivityModel;

public class {{view}} extends Activity implements Observer {
    // Members
{{#accessors}}
    private {{type}} _{{name}};
{{-accessors}}

{{#controls}}
    private {{type}} _{{name}};
{{-controls}}

{{#actions}}
    private {{type}} _{{name}};
{{-actions}}
    

    private {{controller}} controller;

    private void initialize() {
	// Initialize
    {{#actions}}
	_{{name}} = ({{type}}) findViewById(R.id.{{name}});{{/actions}}

    	View.OnClickListener listener = new View.OnClickListener() {
		@Override
		public void onClick(View v) {
    {{#actions}}
		    if (v.equals(_{{name}})) {
			controller.{{name}}ActionPerformed({{view}}.this);
			return;
		    }
    {{/actions}}
		}
	    };

{{#actions}}
	_{{name}}.setOnClickListener(listener);
{{-actions}}
    }

    /** Called when the activity is first created. */
    @Override
	public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.mydemoactivity);

	{{model}} model = new {{model}}();
	model.addObserver(this);

	controller = new {{controller}}(model);

	initialize();
	controller.loadData(this);
    }

    public void update(Observable modelObj, Object arg) {
	initialize();
	{{model}} model = ({{model}})modelObj;
    }

    // Accessors
    {{#actions}}
    public Button {{name}}() {
	return _{{name}};
    }
    {{/actions}}
        
{{#accessors}}
    public {{type}} get{{name}}() {
        return _{{name}};
    }

    public void set{{name}}({{type}} {{name}}) {
        this.{{name}} = {{name}};
    }
{{/accessors}}

{{#controls}}
    public {{type}} get{{name}}() {
        return _{{name}};
    }
{{/controls}}

}
")


(setq controller-templ
      "package my.app;
import com.adsl.demo.models.MyDemoActivityModel;
import com.adsl.demo.views.MyDemoActivityView;

public class {{class}} {
    private {{model}} model;

{{#accessors}}
    private {{type}} _{{name}};
{{-accessors}}

    public void loadData({{view}} view) {
	if (model.loadData(view)) {
	    model.updateAll();
	}
    }
    
    public {{class}}({{model}} model) {
	this.model = model;
    }


{{#actions}}   
   public void {{name}}ActionPerformed({{view}} view) {
   }
{{/actions}}

{{#accessors}}
    public {{type}} get{{name}}() {
        return _{{name}};
    }

    public void set{{name}}({{type}} {{name}}) {
        this.{{name}} = {{name}};
    }
{{/accessors}}
}
")



(defun open-in-buffer (name text)
  (other-window 1)
  (find-file name)
  (switch-to-buffer name)
  (delete-region (point-min) (point-max))
  (insert text))

(defun generate-model (hm)
  (open-in-buffer (concat (gethash "class" hm) ".java")
		  (render-template model-templ hm)))

(defun generate-view (hm)
  (open-in-buffer (concat (gethash "class" hm) ".java")
		  (render-template view-templ hm)))

(defun generate-controller (hm)
  (open-in-buffer (concat (gethash "class" hm) ".java")
		  (render-template controller-templ hm)))


(defmacro model (name &rest args)
  (let* ((class-name (symbol-name name))
	 (hm (process-definitions args (hash-map))))
    `(generate-model
      (hash-map "class" ,class-name
		"accessors" ,(gethash "accessors" hm)
		"view" ,(gethash "view" hm)))))

(defmacro controller (name &rest args)
  (let* ((class-name (symbol-name name))
	 (hm (process-definitions args (hash-map))))
    `(generate-controller
      (hash-map "class" ,class-name
		"accessors" ,(gethash "accessors" hm)
		"actions" ,(gethash "actions" hm)
		"view" ,(gethash "view" hm)
		"model" ,(gethash "model" hm)))))

(defmacro view (name &rest args)
  (let* ((class-name (symbol-name name))
	 (hm (process-definitions args (hash-map))))
    `(generate-view
      (hash-map "class" ,class-name
		"actions" ,(gethash "actions" hm)
		"controls" ,(gethash "controls" hm)
		"accessors" ,(gethash "accessors" hm)
		"controller" ,(gethash "controller" hm)
		"model" ,(gethash "model" hm)))))

