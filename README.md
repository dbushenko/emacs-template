# Emacs templating system inspired by Mustache 
(http://mustache.github.com/)

Supports the following types of tags:

* {{var}} -- substitute with the value of the var
* {{#sect}} -- beginning of section
* {{^sect}} -- beginning of inverted section
* {{/sect}} -- section ending
* {{>incl}} -- include template

You may use it just to substitute placeholders with variables.
    (render-template
            "{{action}} {{name}}!"
            (hash-map "action" "Hello"
                      "name" "World"))
    ----------
    Hello World!

If the section is nil, then its contents is not rendered. If the section is nil and is inverted, then its contents is rendered.

    ;; Not rendered
    (render-template 
            "{{#mysect}}not rendered{{/mysect}}" 
            (hash-map "mysect" nil))
    ---------

    ;; Rendered
    (render-template 
            "{{^mysect}}rendered{{/mysect}}" 
            (hash-map "mysect" nil))
    ---------
    rendered

If the section is hash-table, it's renderend, and its parameters are merged with the parent template parameters.

    (render-template
            "{#mysect}}{{action}} {{name}}!{{/mysect}}"
            (hash-map "mysect" (hash-map "action" "Hello"  
                                         "name" "World")))
    ----------
    Hello World!


If the section is list, then it's rendered like a list.

    (render-template
            "{{#mysect}}{{action}} {{name}}!{{/mysect}}"
            (hash-map "mysect" (list (hash-map "action" "Hello"
                                               "name" "World") 
                                     (hash-map "action" "Welcome"
                                               "name" "home"))))
    ----------
    Hello World!
    Welcome home

## Example usage:

    (load-file "emacs-template.el")
    
    (setq templ "
    public class {{class}} {
    // Fields
    {{#fields}}
    private {{type}} {{name}};{{/fields}}
    
    // Accessors
    {{#fields}}
    {{>method}}
    {{/fields}}
    
    // Methods
    {{^toString}}
    public String toString() {
      return {{#fields}}String.valueOf({{name}}){{^last}}+{{/last}}{{/fields}};
    }
    {{/toString}}
    
    {{#hidden}}
    This section will never show up!
    {{/hidden}}
    }")
    
    (setq method "public {{type}} get{{name}}() {
    return {{name}};
    }
    
    public void set{{name}}({{type}} val}} {
    {{name}} = val;
    }")
    
    (render-template templ
    		  (hash-map "fields" (list
    				      (hash-map "name" "Name"
    						"type" "String")
    				      (hash-map "name" "Age"
    						"type" "int"
    						"last" t))
    			    "class" "Person"
    			    "method" method
    			    "toString" nil
    			    "hidden" nil
    			    ))

