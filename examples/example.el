;; Copyright (C) 2012 by Dmitry Bushenko. Ditributed under BSD license.

;; Place 'emacs-template.el' in the same directory as 'dsl.el' and 'example.el'
;; Evaluate each s-expression.

(load-file "dsl.el")

(model MyTestModel
       (view MyTestView)
       (accessors String Name
		  int Age
		  String Address))

(controller MyTestController
	    (actions Button okBtn
		     Button cancelBtn)
	    (view MyTestView)
	    (model MyTestView))

(view MyTestView
      (model MyTestModel)
      (controller MyTestController)
      (accessors String Name
		 int Age
		 String Address)
      (controls TextView NameText
		TextView AgeText
		TextView AddressText)
      (actions Button okBtn
	       Button cancelBtn))
