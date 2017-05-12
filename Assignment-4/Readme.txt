=========Environment=========
1. Download Dr. Scheme 4.2.5. 
http://download.plt-scheme.org/
2. Choose R5RS language.

=========Files      =========
1. misc.scm        : Basic utility definitions. (You should not modify this file.)
2. object.scm      : Class definition of object. In java, Object class is the base class. (You should not modify this file.)
3. point.scm       : Class definition of point.
4. color_point.scm : Class definition of color-point.

=========Syntax     =========
While defining a new class, you should define two function. 
For example, while defining a Point Class:
a. Class definition  : (define Point ... )
	Writing Class-method here:
	1. 'super : return its parent's "class definition" function. ;Object
	2. 'name  : return a string of its name
	3. 'dictionary : return its "class dictionary" function ;Point-class
	4. 'new   : constructor of this object
		You can also define some inline instance definition here, but they can't be reused by its children.
		
b. Class Dictionary : (define Point-Dictionary ... ) 
	Only methods written here can be reused by its children.
	Example:
	It's a list of message and it's implementation.
	
(define Point-Dictionary
	(list
	    (list 'toString             ; Message name, you don't have to define how many arguments it needs.
	      (lambda (this args)       ; Dont modify this line, 
	        (string-append                               ;From here
	          (this 'super Point 'toString args)         ;
	          ;my(message Object 'toString this args)    ;     Writing the implementation of this function here.
	          " at "                                     ;
	          (number->string (this 'getx))              ;
	          ","                                        ;
	          (number->string (this 'gety)))             ;To here
	      )
	    )
	)
)


=========Usage      =========
(define id (Object 'new)) ; id = new Object()
(id 'draw)                ; id.draw() 

(define p1 (Point 'new 1 3)) ; p1 = new Point(1,3);
(p1 'draw)                   ; p1.draw();
(p1 'move 2 3)               ; p1.move(2,3);
(p1 'setx! 3)                ; p1.setx!(3);
=========Error Handling======
(point 'haha) ;no such class-method
-> Point#Message not understood: haha

(p1 'haha)  ;no such instance-method
-> Root#Message not understood: haha   
;since it will trace back to object, all non-defined instance-method will return "Root"#Message not understood
=============================
To build your own class, just open a new file and define two functions. (Class definition and Class dictionary)
Then add (load "filename.scm") in main.
(load "misc.scm")
(load "object.scm") are required, since these two files build all O-O system.


Notice that in our case, message 'data of Point and Color_point class should be defined inline.
If there's some data in your own class, you should also using inline defition to access and change it.
