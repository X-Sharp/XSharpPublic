// 760. error XS0544: 'Child.Foo': cannot override because 'Parent.Foo()' is not a property
/*
C760.prg(13,2): error XS0544: 'Child.Foo': cannot override because 'Parent.Foo()' is not a property
C760.prg(15,2): error XS0544: 'Child.Bar': cannot override because 'Parent.Bar(params XSharp.__Usual[])' is not a property
C760.prg(19,4): error XS0544: 'ControlSubclass.IsEnabled': cannot override because 'VO.Control.IsEnabled(params XSharp.__Usual[])' is not a property
*/

// version with /vo3 disabled

#pragma warnings(108, off) //   methid hides parent methid
FUNCTION Start() AS VOID

RETURN

CLASS Parent
	METHOD Foo() AS VOID
	METHOD Bar() CLIPPER
	RETURN NIL
END CLASS

CLASS Child INHERIT Parent
	ACCESS Foo AS INT
	RETURN 0
	ASSIGN Bar(u)
END CLASS

CLASS ControlSubclass INHERIT PushButton
   ACCESS IsEnabled
   RETURN FALSE
END CLASS
