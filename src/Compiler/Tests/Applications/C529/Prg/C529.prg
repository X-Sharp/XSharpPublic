// 529. error XS0118: 'Font' is a type but is used like a variable
// problem happens in both Core, Vulcan etc
#using System.Drawing

CLASS _Font
END CLASS

CLASS Something
END CLASS
CLASS Another
END CLASS

CLASS _Form
	PROPERTY _Font AS OBJECT GET NULL SET
	PROPERTY Something AS INT AUTO
	EXPORT Another AS INT
	METHOD Test() AS VOID
		_Font := NULL // error with property
		Something := 333 // error with property
		? Something
		Another := 555 // ok, no problem when it is a field
		? another
	RETURN
END CLASS

FUNCTION Start() AS VOID
	_Form{}:Test()
RETURN


// original problem:

CLASS MyForm INHERIT System.Windows.Forms.Form
	CONSTRUCTOR()
		Font := System.Drawing.Font{"Arial", 5} // error
		Font := NULL // error
		SELF:Font := NULL // ok
	RETURN
	METHOD DoTest(oFont AS Font) AS VOID
		Font := Font{"Arial", 5} // error
	RETURN
	
END CLASS

