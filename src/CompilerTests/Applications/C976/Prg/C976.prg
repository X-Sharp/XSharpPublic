// 976. Problems with the /fox3 option - thisform implementation
// https://github.com/X-Sharp/XSharpPublic/issues/2025

#pragma options("fox3", enable)
#pragma options("allowdot", enable)
#pragma options("lb", enable)
#pragma options("memvar", enable)
#pragma options("undeclared", enable)
#pragma options("allowoldstyleassignments", enable)
CLASS Form INHERIT XSharp.VFP.Custom // problem happens even if not inheriting from any class
	EXPORT Button AS Button
	PROPERTY Caption AS STRING AUTO
	CONSTRUCTOR( )
		SUPER( )
		SELF:Button := Button{ SELF }
		SELF:Caption := "Main Form"
END CLASS

CLASS Button INHERIT XSharp.VFP.Custom // problem happens even if not inheriting from any class
	PROPERTY ThisForm AS USUAL AUTO // or AS Form, AS USUAL, etc
	PROPERTY Caption AS STRING AUTO
	
	CONSTRUCTOR( oForm AS Form )
		SUPER( )
		SELF:ThisForm := oForm
		SELF:Caption := "Click me"
		
	// workaround because the compiler needs this method to exist
/*	METHOD FindForm() AS Form // or AS USUAL, AS OBJECT etc
	RETURN SELF:ThisForm*/
		
	METHOD Click() AS VOID
		// that's the original VFP code
		thisform.Button.Caption = "test"
		? thisform.Button.Caption
		xAssert( thisform.Button.Caption == "test" )

		xAssert( thisform.Caption == "Main Form" )
		thisform.Caption = "another test"
		? thisform.Caption
		xAssert( thisform.Caption == "another test" )
END CLASS

CLASS TextBox 
	PROPERTY ThisForm AS Form AUTO
	CONSTRUCTOR( oForm AS Form )
		SUPER( )
		SELF:ThisForm := oForm
		
		thisform.caption = "caption"
		xAssert( thisform.caption == "caption" )
END CLASS

CLASS ComboBox
	PROTECT ThisForm AS Form
	CONSTRUCTOR( oForm AS Form )
		SUPER( )
		SELF:ThisForm := oForm
		
		thisform.caption = "another caption"
		xAssert( thisform.caption == "another caption" )
END CLASS

CLASS ParentClass // INHERIT XSharp.VFP.Custom 
	PROPERTY ThisForm AS OBJECT AUTO
	CONSTRUCTOR( oForm AS Form )
		SUPER( )
		SELF:ThisForm := oForm
END CLASS
CLASS ListBox inherit ParentClass
	CONSTRUCTOR( oForm AS Form )
		SUPER( oForm )
		thisform.caption = "ccc"
		xAssert( thisform.caption == "ccc" )
END CLASS

FUNCTION Start() AS VOID
	LOCAL oForm AS Form
	oForm := Form{}
	oForm:Button:Click() // emulate user clicking the button

	Textbox{oForm}
	ComboBox{oForm}
	ListBox{oForm}


PROC xAssert(l AS LOGIC) AS VOID
IF .not. l
//	? "FAILED!"
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
	? "Assertion passed"
RETURN
