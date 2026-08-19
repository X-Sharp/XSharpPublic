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
	PROPERTY @@ThisForm AS OBJECT AUTO // or AS Form, AS USUAL, etc
	PROPERTY Caption AS STRING AUTO
	
	CONSTRUCTOR( oForm AS Form )
		SUPER( )
		SELF:ThisForm := oForm
		SELF:Caption := "Click me"
		
	// workaround because the compiler needs this method to exist
	METHOD FindForm() AS Form // or AS USUAL, AS OBJECT etc
	RETURN SELF:ThisForm
		
	METHOD Click() AS VOID
		// that's the original VFP code
		thisform.Button.Caption = "test"
		? thisform.Button.Caption

		thisform.Caption := "another test"
		? thisform.Caption
END CLASS

FUNCTION Start() AS VOID
	LOCAL oForm AS Form
	oForm := Form{}
	oForm:Button:Click() // emulate user clicking the button
