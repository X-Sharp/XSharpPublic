// 879. Compiler incorrectly selects constructor overload with OBJECT[] parameter
// 
/*
System.ArgumentException: Non white space characters cannot be added to content.
   at System.Xml.Linq.XDocument.ValidateString(String s)
   at System.Xml.Linq.XContainer.AddStringSkipNotify(String s)
   at System.Xml.Linq.XContainer.AddContentSkipNotify(Object content)
*/

USING System.Xml.Linq

FUNCTION Start( ) AS VOID
	LOCAL x AS XDocument
	x := XDocument{}
	x := XDocument{x} // this works fine
	x := XDocument{XDeclaration{"1.0", "utf-8", "False"}} // incorrect overload picked here
	? x:Declaration:Version
RETURN
