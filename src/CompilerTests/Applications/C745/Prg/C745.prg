// 745. error XS0121: The call is ambiguous between the following methods or properties: 'Child.Test(ref int, int, logic, string)' 
// and 'Parent.Test(ref int, int, string)'
// Previous builds than 2.6 compiled this without errors and called the "child" method.
// c# does the same thing (calls the child method)
// Also not that this method hierarchy is a very used one in MVVM, see method Set() of class ObservableObject and class ViewModelBase : ObservableObject
// https://github.com/X-Sharp/XSharpPublic/issues/503
PUBLIC CLASS Parent
METHOD Test(n REF INT, n2 AS INT, c := NULL AS STRING ) AS STRING
? "Parent"
RETURN "Parent"
END CLASS

CLASS Child INHERIT Parent
METHOD Test(n REF INT, n2 := 0 AS INT, l := FALSE AS LOGIC, c := NULL AS STRING) AS STRING
? "Child"
RETURN "Child"
END CLASS

FUNCTION Start() AS VOID
LOCAL n := 0 AS INT
LOCAL o := Child{} AS Child
LOCAL c AS STRING
c := o:Test(REF n,1)

xAssert(c == "Child")

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

