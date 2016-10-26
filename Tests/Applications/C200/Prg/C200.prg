//200. error XS1061: 'Dictionary<int, int>.KeyCollection' does not contain a definition FOR 'ToArray' and no extension METHOD 'ToArray' accepting a first argument of type 'Dictionary<int, int>.KeyCollection' could be Found (are you missing a USING directive or an assembly reference?)
// make sure there's a reference to System.Core, ToArray() is an extenstio mehod declared in it

//USING System.Linq // it works with that, but why is it required?
USING System.Collections.Generic

FUNCTION Start() AS VOID
LOCAL a AS Dictionary<INT, INT>
a := Dictionary<INT, INT>{}
a:Add(1,100)
a:Add(2,200)
? a:Keys:ToArray():Length

