FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	? o:p1
	? o:p2
	
	DoTest(o)
	? o:p1
RETURN

PROCEDURE DoTest(o AS TestClass)
	o:p1 := String2Psz("newpSZ") // should be  a warning here

CLASS TestClass
	EXPORT p1 AS PSZ
	EXPORT p2 AS PSZ
    //EXPORT p2 := String2Psz("psz1") AS PSZ

	CONSTRUCTOR()
		p1 := String2Psz("psz2") // should be  a warning here
		? SELF:p1
		? SELF:p2
		? String2Psz("abc")       
	RETURN
END CLASS


CLASS TestClass2
	EXPORT p1 AS PSZ
	EXPORT p2 AS PSZ

	CONSTRUCTOR()
		p1 := StringAlloc("psz1")
		p2 := StringAlloc("psz2")

    DESTRUCTOR
        MemFree(p1)
        MemFree(p2)

	RETURN
END CLASS
