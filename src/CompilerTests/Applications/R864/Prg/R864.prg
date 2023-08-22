#pragma warnings(9230, off)
//public FoxPro
public Fox
public FoxPro
public Clipper
FUNCTION Start( ) AS VOID
    // note that you can assign a number even when the public has the as string clause
    public MyPublic as string
    MyPublic := 10
    ? __DIALECT__
    ? MyPublic
    ? FoxPro
    ? Fox
    ? CLipper
    xAssert(MyPublic == 10)
    xAssert(FoxPro == true)
    xAssert(Fox == true)
    xAssert(Clipper == false)
RETURN


PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
