// 308. error XS1656: Cannot assign to 'Left' because it is a 'method group'
GLOBAL Left AS INT

FUNCTION Start() AS VOID
// ok here
Left := 1
? Left

// vulcan does not allow this: (no problem that we do of course!)
? Left("asd" , 2)

TestClass{}:Test()

CLASS TestClass
METHOD Test() AS VOID
// error XS1656: Cannot assign to 'Left' because it is a 'method group'
Left := 1
// error XS1503: Argument 1: cannot convert from 'method group' to '__Usual'
? Left

// does not work in vulcan, either
// ? Left("xyz" , 2)
END CLASS

