// 284. error XS1501: No overload for method 'Left' takes 2 arguments:
CLASS TestClass
    METHOD Left(n AS INT) AS LOGIC
    RETURN FALSE
    METHOD DoTest() AS VOID
        ? Left("asd",1)
    RETURN
END CLASS 

FUNCTION Start() AS VOID
TestClass{}:DoTest()
