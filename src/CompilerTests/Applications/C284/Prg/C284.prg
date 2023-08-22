// 284. error XS1501: No overload for method 'Left' takes 2 arguments:
#pragma warnings(9066, disable)
CLASS TestClass
    METHOD Left(n AS INT) AS LOGIC
    RETURN FALSE
    METHOD InstanceMethod() AS LOGIC
    RETURN FALSE

    METHOD DoTest() AS VOID
        ? Left("asd",1)
        ? SELF:Left(123)
        ? InstanceMethod()
        ? SELF:InstanceMethod()
    RETURN
END CLASS

FUNCTION Start() AS VOID
TestClass{}:DoTest()
