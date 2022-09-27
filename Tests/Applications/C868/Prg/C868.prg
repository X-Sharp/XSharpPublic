// 868. INSTANCE iVars are incorrectly treated as public vars
// https://github.com/X-Sharp/XSharpPublic/issues/1115
// Following code compiles and runs with no errors in X#. In VO, it throws an error at runtime "No exported variable"

FUNCTION Start() AS VOID
LOCAL o AS TestClass
o := TestClass{}
? o:iInstance
o:iInstance := 123
? o:iInstance

CLASS TestClass

    INSTANCE iInstance := 555 AS INT
END CLASS

CLASS SubClass INHERIT TestClass
    CONSTRUCTOR
        SUPER()
        iInstance := 444
END CLASS
