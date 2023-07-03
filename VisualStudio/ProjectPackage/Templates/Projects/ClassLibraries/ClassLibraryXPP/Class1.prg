// $safeitemrootname$.prg
// Created by    : $username$
// Creation Date : $time$
// Created for   : $registeredorganization$
// WorkStation   : $machinename$
USING System
USING System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)USING System.Linq
$endif$USING System.Text


// X# allows to group class inside namespaces. Namespaces are optional.
// You can also delete the BEGIN NAMESPACE .. END NAMESPACE block

BEGIN NAMESPACE $safeprojectname$
    CLASS Class1
    EXPORTED:                   // Some Exported instance variables.
                                // You can also use HIDDEN: , PROTECTED: and INTERNAL:
                                // X# allows PUBLIC as alias for EXPORTED
                                //    and PRIVATE as alias for HIDDEN

    VAR var1, var2  AS STRING   // X# allows to declare the type of the instance variable

    CLASS VAR listOfObjects AS List<Class1> // You can declare a class var and also add a type, including generic types!

    CLASS VAR someObject AS OBJECT

    METHOD init                             // Forward declare the constructor
    CLASS METHOD initClass                  // Forward declare the class constructor

    METHOD SomeMethod                       // Forward declaration of the method

    INLINE METHOD AnotherMethod(cVal as STRING) AS STRING // Inline declaration of another method
        RETURN "CallMe "+cVal                             // You may declare the parameter type and return type
    ENDCLASS


    CLASS METHOD Class1:initClass()
        // This is the type constructor. it gets called only once
        ::listOfObjects := List<Class1>{}     // You can use the "new" syntax with curly braces to create an objec
        ::someObject    := Object():new()     // but also use Type():New() syntax.

    METHOD Class1:init()
        ::var1 := "First String"
        ::var2 := "Second String"

        RETURN
    METHOD Class1:SomeMethod(nValue)        // Implementation of the method declared inside the class
    RETURN 42 + nValue


END NAMESPACE // $safeprojectname$

