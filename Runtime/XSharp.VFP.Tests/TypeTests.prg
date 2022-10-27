//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XUnit

using System.Diagnostics

// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.VFP.Tests

	CLASS TypeTests
		STATIC CONSTRUCTOR
        RegisterFoxMemVarSupport()
        XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro

		[Fact, Trait("Category", "Types")];
		METHOD VarTypeTests() AS VOID
            DIMENSION a[10]
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
            XSharp.RuntimeState.CompilerOptionFox2 := TRUE
            a = 123 // fill the array

            ? VarType ( a ) , VarType ( a[10] )
            ?
            Assert.Equal("N", VarType ( a ))
            Assert.Equal("N", VarType ( a[10]))


            a = "text"

            ? VarType ( a ) , VarType ( a[10] )
            ?
            Assert.Equal("C", VarType ( a ))
            Assert.Equal("C", VarType ( a[10]))

            DIMENSION b[4,2]

            ? VarType ( b ) , VarType ( b[3,1] , TRUE ) , VarType ( b[3,1] , FALSE )
            ?

            && VFP  L U U
            && X#   A L X

            Assert.Equal("L", VarType ( b ))
            Assert.Equal("U", VarType ( b [3,1], TRUE))
            // FoxPro returns U for the next test. We return "L"
            //Assert.Equal("U", VarType ( b [3,1], FALSE ))
            Assert.Equal("L", VarType ( b [3,1], FALSE ))

            b[3,1] = "Text"

            ? VarType ( b ) , VarType ( b[3,1] )
            ?
            && VFP L C
            Assert.Equal("L", VarType ( b ))
            Assert.Equal("C", VarType ( b [3,1]))
            b := DBNull.Value
            Assert.Equal("X", VarType ( b ))
            PRIVATE c
            c := .NULL.
            Assert.Equal("X", VarType ( c ))


            DO CheckParams WITH "TestString" , 5 , 3



		[Fact, Trait("Category", "Types")];
		METHOD xTypeTests() AS VOID
            var state := XSharp.RuntimeState.Dialect
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
            DIMENSION arr[3]
            private c, n

            c = "text"
            n = 12

            ? Type ("arr")  && "A" - while VFP returns "L"
            Assert.Equal("L", Type ("arr"))

            arr[1] = "text"

            ? Type ("arr")  && "A" - while VFP returns "C"

            Assert.Equal("C", Type ("arr"))


            //It seems that if VFPs Type() detects a array it returns the type of the first element.
            //To get "A" with VFP the additional (optional) param of the VFP Type() must be used


            && ---- VFP code only --- !

            && it seems that any other value than 1 throws an runtime error

            ? Type ("arr", 1 )  && "A"

            Assert.Equal("A", Type ("arr",1))

            && test with none array vars

            Assert.Equal("U", Type ("c",1))
            Assert.Equal("U", Type ("n",1))
            Assert.Equal("U", Type ("X",1))


            XSharp.RuntimeState.Dialect := state
            RETURN




	END CLASS

            PROCEDURE CheckParams AS VOID
                && LPARAMETERS a,b,c,d
                PARAMETERS a,b,c,d

                ? "VarType()" , VarType(d) , VarType ( d , TRUE )
                Assert.Equal("L", VarType ( d ))
                Assert.Equal("U", VarType ( d , TRUE))
                RETURN


END NAMESPACE
