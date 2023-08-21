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

#pragma warnings(165, off)  // unassigned variables


BEGIN NAMESPACE XSharp.RT.Tests

	CLASS EmptyTests

 		[Trait("Category", "Usual")];
		[Fact];
		METHOD EmptyUsualTest() AS VOID
			LOCAL u AS USUAL
			u := ""
			Assert.Equal(TRUE, Empty(u))
			u := Space(10)
			Assert.Equal(TRUE, Empty(u))
			u := e"\r\n"
			Assert.Equal(TRUE, Empty(u))
			u := "sometext"
			Assert.Equal(FALSE, Empty(u))
			u := {1,2,3}
			Assert.Equal(FALSE, Empty(u))
			u := {}
			Assert.Equal(TRUE, Empty(u))
			u := NULL_ARRAY
			Assert.Equal(TRUE, Empty(u))
			u := 0L
			Assert.Equal(TRUE, Empty(u))
			u := 1L
			Assert.Equal(FALSE, Empty(u))
			u := 0U
			Assert.Equal(TRUE, Empty(u))
			u := 1U
			Assert.Equal(FALSE, Empty(u))
			u := 0.0m
			Assert.Equal(TRUE, Empty(u))
			u := 1.1m
			Assert.Equal(FALSE, Empty(u))
			u := 0.0
			Assert.Equal(TRUE, Empty(u))
			u := 1.1
			Assert.Equal(FALSE, Empty(u))
			u := NULL_DATE
			Assert.Equal(TRUE, Empty(u))
			u := Today()
			Assert.Equal(FALSE, Empty(u))
			u := DateTime.MinValue
			Assert.Equal(TRUE, Empty(u))
			u := (DateTime) Today()
			Assert.Equal(FALSE, Empty(u))
			u := #SomeSymbol
			Assert.Equal(FALSE, Empty(u))
			u := NULL_SYMBOL
         Assert.Equal(TRUE, Empty(u))
         // In the FoxPro dialect .NULL. = DbNull.Value is not seen as empty
         LOCAL eDialect := RuntimeState.Dialect AS XSharpDialect
         RuntimeState.Dialect := XSharpDialect.VO
         u := DBNull.Value
			Assert.Equal(TRUE, Empty(u))
         RuntimeState.Dialect := XSharpDialect.FoxPro
         u := DBNull.Value
			Assert.Equal(FALSE, Empty(u))
         RuntimeState.Dialect := eDialect

 		[Trait("Category", "Object")];
		[Fact];
		METHOD EmptyObjectTest() AS VOID
            LOCAL o AS OBJECT
			o := ""
			Assert.Equal(TRUE, Empty(o))
			o := Space(10)
			Assert.Equal(TRUE, Empty(o))
			o := e"\r\n"
			Assert.Equal(TRUE, Empty(o))
			o := "sometext"
			Assert.Equal(FALSE, Empty(o))
			o := {1,2,3}
			Assert.Equal(FALSE, Empty(o))
			o := {}
			Assert.Equal(TRUE, Empty(o))
			o := NULL_ARRAY
			Assert.Equal(TRUE, Empty(o))
			o := 0L
			Assert.Equal(TRUE, Empty(o))
			o := 1L
			Assert.Equal(FALSE, Empty(o))
			o := 0U
			Assert.Equal(TRUE, Empty(o))
			o := 1U
			Assert.Equal(FALSE, Empty(o))
			o := 0.0m
			Assert.Equal(TRUE, Empty(o))
			o := 1.1m
			Assert.Equal(FALSE, Empty(o))
			o := 0.0
			Assert.Equal(TRUE, Empty(o))
			o := 1.1
			Assert.Equal(FALSE, Empty(o))
			o := NULL_DATE
			Assert.Equal(TRUE, Empty(o))
			o := Today()
			Assert.Equal(FALSE, Empty(o))
			o := DateTime.MinValue
			Assert.Equal(TRUE, Empty(o))
			o := (DateTime) Today()
			Assert.Equal(FALSE, Empty(o))
			o := #SomeSymbol
			Assert.Equal(FALSE, Empty(o))
			o := NULL_SYMBOL
			Assert.Equal(TRUE, Empty(o))

       [Trait("Category", "Object")];
		[Fact];
		METHOD EmptyOtherTests() AS VOID
        VAR dt  := DateTime.MinValue
        Assert.Equal(TRUE, Empty(dt))
        dt  := DateTime.MaxValue
        Assert.Equal(FALSE, Empty(dt))
        dt  := DateTime.Now
        Assert.Equal(FALSE, Empty(dt))
        LOCAL aStr := {} AS ARRAY OF STRING
        Assert.Equal(TRUE, Empty(aStr))
        AAdd(aStr, "")
        Assert.Equal(FALSE, Empty(aStr))
        LOCAL d AS DATE
        d := Today()
        Assert.Equal(FALSE, Empty(d))
        d := NULL_DATE
        Assert.Equal(TRUE, Empty(d))

        LOCAL l AS LONG
        l := 0
        Assert.Equal(TRUE, Empty(l))
        l := 42
        Assert.Equal(FALSE, Empty(l))

        LOCAL dw AS DWORD
        dw := 0u
        Assert.Equal(TRUE, Empty(dw))
        dw := 42U
        Assert.Equal(FALSE, Empty(dw))
        LOCAL fl AS FLOAT
        fl := 0.0
        Assert.Equal(TRUE, Empty(fl))
        fl := 42.42
        Assert.Equal(FALSE, Empty(fl))

        LOCAL cu AS CURRENCY
        cu := $0.0
        Assert.Equal(TRUE, Empty(cu))
        cu := $42.42
        Assert.Equal(FALSE, Empty(cu))

        LOCAL log AS LOGIC
        log := FALSE
        Assert.Equal(TRUE, Empty(log))
        log := TRUE
        Assert.Equal(FALSE, Empty(log))

        LOCAL sym AS SYMBOL
        sym := NULL_SYMBOL
        Assert.Equal(TRUE, Empty(sym))
        sym := #MisterData
        Assert.Equal(FALSE, Empty(sym))

        LOCAL p1 AS PTR
        p1 := NULL_PTR
        Assert.Equal(TRUE, Empty(p1))
        p1 := @p1
        Assert.Equal(FALSE, Empty(p1))

        LOCAL p2 AS PSZ
        p2 := NULL_PSZ
        Assert.Equal(TRUE, Empty(p2))
        p2 := String2Psz("")
        Assert.Equal(TRUE, Empty(p2))
        p2 := String2Psz("abc")
        Assert.Equal(FALSE, Empty(p2))

        LOCAL arrx := NULL AS STRING[]
        Assert.Equal(TRUE, Empty(arrx))
        arrx := <STRING>{}
        Assert.Equal(TRUE, Empty(arrx))
        arrx := <STRING>{"aaa"}
        Assert.Equal(FALSE, Empty(arrx))

        LOCAL listx := NULL AS List<STRING>
        Assert.Equal(TRUE, Empty(listx))
        listx := List<STRING>{}
        Assert.Equal(TRUE, Empty(listx))
        listx:Add("12345")
        Assert.Equal(FALSE, Empty(listx))
    END CLASS
END NAMESPACE // XSharp.Runtime.Tests
