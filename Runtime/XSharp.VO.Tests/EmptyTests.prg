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
			LOCAL u as USUAL
			u := ""
			Assert.Equal(TRUE, Empty(u))
			u := space(10)
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

 		[Trait("Category", "Object")];
		[Fact];
		METHOD EmptyObjectTest() AS VOID
            LOCAL o as OBJECT
			o := ""
			Assert.Equal(TRUE, Empty(o))
			o := space(10)
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
        var dt  := DateTime.MinValue
        Assert.Equal(TRUE, Empty(dt))
        dt  := DateTime.MaxValue
        Assert.Equal(FALSE, Empty(dt))
        dt  := DateTime.Now
        Assert.Equal(FALSE, Empty(dt))
        LOCAL aStr := {} AS ARRAY OF STRING
        Assert.Equal(TRUE, Empty(aStr))
        AAdd(aStr, "")
        Assert.Equal(FALSE, Empty(aStr))
        LOCAL d as Date
        d := ToDay()
        Assert.Equal(FALSE, Empty(d))
        d := NULL_DATE
        Assert.Equal(TRUE, Empty(d))

        LOCAL l as Long
        l := 0
        Assert.Equal(TRUE, Empty(l))
        l := 42
        Assert.Equal(FALSE, Empty(l))

        LOCAL dw as DWORD
        dw := 0u
        Assert.Equal(TRUE, Empty(dw))
        dw := 42U
        Assert.Equal(FALSE, Empty(dw))
        LOCAL fl as FLOAT
        fl := 0.0
        Assert.Equal(TRUE, Empty(fl))
        fl := 42.42
        Assert.Equal(FALSE, Empty(fl))

        LOCAL cu as CURRENCY
        cu := $0.0
        Assert.Equal(TRUE, Empty(cu))
        cu := $42.42
        Assert.Equal(FALSE, Empty(cu))

        LOCAL log as LOGIC
        log := FALSE
        Assert.Equal(TRUE, Empty(log))
        log := TRUE
        Assert.Equal(FALSE, Empty(log))

        LOCAL sym as SYMBOL
        sym := NULL_SYMBOL
        Assert.Equal(TRUE, Empty(sym))
        sym := #MisterData
        Assert.Equal(FALSE, Empty(sym))

        LOCAL p1 as PTR
        p1 := NULL_PTR
        Assert.Equal(TRUE, Empty(p1))
        p1 := @p1
        Assert.Equal(FALSE, Empty(p1))

        LOCAL p2 as PSZ
        p2 := NULL_PSZ
        Assert.Equal(TRUE, Empty(p2))
        p2 := String2Psz("")
        Assert.Equal(TRUE, Empty(p2))
        p2 := String2Psz("abc")
        Assert.Equal(FALSE, Empty(p2))

        LOCAL arrx := NULL as STRING[]
        Assert.Equal(TRUE, empty(arrx))
        arrx := <STRING>{}
        Assert.Equal(TRUE, empty(arrx))
        arrx := <STRING>{"aaa"}
        Assert.Equal(FALSE, empty(arrx))

        LOCAL listx := NULL as List<String>
        Assert.Equal(TRUE, empty(listx))
        listx := List<String>{}
        Assert.Equal(TRUE, empty(listx))
        listx:Add("12345")
        Assert.Equal(FALSE, empty(listx))
    END CLASS
END NAMESPACE // XSharp.Runtime.Tests
