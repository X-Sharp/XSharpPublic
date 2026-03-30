//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING XUnit
USING XSharp.XPP

BEGIN NAMESPACE XSharp.XPP.Tests

    CLASS XppDataObjectTests

        // ---- IsMemberVar tests ----

        [Fact, Trait("Category", "DataObject")];
        METHOD IsMemberVarDataObjectTests() AS VOID
            LOCAL oData AS DataObject
            oData := DataObject{}
            // Before setting any property, field does not exist
            Assert.False(IsMemberVar(oData, "Name", 0))
            // Set a dynamic field via NoIvarPut (late binding via ivar put)
            oData:Name := "Test"
            Assert.True(IsMemberVar(oData, "Name", 0))
            Assert.True(IsMemberVar(oData, "name", 0))   // case insensitive
            Assert.True(IsMemberVar(oData, "NAME", 0))
            Assert.False(IsMemberVar(oData, "Unknown", 0))
        RETURN

        [Fact, Trait("Category", "DataObject")];
        METHOD IsMemberVarMultipleFieldsTests() AS VOID
            LOCAL oData AS DataObject
            oData := DataObject{}
            oData:FirstName := "John"
            oData:LastName  := "Doe"
            oData:Age       := 30
            Assert.True(IsMemberVar(oData, "FirstName", 0))
            Assert.True(IsMemberVar(oData, "LastName", 0))
            Assert.True(IsMemberVar(oData, "Age", 0))
            Assert.False(IsMemberVar(oData, "Email", 0))
        RETURN

        // ---- DataObject Copy tests ----

        [Fact, Trait("Category", "DataObject")];
        METHOD DataObjectCopyTests() AS VOID
            LOCAL oData AS DataObject
            LOCAL oCopy AS DataObject
            oData := DataObject{}
            oData:Name  := "Alice"
            oData:Score := 100
            oCopy := oData:Copy()
            Assert.NotEqual(NULL_OBJECT, oCopy)
            Assert.Equal("Alice", (STRING) oCopy:Name)
            Assert.Equal(100, (INT) oCopy:Score)
            // Verify the copy is independent
            oCopy:Name := "Bob"
            Assert.Equal("Alice", (STRING) oData:Name)
            Assert.Equal("Bob",   (STRING) oCopy:Name)
        RETURN

        // ---- DataObject Merge tests ----

        [Fact, Trait("Category", "DataObject")];
        METHOD DataObjectMergeTests() AS VOID
            LOCAL oBase AS DataObject
            LOCAL oExtra AS DataObject
            oBase  := DataObject{}
            oBase:Name := "Alice"
            oExtra := DataObject{}
            oExtra:Score := 99
            oExtra:Level := 5
            oBase:Merge(oExtra, "")
            // After merge, oBase should have all fields from oExtra (that weren't already there)
            Assert.True(IsMemberVar(oBase, "Score", 0))
            Assert.True(IsMemberVar(oBase, "Level", 0))
            Assert.Equal(99,  (INT) oBase:Score)
            Assert.Equal(5,   (INT) oBase:Level)
            // Original field is preserved
            Assert.Equal("Alice", (STRING) oBase:Name)
        RETURN

        [Fact, Trait("Category", "DataObject")];
        METHOD DataObjectMergeWithPrefixTests() AS VOID
            LOCAL oBase AS DataObject
            LOCAL oExtra AS DataObject
            oBase  := DataObject{}
            oBase:Name := "Alice"
            oExtra := DataObject{}
            oExtra:City := "Paris"
            // Merge with prefix "Extra_"
            oBase:Merge(oExtra, "Extra_")
            Assert.True(IsMemberVar(oBase, "Extra_City", 0))
            Assert.False(IsMemberVar(oBase, "City", 0))
        RETURN

        [Fact, Trait("Category", "DataObject")];
        METHOD DataObjectMergeDoesNotOverwriteTests() AS VOID
            LOCAL oBase AS DataObject
            LOCAL oExtra AS DataObject
            oBase  := DataObject{}
            oBase:Name := "Alice"
            oExtra := DataObject{}
            oExtra:Name := "Bob"
            // Merge should NOT overwrite existing key
            oBase:Merge(oExtra, "")
            Assert.Equal("Alice", (STRING) oBase:Name)
        RETURN

        // ---- DataObject DefineMethod tests ----

        [Fact, Trait("Category", "DataObject")];
        METHOD DataObjectDefineMethodTests() AS VOID
            LOCAL oData AS DataObject
            oData := DataObject{}
            oData:Value := 10
            // Define a method that doubles the Value
            oData:DefineMethod("DoubleValue", {|oSelf| (INT) oSelf:Value * 2})
            LOCAL nResult AS INT
            nResult := (INT) oData:DoubleValue()
            Assert.Equal(20, nResult)
        RETURN

        [Fact, Trait("Category", "DataObject")];
        METHOD DataObjectDefineMultipleMethodsTests() AS VOID
            LOCAL oData AS DataObject
            oData := DataObject{}
            oData:X := 3
            oData:Y := 4
            oData:DefineMethod("SumXY",  {|oSelf| (INT) oSelf:X + (INT) oSelf:Y})
            oData:DefineMethod("ProdXY", {|oSelf| (INT) oSelf:X * (INT) oSelf:Y})
            Assert.Equal(7,  (INT) oData:SumXY())
            Assert.Equal(12, (INT) oData:ProdXY())
        RETURN

        // ---- DataObject GetPropertyNames tests ----

        [Fact, Trait("Category", "DataObject")];
        METHOD DataObjectGetPropertyNamesTests() AS VOID
            LOCAL oData AS DataObject
            LOCAL aNames AS STRING[]
            oData := DataObject{}
            oData:Alpha := "a"
            oData:Beta  := "b"
            oData:Gamma := "c"
            aNames := oData:GetPropertyNames()
            Assert.Equal(3, aNames:Length)
        RETURN

    END CLASS

END NAMESPACE
