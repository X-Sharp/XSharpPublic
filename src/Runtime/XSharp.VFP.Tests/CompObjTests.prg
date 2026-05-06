//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests
    CLASS SimpleObj
        PROPERTY Name AS STRING AUTO
        PROPERTY Age AS INT AUTO
        EXPORT Salary AS DECIMAL
    END CLASS

    CLASS NestedObj
        PROPERTY Title AS STRING AUTO
        PROPERTY Child AS SimpleObj AUTO
    END CLASS

    CLASS ExtraPropObj
        PROPERTY Name AS STRING AUTO
        PROPERTY Age AS INT AUTO
        PROPERTY Extra AS STRING AUTO
        EXPORT Salary AS DECIMAL
    END CLASS

    CLASS WithIndexerObj
        PROPERTY Name AS STRING AUTO
        PROPERTY SELF[nIndex AS INT] AS INT
            GET
                RETURN nIndex
            END GET
        END PROPERTY
    END CLASS

    CLASS CompObjTests
        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        [Fact, Trait("Category", "ClassAndObject")];
        METHOD SameObjReferenceTest AS VOID
            VAR o := SimpleObj{}
            o:Name := "Test"
            o:Age := 42
            o:Salary := 1000.00m
            Assert.True(COMPOBJ(o, o))
        END METHOD

        [Fact, Trait("Category", "ClassAndObject")];
        METHOD IdenticalObjectsTest AS VOID
            VAR o1 := SimpleObj{}
            o1:Name := "Test"
            o1:Age := 42
            o1:Salary := 1000.00m

            VAR o2 := SimpleObj{}
            o2:Name := "Test"
            o2:Age := 42
            o2:Salary := 1000.00m

            Assert.True(COMPOBJ(o1, o2))
        END METHOD

        [Fact, Trait("Category", "ClassAndObject")];
        METHOD DifferentValuesTest AS VOID
            LOCAL o1, o2 AS SimpleObj
            o1 := SimpleObj{}
            o1:Name := "Test"
            o1:Age := 42
            o1:Salary := 1000.00m

            o2 := SimpleObj{}
            o2:Name := "Test"
            o2:Age := 99
            o2:Salary := 1000.00m
            Assert.False(COMPOBJ(o1, o2))
        END METHOD

        [Fact, Trait("Category", "ClassAndObject")];
        METHOD NestedObjectsEqualTest AS VOID
            LOCAL o1, o2 AS NestedObj

            o1 := NestedObj{}
            o1:Title := "A"
            o1:Child := SimpleObj{}
            o1:Child:Name := "X"
            o1:Child:Age := 1
            o1:Child:Salary := 1000.00m
            o2 := NestedObj{}
            o2:Title := "A"
            o2:Child := SimpleObj{}
            o2:Child:Name := "X"
            o2:Child:Age := 1
            o2:Child:Salary := 1000.00m

            Assert.True(COMPOBJ(o1, o2))
        END METHOD

        [Fact, Trait("Category", "ClassAndObject")];
        METHOD BothNullTest AS VOID
            Assert.True(COMPOBJ(NULL_OBJECT, NULL_OBJECT))
        END METHOD

        [Fact, Trait("Category", "ClassAndObject")];
        METHOD OneNullTest AS VOID
            VAR o := SimpleObj{}

            Assert.False(COMPOBJ(o, NULL_OBJECT))
            Assert.False(COMPOBJ(NULL_OBJECT, o))
        END METHOD

        [Fact, Trait("Category", "ClassAndObject")];
        METHOD ExtraPropertyReturnsFalseTest AS VOID
            // oExpression2 has an extra "Extra" property not present on oExpression1
            VAR o1 := SimpleObj{}
            o1:Name := "Test"
            o1:Age := 42
            o1:Salary := 1000.00m
            VAR o2 := ExtraPropObj{}
            o2:Name := "Test"
            o2:Age := 42
            o2:Salary := 1000.00m
            o2:Extra := "Extra"


            Assert.False(COMPOBJ(o1, o2))
            Assert.False(COMPOBJ(o2, o1))
        END METHOD

        [Fact, Trait("Category", "ClassAndObject")];
        METHOD IndexedPropertyIgnoredTest AS VOID
            // Indexed properties must be ignored; only the regular Name property is compared
            VAR o1 := WithIndexerObj{}
            o1:Name := "Test"

            VAR o2 := WithIndexerObj{}
            o2:Name := "Test"

            Assert.True(COMPOBJ(o1, o2))
        END METHOD

    END CLASS
END NAMESPACE // XSharp.VFP.Tests
