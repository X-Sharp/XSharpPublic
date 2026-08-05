//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.IO
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests
	CLASS IntrospectionTests
	    STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        [Fact, Trait("Category", "Introspection")];
        METHOD AClassFromObjectReturnsHierarchy AS VOID
            VAR o := Custom{}
            LOCAL ARRAY aClasses(1)
            VAR nCount := AClass(aClasses, o)
            Assert.True(nCount > 0)
            Assert.Equal(nCount, ALen(aClasses, 1))
            Assert.Equal("CUSTOM", aClasses[1])
        END METHOD

        [Fact, Trait("Category", "Introspection")];
        METHOD AClassInvalidTypeReturnsZero AS VOID
            LOCAL ARRAY aClasses(1)
            VAR nCount := AClass(aClasses, "NonExistentClass")
            Assert.Equal(0, (INT)nCount)
        END METHOD

        [Fact, Trait("Category", "Introspection")];
        METHOD ASessionsReturnsSessions AS VOID
            LOCAL ARRAY laSessions(1)
            VAR nCount := ASessions(laSessions)
            Assert.True(nCount > 0)
            Assert.Equal(2, (INT)ALen(laSessions, 2))
            Assert.True((INT)laSessions[1, 1] > 0)
        END METHOD

        [Fact, Trait("Category", "Introspection")];
        METHOD AStackInfoReturnsStack AS VOID
            LOCAL ARRAY aStack(1)
            VAR nLevels := AStackInfo(aStack)
            Assert.True(nLevels > 0)
            Assert.Equal(6, (INT)ALen(aStack, 2))
            Assert.Equal(1, (INT)aStack[1, 1])
        END METHOD

	END CLASS
END NAMESPACE // XSharp.VFP.Tests
