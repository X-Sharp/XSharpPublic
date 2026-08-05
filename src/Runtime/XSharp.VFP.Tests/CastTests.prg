//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests

    CLASS CastTests
        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro

        // Full type names must route to the same handler as the single-letter codes
        [Fact, Trait("Category", "Cast")];
        METHOD Cast_FullTypeNames() AS VOID
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
            Assert.Equal("N", VarType(CAST("123" AS Integer)))
            Assert.Equal("N", VarType(CAST("123" AS Int)))
            Assert.Equal("N", VarType(CAST("123" AS Num)))       // Num alias
            Assert.Equal("N", VarType(CAST("123" AS Numeric)))
            Assert.Equal("C", VarType(CAST(65 AS Character)))
            Assert.Equal("C", VarType(CAST(65 AS Char)))
            Assert.Equal("Y", VarType(CAST(1 AS Currency)))
            Assert.Equal("L", VarType(CAST(1 AS Logical)))

        // Type name is case-insensitive
        [Fact, Trait("Category", "Cast")];
        METHOD Cast_CaseInsensitive() AS VOID
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
            Assert.Equal("N", VarType(CAST("123" AS n)))
            Assert.Equal("N", VarType(CAST("123" AS integer)))

        // numeric -> character: default width is 1 (VFP), explicit width honored
        [Fact, Trait("Category", "Cast")];
        METHOD Cast_CharacterWidth() AS VOID
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
            Assert.Equal("6", (STRING) CAST(65 AS C))            // "65" truncated to default width 1
            Assert.True(Len(CAST(65 AS C)) == 1)
            Assert.True(Len(CAST(65 AS C(20))) == 20)
            Assert.Equal("65", AllTrim(CAST(65 AS C(20))))

        // width + precision
        [Fact, Trait("Category", "Cast")];
        METHOD Cast_NumericPrecision() AS VOID
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
            Assert.Equal("N", VarType(CAST(3.14159 AS Numeric(8,2))))

        // NULL / NOT NULL clause must be accepted (and ignored at expression level)
        [Fact, Trait("Category", "Cast")];
        METHOD Cast_NullClause() AS VOID
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
            Assert.Equal("N", VarType(CAST("123" AS Integer NULL)))
            Assert.Equal("N", VarType(CAST("123" AS Integer NOT NULL)))
            Assert.Equal("C", VarType(CAST(65 AS Char(10) NOT NULL)))

        // dynamic type: CAST(x AS (expr)) where expr yields the type name at runtime
        [Fact, Trait("Category", "Cast")];
        METHOD Cast_DynamicType() AS VOID
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
            LOCAL cTipo := "N" AS STRING
            Assert.Equal("N", VarType(CAST(65 AS (cTipo))))
            Assert.Equal("N", VarType(CAST(65 AS (cTipo) NOT NULL)))

    END CLASS

END NAMESPACE
