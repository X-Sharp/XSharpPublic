//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic

USING System.Linq
USING System.IO
USING System.Text
USING XUnit
USING NewtonSoft.Json
#pragma warnings(168, off)  // unused variables
// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.RT.Tests

	CLASS SerializeTests

 		[Trait("Category", "Serialize")];
		[Fact];
        METHOD DateTestJson AS VOID
             LOCAL d := Today() AS DATE
             VAR o := SELF:JsonSaveRestore(d)
             Assert.True(o IS DATE)
             d := (DATE) o
             Assert.True (d == ToDay())
            RETURN

       METHOD JsonSaveRestore<T>(source AS T, type := NULL AS System.Type) AS OBJECT
             VAR s := JsonConvert.SerializeObject(source,Formatting.Indented)
             IF type == NULL
                 type := typeof(T)
             ENDIF
             VAR o := JsonConvert.DeserializeObject(s, type)
             RETURN o

		[Trait("Category", "Serialize")];
		[Fact];
        METHOD DateTestBinary AS VOID
             LOCAL d := Today() AS DATE
             LOCAL o AS OBJECT
             o := SELF:BinarySaveRestore(d)
             Assert.True(o IS DATE)
             d := (DATE) o
             Assert.True (d == ToDay())
            RETURN

        METHOD BinarySaveRestore<T>(source AS T) AS OBJECT
             VAR ms := MemoryStream{}
             VAR bw := System.Runtime.Serialization.Formatters.Binary.BinaryFormatter{}
             bw:Serialize(ms, source)
             ms:Position := 0
             VAR o := bw:Deserialize(ms)
             RETURN o


        [Trait("Category", "Serialize")];
		[Fact];
        METHOD FloatTestJson AS VOID
             LOCAL f := 123.456 AS FLOAT
             VAR o := SELF:JsonSaveRestore(f)
             Assert.True(o IS FLOAT)
             VAR f2 := (FLOAT) o
             Assert.True (f == f2)
            RETURN
        [Trait("Category", "Serialize")];
		[Fact];
        METHOD FloatTestBinary AS VOID
             LOCAL f := 123.456 AS FLOAT
             VAR o := SELF:BinarySaveRestore(f)
             Assert.True(o IS FLOAT)
             VAR f2 := (FLOAT) o
             Assert.True (f == f2)
            RETURN

        [Trait("Category", "Serialize")];
		[Fact];
        METHOD SymbolTestJson AS VOID
             LOCAL s1 := #MySymbol AS SYMBOL
             VAR o := SELF:JsonSaveRestore(s1)
             Assert.True(o IS SYMBOL)
             VAR s2 := (SYMBOL) o
             Assert.True (s1 == s2)
            RETURN
        [Trait("Category", "Serialize")];
		[Fact];
        METHOD SymbolTestBinary AS VOID
             LOCAL s1 := #MySymbol AS SYMBOL
             VAR o := SELF:BinarySaveRestore(s1)
             Assert.True(o IS SYMBOL)
             VAR s2 := (SYMBOL) o
             Assert.True (s1 == s2)
            RETURN

        [Trait("Category", "Serialize")];
        [Fact];
        METHOD CurrencyTestJson AS VOID
             LOCAL c1 := $123.456 AS CURRENCY
             VAR o := SELF:JsonSaveRestore(c1)
             Assert.True(o IS CURRENCY)
             VAR c2 := (CURRENCY) o
             Assert.True (c1 == c2)
            RETURN

        [Trait("Category", "Serialize")];
        [Fact];
        METHOD CurrencyTestBinary AS VOID
             LOCAL c1 := $123.456 AS CURRENCY
             VAR o := SELF:BinarySaveRestore(c1)
             Assert.True(o IS CURRENCY)
             VAR c2 := (CURRENCY) o
             Assert.True (c1 == c2)
            RETURN

        [Trait("Category", "Serialize")];
        [Fact];
        METHOD BinaryTestJson AS VOID
             LOCAL b1 := 0h4567890ABCDE AS BINARY
             VAR o := SELF:JsonSaveRestore(b1)
             Assert.True(o IS BINARY)
             VAR b2 := (BINARY ) o
             Assert.True (b1 == b2)
            RETURN

        [Trait("Category", "Serialize")];
        [Fact];
        METHOD BinaryTestBinary AS VOID
             LOCAL b1 := 0h4567890ABCDE AS BINARY
             VAR o := SELF:BinarySaveRestore(b1)
             Assert.True(o IS BINARY)
             VAR b2 := (BINARY ) o
             Assert.True (b1 == b2)
            RETURN

        // PSZ cannot be serialized to json since it is a pointer !
//        [Trait("Category", "Serialize")];
//        [Fact];
//        METHOD PszTestJson AS VOID
//             LOCAL p1 := String2Psz("The quick brown fox jumps over the lazy dog") AS PSZ
//             LOCAL p2 AS PSZ
//             LOCAL s AS STRING
//             LOCAL o AS OBJECT
//             s := JsonConvert.SerializeObject((OBJECT) p1,Formatting.Indented)
//             o := JsonConvert.DeserializeObject(s, typeof(PSZ))
//             Assert.True(o IS PSZ)
//             p2 := (PSZ ) o
//             Assert.True (p1 == p2)
//            RETURN

        [Trait("Category", "Serialize")];
        [Fact];
        METHOD PszTestBinary AS VOID
             LOCAL p1 := String2Psz("The quick brown fox jumps over the lazy dog") AS PSZ
             VAR o := SELF:BinarySaveRestore(p1)
             Assert.True(o IS PSZ)
             // Cannot cast OBJECT to PSZ ?
             //VAR p2 := (PSZ) o
             VAR type := o:GetType()
             VAR prop := type:GetProperty("Address")
             VAR p  := (IntPtr) prop:GetValue(o, NULL)
             VAR p2   := PSZ{p}
             Assert.True (p1 == p2)
            RETURN

        [Trait("Category", "Serialize")];
        [Fact];
        METHOD WinBoolTestJson AS VOID
             LOCAL b1 := TRUE AS __WinBool
             VAR o := SELF:JsonSaveRestore(b1)
             Assert.True(o IS __WinBool)
             VAR b2 := (__WinBool ) o
             Assert.True (b1 == b2)
            RETURN
       [Trait("Category", "Serialize")];
        [Fact];
        METHOD WinBoolTestBinary AS VOID
             LOCAL b1 := TRUE AS __WinBool
             VAR o := SELF:BinarySaveRestore(b1)
             Assert.True(o IS __WinBool)
             VAR b2 := (__WinBool ) o
             Assert.True (b1 == b2)
            RETURN
            /// Serializing USUAL values to Json is a challenge
            // Calling SerializeObject will call ToObject() and will extract the contents of the usual
        /*
        [Trait("Category", "Serialize")];
        [Fact];
        METHOD UsualTestJson AS VOID
             LOCAL u1 := TRUE AS USUAL
             LOCAL u2 AS USUAL
             LOCAL s AS STRING
             LOCAL o AS OBJECT
             u2 := SaveRestoreUsual(u1)
             Assert.True (u1 == u2)
             u1 := ToDay()
             u2 := SaveRestoreUsual(u1)
             Assert.True (u1 == u2)
             u1 := 42
             u2 := SaveRestoreUsual(u1)
             Assert.True (u1 == u2)
             u1 := "someday I will fly away"
             u2 := SaveRestoreUsual(u1)
             Assert.True (u1 == u2)
             u1 := #IAmASymbol
             u2 := SaveRestoreUsual(u1)
             Assert.True (u1 == u2)
            RETURN
            */
        [Trait("Category", "Serialize")];
        [Fact];
        METHOD UsualTestBinary AS VOID
             LOCAL u1 := TRUE AS USUAL
             LOCAL s AS STRING
             LOCAL o AS OBJECT
             o := SELF:BinarySaveRestore(u1)
             Assert.True (o IS USUAL)
             VAR u2 := o
             Assert.True (u1 == u2)
             u1 := ToDay()
             o := SELF:BinarySaveRestore(u1)
             Assert.True (o IS USUAL)
             u2 := o
             Assert.True (u1 == u2)
             u1 := 42
             o := SELF:BinarySaveRestore(u1)
             Assert.True (o IS USUAL)
             u2 := o
             Assert.True (u1 == u2)
             u1 := "one day I will fly away"
             o := SELF:BinarySaveRestore(u1)
             Assert.True (o IS USUAL)
             u2 := o
             Assert.True (u1 == u2)
             u1 := #IAmASymbol //symbol
             o := SELF:BinarySaveRestore(u1)
             Assert.True (o IS USUAL)
             u2 := o
             Assert.True (u1 == u2)
             u1 := (FLOAT) 1.234
             o := SELF:BinarySaveRestore(u1)
             Assert.True (o IS USUAL)
             u2 := o
             Assert.True (u1 == u2)
             u1 := $1234.56 // Currency
             o := SELF:BinarySaveRestore(u1)
             Assert.True (o IS USUAL)
             u2 := o
             Assert.True (u1 == u2)
             u1 := 0h987654 // Binary
             o := SELF:BinarySaveRestore(u1)
             Assert.True (o IS USUAL)
             u2 := o
             Assert.True (u1 == u2)

            RETURN
            

        [Trait("Category", "Serialize")];
        [Fact];
        METHOD ArrayTestJson AS VOID
             LOCAL a1 := {1,"a",TRUE,42} AS ARRAY
             LOCAL s AS STRING
             VAR o := SELF:JsonSaveRestore(a1, typeof(OBJECT[]))
             Assert.True(o IS OBJECT[])
             VAR a2 := ARRAY{ (OBJECT[]) o}
             Assert.True(Alen(a1) == ALen(a2))
             // Json can only save "normal" types in an array
             // Json converter stores Int64 number for Long and String for date
//             FOR VAR i := 1 TO ALen(a1)
//                    Assert.True( a1[i] == a2[i])
//             NEXT
            RETURN

        [Trait("Category", "Serialize")];
        [Fact];
        METHOD ArrayTestBinary AS VOID
             LOCAL a1 := {1,"a",TRUE,today(), 42} AS ARRAY
             LOCAL s AS STRING
             VAR o := SELF:BinarySaveRestore(a1)
             Assert.True(o IS ARRAY)
             VAR a2 := (ARRAY) o
             Assert.True(Alen(a1) == ALen(a2))
             FOR VAR i := 1 TO ALen(a1)
                    Assert.True( a1[i] == a2[i])
             NEXT
            RETURN



END CLASS

END NAMESPACE // XSharp.Runtime.Tests
