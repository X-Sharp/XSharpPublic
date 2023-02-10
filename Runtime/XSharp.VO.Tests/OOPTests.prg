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
// These test require late binding, that is why we enable late binding for this file only
#pragma options ("lb", ON)
#pragma warnings(169, off)  // unused field

BEGIN NAMESPACE XSharp.VO.Tests



	CLASS OOPTests

		[Fact, Trait("Category", "OOP")];
		METHOD CreateInstanceTests() AS VOID
			LOCAL oObject AS OBJECT
            LOCAL oObject2 AS TestStrong
			/// note that vulcan does not return true for IsClassOf(#Tester, "Object")
			oObject := CreateInstance(#Tester)
			Assert.NotEqual(NULL_OBJECT, oObject)
			IVarPut(oObject,"Name", "X#")
			Assert.Equal("X#", IVarGet(oObject, "Name"))
			IVarPut(oObject,"Age",42)
			Assert.Equal(42, (INT) IVarGet(oObject, "Age"))
            oObject2 := CreateInstance(#TestStrong) // no parameters passed
            Assert.Equal(NULL, oObject2:Param)
            oObject2 := CreateInstance(#TestStrong, oObject, oObject) // too many parameters passed
            Assert.Equal(oObject, oObject2:Param)
            // New _CreateInstance that takes an USUAL[] for the parameters
            var usuals := List<USUAL>{}{oObject, oObject}
            oObject2 := _CreateInstance(#TestStrong, usuals:ToArray()) // too many parameters passed
            Assert.Equal(oObject, oObject2:Param)


		[Fact, Trait("Category", "OOP")];
		METHOD MetadataTests() AS VOID
			LOCAL oObject AS OBJECT
			LOCAL uValue AS USUAL
			Assert.Equal(TRUE, IsClass("tester"))
			Assert.Equal(TRUE, IsClass(#tester))
			Assert.Equal(TRUE, IsClassOf(#tester,#Father))
			oObject := CreateInstance(#Tester)
			// ClassName
			Assert.Equal("TESTER", ClassName(oObject))
			// IsMethod
			Assert.Equal(FALSE, IsMethod(oObject, "Doesnotexist"))
			Assert.Equal(TRUE, IsMethod(oObject, "GetHashCode"))
			// InstanceOf
			Assert.Equal(TRUE, IsInstanceOf(oObject, #Father))
			uValue := oObject
			Assert.Equal(TRUE, IsInstanceOfUsual(uValue, #Father))
			// IVarLIst
			VAR aVars := IvarList(oObject)
			Assert.Equal(3, (INT) ALen(aVars))
			// MethodList
			VAR aMethods := MethodList(oObject)
			Assert.Equal(7, (INT) ALen(aMethods))		// 4 METHODS of the OBJECT CLASS + TestMe + TestMe2
			// ClassTree
			VAR aTree := ClassTree(oObject)
			Assert.Equal(3, (INT) ALen(atree))		// TESTER, FATHER and OBJECT
			aTree := ClassTreeClass(#Tester)
			Assert.Equal(3, (INT) ALen(atree))		// TESTER, FATHER and OBJECT

			aTree := OOPTree(oObject)
			Assert.Equal(3, (INT) ALen(aTree))			// 3 classes, TESTER, FATHER and OBJECT
			Assert.Equal(3, (INT) ALen(aTree[1]))		// symbol, Ivars, Methods
			Assert.Equal(3, (INT) ALen(aTree[1][2]))	// 2 Ivars: Name & Age & FullName = same as IVarLIst
			Assert.Equal(9, (INT) ALen(aTree[1][3]))	// 9 Methods = same as MethodList + 2 non public methods
			aTree := OOPTreeClass(#tester)
			Assert.Equal(3, (INT) ALen(aTree))			// 3 classes, TESTER, FATHER and OBJECT
			Assert.Equal(3, (INT) ALen(aTree[1]))		// symbol, Ivars, Methods
			Assert.Equal(3, (INT) ALen(aTree[1][2]))	// 2 Ivars: Name & Age & FullName = same as IVarLIst
			Assert.Equal(9, (INT) ALen(aTree[1][3]))	// 9 Methods = same as MethodList + 2 non public methods



		[Fact, Trait("Category", "OOP")];
		METHOD SendTests() AS VOID
			LOCAL oObject AS OBJECT
			oObject := CreateInstance(#Tester)
			Assert.Equal(2121+1+2+3, (INT) Send(oObject, #TestMe,1,2,3))
			Assert.Equal(4242+1+2+3, (INT) Send(oObject, #TestMe2,1,2,3))
			Assert.Equal(6363+1+2+3, (INT) Send(oObject, #TestMe3,1.0,2,3))             // the float causes the USUAL overload to be called
			Assert.Equal(8484+1+2+3, (INT) __InternalSend(oObject, #TestMe3,1,2,3))     // all int so the int overload gets called

			LOCAL uObject AS USUAL
			LOCAL usMethod := #NewTest AS USUAL
			LOCAL sMethod := #NewTest AS SYMBOL
			oObject := SendTester{}
			uObject := oObject

			Assert.True(Send(oObject, #NewTest, 1) == 1)
			Assert.True(Send(uObject, #NewTest, 2) == 2)
			Assert.True(Send(oObject, sMethod,  3) == 3)
			Assert.True(Send(uObject, sMethod,  4) == 4)
			Assert.True(Send(oObject, usMethod, 5) == 5)
			Assert.True(Send(uObject, usMethod, 6) == 6)

		[Fact, Trait("Category", "OOP")];
		METHOD _SendTests() AS VOID
            // Test _Send that takes MethodInfo parameter
			LOCAL oObject AS OBJECT
			oObject := CreateInstance(#Tester)
            VAR mi := Typeof(tester):GetMethod("TestMe")
            VAR mi2 := Typeof(tester):GetMethod("TestMe2")
            VAR mi3u := Typeof(tester):GetMethods():Where ( { m => m:Name:ToUpper()=="TESTME3" .AND. m:GetParameters()[1]:ParameterType:FullName=="XSharp.__Usual"}):First()
            VAR mi3i := Typeof(tester):GetMethods():Where ( { m => m:Name:ToUpper()=="TESTME3" .AND. m:GetParameters()[1]:ParameterType:FullName!="XSharp.__Usual"}):First()
			Assert.Equal(2121+1+2+3, (INT) _Send(oObject, mi,1,2,3))
			Assert.Equal(2121+4+5+6, (INT) _Send(oObject, mi,4,5,6))
			Assert.Equal(4242+1+2+3, (INT) _Send(oObject, mi2,1,2,3))
			Assert.Equal(4242+4+5+6, (INT) _Send(oObject, mi2,4,5,6))
			Assert.Equal(6363+1+2+3, (INT) _Send(oObject, mi3u,1.0,2,3))             // USUAL overload
			Assert.Equal(6363+4+5+6, (INT) _Send(oObject, mi3u,4.0,5,6))             // USUAL overload
			Assert.Equal(8484+1+2+3, (INT) _Send(oObject, mi3i,1.0,2,3))             // INT overload, runtime will convert 1.0 to 1
			Assert.Equal(8484+4+5+6, (INT) _Send(oObject, mi3i,4.0,5,6))             // INT overload, runtime will convert 4.0 to 1

		[Fact, Trait("Category", "OOP")];
		METHOD NoMethodTests() AS VOID
			LOCAL oObject AS OBJECT
			oObject := CreateInstance("ClassWithNoMethod")
			assert.Equal(9, (INT) Send(oObject, "ADD",2,3,4))
			assert.Equal(24, (INT) Send(oObject, "MUL",2,3,4))
            //assert.Equal(24, (int) oObject:Mul(2,3,4))
			assert.Equal("DIV", (STRING) Send(oObject, "DIV",2,3,4))

		[Fact, Trait("Category", "OOP")];
		METHOD ParamCountTests() AS VOID
            VObject{}
			assert.Equal(3, (INT) FParamCount("STR"))
			assert.Equal(2, (INT) FParamCount("STR2"))
			assert.Equal(3, (INT) FParamCount("STR3"))
			assert.Equal(3, (INT) MParamCount(#Tester, #TestMe))
			assert.Equal(0, (INT) MParamCount("VObject", "Destroy"))
			//assert.Throws( FParamCount("ProcName"))	// ambiguous


		[Fact, Trait("Category", "OOP")];
		METHOD CallClipFuncTests() AS VOID
			SetDecimalSep( (WORD) c'.')
			assert.Equal("10.00", (STRING) _CallClipFunc("STR", {10,5,2}))
			assert.Equal("   10.01", (STRING) _CallClipFunc("STR", {10.01,8,2}))
			assert.Equal("   10.02", (STRING) _CallClipFunc("STR3", {10.02,8,2}))
			assert.Equal("2.50", (STRING) _CallClipFunc("STR3", {2.49999,4,2}))


		[Fact, Trait("Category", "OOP")];
		METHOD ObjectToArrayTest() AS VOID
			LOCAL oObject AS OBJECT
			oObject := CreateInstance(#Tester)
			Assert.Equal(3, (INT) ALen(Object2Array(oObject)))

		[Fact, Trait("Category", "OOP")];
		METHOD LateBindingMethods() AS VOID
			LOCAL u AS USUAL
			u := AnotherClass{}
			u:TestOther()
			u:TestOtherC()
			Assert.Equal(1, (INT)u:TestOther1())
			Assert.Equal("22", (STRING)u:TestOther2(2))
			Assert.Equal("22", (STRING)u:TestOther2C(2))

			u := Tester{}
			Assert.Equal(2121+1+2+3, (INT)u:TestMe(1,2,3))
			Assert.Equal(4242+1+2+3, (INT)u:TestMe2(1,2,3))
            Assert.Equal(6363+1+2+3, (INT)u:TestMe3(1.0,2,3))   // overloaded. The float calls the USUAL variant
			Assert.Equal(8484+1+2+3, (INT)u:TestMe3(1,2,3))     // overloaded. all ints, so the INT variant is called
		RETURN

		[Fact, Trait("Category", "OOP")];
		METHOD LateBindingFields_Props() AS VOID
			LOCAL u AS USUAL
			u := Tester{}

			u:age := 42
			Assert.Equal(42, (INT)u:AGE)

			u:FULLNAME := "Olympiacos"
			Assert.Equal("Olympiacos", u:fullNAme)

            U := TestClassChild{}
            Assert.Equal(u:Name, "TestClassChild")
            u:Name := "This class has a setter in the parent Only"

		RETURN

#pragma options ("vo7", ON)
		[Fact, Trait("Category", "OOP")];
		METHOD LateBindingFields_ParamsbyReference() AS VOID
			LOCAL u AS USUAL
			u := LBTestClass{}

			LOCAL l AS LOGIC
			l := TRUE
			u:TestUntyped(REF l)
			Assert.Equal(FALSE, l)

			l := TRUE
			u:TestUntyped(@l)
			Assert.Equal(FALSE, l)


			LOCAL i AS INT
			i := 555
			u:TestUntyped(REF i)
			Assert.Equal(123, i)

			i := 555
			u:TestUntyped(@i)
			Assert.Equal(123, i)


			LOCAL c AS STRING
			c := "asd"
			u:TestUntyped(REF c)
			Assert.Equal("test", c)

			c := "asd"
			u:TestUntyped(@c)
			Assert.Equal("test", c)


			LOCAL d AS DATE
			d := Today()
			u:TestUntyped(REF d)
			Assert.Equal(Today()-1, d)

			d := Today()
			u:TestUntyped(@d)
			Assert.Equal(Today()-1, d)


			LOCAL f AS FLOAT
			u:TestUntyped(@f)
            Assert.Equal(f, 123.0)
			LOCAL r8 AS REAL8
			u:TestUntyped(@r8)
            Assert.Equal(r8, (REAL8) 123.0)
			LOCAL r4 AS REAL4
			u:TestUntyped(@r4)
            Assert.Equal(r4, (REAL4)123.0)
			LOCAL uu AS USUAL
			u:TestUntyped(@uu)
			LOCAL s := #ASD AS SYMBOL
			u:TestUntyped(@s)



			LOCAL n AS INT
			n := 1
			u:TestInt(REF n)
			Assert.Equal(123, n)
			n := 1
			u:TestInt(@n)
			Assert.Equal(123, n)

			l := FALSE
			u:TestLogic(REF l)
			Assert.Equal(TRUE, l)
			l := FALSE
			u:TestLogic(@l)
			Assert.Equal(TRUE, l)

			c := ""
			u:TestString(REF c)
			Assert.Equal("changed", c)
			c := ""
			u:TestString(@c)
			Assert.Equal("changed", c)


		RETURN
#pragma options ("vo7", DEFAULT)

        [Fact, Trait("Category", "OOP")];
        METHOD IConvertibleTest() AS VOID
           //Issue 1 - runtime - Object must implement IConvertible
            LOCAL o AS AzControl
            LOCAL x AS OBJECT

            o := AzControl{}
            o:cbWhen := {||TRUE}

            x := o

            x:cbWhen := {||TRUE}

            Assert.NotEqual(o:cbWhen, NULL)

		[Fact, Trait("Category", "OOP")];
		METHOD IsClassOf_Tests() AS VOID
			Assert.True(IsClassOf(#TestClassChild, #TestClassParent))
			Assert.False(IsClassOf(#TestClassParent, #TestClassChild))
			Assert.True(IsClassOf(#TestClassChild, #TestClassChild))
			Assert.True(IsClassOf(#TestClassParent, #TestClassParent))
			Assert.False(IsClassOf(#None, #None))
			Assert.False(IsClassOf(#None, #TestClassChild))
			Assert.False(IsClassOf(#TestClassChild, #None))

		[Fact, Trait("Category", "OOP")];
		METHOD IsInstanceOf_Tests() AS VOID
			Assert.True(IsInstanceOf(123 , "Int32"))
			Assert.True(IsInstanceOf(TRUE , "Boolean"))
			Assert.False(IsInstanceOf(123 , "Nothing"))

        [Fact, Trait("Category", "OOP")];
        METHOD IsAccessAssignMethod_tests() AS VOID
        	LOCAL o AS GeneralLBTestClass
        	o := GeneralLBTestClass{}
        	Assert.True( IsAccess(o , #acc_exp) )
        	Assert.True( IsAccess(o , #acc_prot) )
        	Assert.True( IsAccess(o , #acc_priv) )
        	Assert.True( IsAssign(o , #asn_exp) )
        	Assert.True( IsAssign(o , #asn_prot) )
        	Assert.True( IsAssign(o , #asn_priv) )


        	Assert.False( IsAssign(o , #acc_exp) )
        	Assert.False( IsAssign(o , #acc_prot) )
        	Assert.False( IsAccess(o , #asn_exp) )
        	Assert.False( IsAccess(o , #asn_prot) )

        	Assert.True( IsMethod(o , #meth_exp) )
        	Assert.True( IsMethod(o , #meth_prot) )
        	Assert.True( IsMethod(o , #meth_priv) )
            Assert.True( IsMethod(o , #methodoverloaded) )

        [Fact, Trait("Category", "OOP")];
        METHOD IsAccessAssignMethod_Nulltests() AS VOID
        	LOCAL o AS OBJECT
        	o := NULL_OBJECT
        	Assert.False( IsAccess(o , #acc_exp) )
        	Assert.False( IsAssign(o , #acc_exp) )
        	Assert.False( IsMethod(o , #meth_exp) )



        [Fact, Trait("Category", "OOP")];
        METHOD IVarPutGetSet_tests() AS VOID
        	LOCAL o AS GeneralLBTestClass
        	o := GeneralLBTestClass{}

        	IVarPut(o , #fld_exp , 1)
        	Assert.Equal(1 , (INT) IVarGet(o , #fld_exp))

        	IVarPutSelf(o , #fld_prot , 2)
        	Assert.Equal(2 , (INT) IVarGetSelf(o , #fld_prot))

        	Assert.ThrowsAny<Exception>( { => IVarPut(o , #fld_prot , 3) })
        	Assert.Equal(2 , (INT) IVarGetSelf(o , #fld_prot))

        	Assert.ThrowsAny<Exception>( { => IVarGet(o , #fld_prot) })
        	Assert.ThrowsAny<Exception>( { => IVarGet(o , #fld_priv) })


        [Fact, Trait("Category", "OOP")];
        METHOD IVarPutVisibilitytests() AS VOID
        	LOCAL o AS OOpVisTestClass
            o := OOpVisTestClass{}
            Assert.ThrowsAny<Exception>( { => IVarGet(o , #PrivateDummy) })
            Assert.ThrowsAny<Exception>( { => IVarPut(o , #PrivateDummy,10) })
            Assert.Equal( (INT) IVarGet(o , #PrivateSetDummy) ,42)
            Assert.ThrowsAny<Exception>( { => IVarPut(o , #PrivateSetDummy,10) })


        [Fact, Trait("Category", "OOP")];
        METHOD PtrAndIntPtrMethodCalls() AS VOID
        	LOCAL o AS GeneralLBTestClass
        	o := GeneralLBTestClass{}

        	LOCAL pPtr AS PTR
        	LOCAL pIntPtr AS PTR
        	pPtr := o:MethodPtr()
        	? pPtr
        	pIntPtr := o:MethodPtr()
        	? pIntPtr
        	Assert.NotEqual((INT)pIntPtr , 0)
        	Assert.NotEqual((INT)pPtr , 0)

        	pPtr := o:MethodIntPtr()
        	? pPtr
        	pIntPtr := o:MethodIntPtr()
        	? pIntPtr
        	Assert.NotEqual((INT)pIntPtr , 0)
        	Assert.NotEqual((INT)pPtr , 0)

        	LOCAL u AS USUAL
        	u := o
        	pPtr := u:MethodPtr()
        	? pPtr
        	pIntPtr := u:MethodPtr()
        	? pIntPtr
        	Assert.NotEqual((INT)pIntPtr , 0)
        	Assert.NotEqual((INT)pPtr , 0)

        	pPtr := u:MethodIntPtr()
        	? pPtr
        	pIntPtr := u:MethodIntPtr()
        	? pIntPtr
        	Assert.NotEqual((INT)pIntPtr , 0)
        	Assert.NotEqual((INT)pPtr , 0)

        [Fact, Trait("Category", "OOP")];
        METHOD DefaultParameters() AS VOID
        	LOCAL o AS USUAL
        	o := AnotherClass{}

        	Assert.Equal(60 , (INT)o:DefaultParams1(10,20,30))
        	Assert.Equal(15 , (INT)o:DefaultParams1(10))
        	Assert.Equal(32 , (INT)o:DefaultParams1(10,,20)) // doesn't work in VO either
        	Assert.Equal(113 , (INT)o:DefaultParams1(10,100))

        	Assert.Equal("TESTFALSE" , (STRING)o:DefaultParams2("TEST",FALSE))
        	Assert.Equal("TESTTRUE" , (STRING)o:DefaultParams2("TEST",TRUE))
        	Assert.Equal("TESTTRUE" , (STRING)o:DefaultParams2("TEST"))
        	Assert.Equal("abcTRUE" , (STRING)o:DefaultParams2())

        	Assert.Equal(2000.10.20 , (DATE)o:DefaultParams3(2000.10.20))
        	Assert.Equal(2018.10.20 , (DATE)o:DefaultParams3())

        	Assert.Equal(#MYSYMBOL , (SYMBOL)o:DefaultParams4())

        	Assert.Equal(TRUE, (LOGIC)o:DefaultParams5())
        	Assert.Equal(TRUE, (LOGIC)o:DefaultParams6())

		// TECH-6YD882O372, Runtime exception with inexact equals operator and NIL values
		[Fact, Trait("Category", "StringUsual")];
		METHOD StringNil_InExactEquals() AS VOID
			LOCAL u := NIL AS USUAL
			LOCAL c := "abc" AS STRING
			Assert.False( c == u ) // FALSE, ok
			Assert.False( c = u  ) // FALSE, ok
			Assert.False( u == c ) // FALSE, ok
			Assert.False( u = c  ) // exception
			Assert.False( u = "aaa"  ) // exception

			// exceptions also:
			u := NilTestClass{}
			Assert.False( u:NilMethod() = "A" )
			Assert.False( u:NilMethodUntyped() = "A" )
			Assert.False( u:NilAccess = "A" )
		RETURN

		// TECH-CLZ71NH0C3, Runtime exception with return value of NoIVarGet()
        [Fact, Trait("Category", "OOP")];
        METHOD NilComparisons() AS VOID
			LOCAL n := 0 AS INT
			LOCAL c := "abc" AS STRING
			LOCAL uNil := NIL AS USUAL
			Assert.False( n == NIL  )
			Assert.False( n == uNil )
			Assert.False( NIL == n  )
			Assert.False( uNil == n )

			Assert.False( uNil == c )
			Assert.False( c == NIL  )
			Assert.False( c == uNil )

			LOCAL o AS USUAL
			o := NilTestClass{}
			Assert.False( o:NilMethod() == 1        )
			Assert.False( o:NilMethodUntyped() == 1 )
			Assert.False( o:NilMethodUntyped() == c )

			Assert.False( o:NilAccess == 1  )
			Assert.False( o:NilAccess == "" )

			Assert.False( o:DoesNotExistAccess == 1   )
			Assert.False( o:DoesNotExistMethod() == 1 )

			Assert.False( o:DoesNotExistMethodNil() == 1     )
			Assert.False( o:DoesNotExistMethodNil() == "abc" )

			Assert.False( o:DoesNotExistAccessNil == 1     ) // exception here "Value does not fall within the expected range."
			Assert.False( o:DoesNotExistAccessNil == "abc" ) // exception
			Assert.False( o:DoesNotExistAccessNil == n     ) // exception

			Assert.True( o:DoesNotExistAccessNil == NIL   ) // OK, TRUE
		RETURN


		// TECH-R2TJNB9TZ7, Problem with late bound assign of DateTime to DATE iVar
		[Fact, Trait("Category", "Date")];
		METHOD Date_to_DateTime_and_back() AS VOID

			LOCAL o := DateClass{} AS DateClass

			o:FieldDate := DateTime{2000,12,1}
			Assert.Equal(o:FieldDate , ConDate(2000,12,1))
			o:AssignDate := DateTime{2000,12,2}
			Assert.Equal(o:FieldDate , ConDate(2000,12,2))
			Assert.Equal(o:AccessDate , ConDate(2000,12,2))

			o:FieldDateTime := ConDate(2020,12,1)
			Assert.Equal(o:FieldDateTime , DateTime{2020,12,1})
			o:AssignDateTime := ConDate(2020,12,2)
			Assert.Equal(o:FieldDateTime , DateTime{2020,12,2})
			Assert.Equal(o:AccessDateTime , DateTime{2020,12,2})

			o:Date_DateTime_Method(DateTime{2010,1,1} , 2012.02.02)
			Assert.Equal(o:FieldDate , 2010.01.01)
			Assert.Equal(o:FieldDateTime , DateTime{2012,02,02})


			LOCAL u := DateClass{} AS USUAL

			u:FieldDate := DateTime{2000,12,1}
			Assert.Equal((DATE)u:FieldDate , ConDate(2000,12,1))
			u:AssignDate := DateTime{2000,12,2}
			Assert.Equal((DATE)u:FieldDate , ConDate(2000,12,2))
			Assert.Equal((DATE)u:AccessDate , ConDate(2000,12,2))

			u:FieldDateTime := ConDate(2020,12,1)
			Assert.Equal((DateTime)u:FieldDateTime , DateTime{2020,12,1})
			u:AssignDateTime := ConDate(2020,12,2)
			Assert.Equal((DateTime)u:FieldDateTime , DateTime{2020,12,2})
			Assert.Equal((DateTime)u:AccessDateTime , DateTime{2020,12,2})

			u:Date_DateTime_Method(DateTime{2010,1,1} , 2012.02.02)
			Assert.Equal((DATE) u:FieldDate , 2010.01.01)
			Assert.Equal((DateTime) u:FieldDateTime , DateTime{2012,02,02})
		RETURN

		CLASS DateClass
			EXPORT FieldDate := Today() AS DATE
			EXPORT FieldDateTime := DateTime.Now AS DateTime
			ASSIGN AssignDate(val AS DATE)
				SELF:FieldDate := val
			ASSIGN AssignDateTime(val AS DateTime)
				SELF:FieldDateTime := val
			ACCESS AccessDate AS DATE
				RETURN SELF:FieldDate
			ACCESS AccessDateTime AS DateTime
				RETURN SELF:FieldDateTime
			METHOD Date_DateTime_Method(d AS DATE, dt AS DateTime) AS VOID
				SELF:FieldDate := d
				SELF:FieldDateTime := dt
			RETURN
		END CLASS

        CLASS AzControl

            EXPORT cbWhen AS CODEBLOCK

        END CLASS

        [Fact, Trait("Category", "OOP")];
		METHOD SymbolLateBound() AS VOID
			LOCAL u AS USUAL
			u := SymbolTest{}
			Assert.True(u:sym == #sym)
			Assert.True(u:sym == #SYM)
			Assert.True(u:symEmpty == NULL_SYMBOL)
			Assert.False(Empty(u:sym))
			Assert.True(Empty(u:symEmpty))
			Assert.True(u:symUsual == #SymUsual)

			u:sym := #test
			Assert.True(u:sym == #TEST)
			u:symUsual := #test
			Assert.True(u:symUsual == #TEST)

			u:sym := NULL_SYMBOL
			Assert.True(u:sym == NULL_SYMBOL)
			u:symUsual := NULL_SYMBOL
			Assert.True(u:symUsual == NULL_SYMBOL)
		RETURN

		INTERNAL CLASS SymbolTest
			EXPORT sym := "sym" AS SYMBOL
			EXPORT symEmpty := NULL_SYMBOL AS SYMBOL
			EXPORT symUsual := #SymUsual AS USUAL
		END CLASS


        [Fact, Trait("Category", "OOP")];
		METHOD IVarsLateBound() AS VOID
			LOCAL u AS USUAL
			u := TestIVars{}

			u:InternalGet := "abc"
			Assert.True( IVarGetSelf(u , "InternalGet") == "abc" )
			Assert.True( u:InternalGet == "abc" )

			u:ProtectedGet := "123"
			Assert.True( IVarGetSelf(u , "ProtectedGet") == "123" )

			u:PrivateGet := "456"
			Assert.True( IVarGetSelf(u , "PrivateGet") == "456" )


			u:InternalSET := "abc123"
			Assert.True( IVarGet(u , "InternalSET") == "abc123" )

			IVarPutSelf( u , "InternalSET" , "set")
			Assert.True( IVarGetSelf(u , "InternalSET") == "set" )
			Assert.True( u:InternalSET == "set" )

			IVarPutSelf( u , "ProtectedSET" , "SET")
			Assert.True( IVarGet(u , "ProtectedSET") == "SET" )
			Assert.True( u:ProtectedSET == "SET" )

			IVarPutSelf( u , "PrivateSET" , "abc")
			Assert.True( u:PrivateSET == "abc" )
			Assert.True( IVarGetSelf(u , "PrivateSET") == "abc" )
		RETURN

		INTERNAL CLASS TestIVars
			PROPERTY InternalGet AS STRING AUTO INTERNAL GET SET
			PROPERTY ProtectedGet AS STRING AUTO PROTECTED GET SET
			PROPERTY PrivateGet AS STRING AUTO PRIVATE GET SET

			PROPERTY InternalSET AS STRING AUTO GET INTERNAL SET
			PROPERTY ProtectedSET AS STRING AUTO GET PROTECTED SET
			PROPERTY PrivateSET AS STRING AUTO GET PRIVATE SET
		END CLASS

	END CLASS


END NAMESPACE

CLASS SendTester
	METHOD NewTest(u)
	RETURN u
END CLASS

CLASS Tester INHERIT father
	PROPERTY name AS STRING AUTO
	PROPERTY age AS INT AUTO
	EXPORT fullname AS STRING
CONSTRUCTOR CLIPPER
	METHOD TestMe(a AS INT,b AS INT,c AS INT) AS LONG
		RETURN 2121+a+b+c
	METHOD TestMe2(a,b, c ) AS LONG
		RETURN 4242+a+b+c
	METHOD TestMe3(a AS USUAL,b AS USUAL, c AS USUAL) AS LONG
		RETURN 6363+a+b+c
	METHOD TestMe3(a AS INT,b AS INT, c AS INT) AS LONG
        RETURN 8484+a+b+c
END CLASS

CLASS Father
END CLASS


CLASS ClassWithNoMethod
	METHOD NoMethod(arg1, arg2, arg3 , uNil)
		LOCAL cMethodName AS STRING
		cMethodName := XSharp.RT.Functions.NoMethod()
		IF cMethodName = "ADD"
			RETURN arg1+arg2+arg3
		ELSEIF cMethodName = "MUL"
			RETURN arg1*arg2*arg3
		ENDIF
		Assert.True(IsNil(uNil))
		RETURN cMethodName
END CLASS

CLASS AnotherClass
	METHOD TestOther() AS VOID PASCAL
	METHOD TestOtherC() CLIPPER
	RETURN NIL
	METHOD TestOther1() AS INT
	RETURN 1
	METHOD TestOther2(n AS INT) AS STRING
	RETURN "2" + n:ToString()
	METHOD TestOther2C(n)
	RETURN "2" + ((INT)n):ToString()

	METHOD DefaultParams1(n1 AS INT, n2 := 2 AS INT, n3 := 3 AS INT) AS INT
	RETURN n1 + n2 + n3
	METHOD DefaultParams2(cValue := "abc" AS STRING, lValue := TRUE AS LOGIC) AS STRING
	RETURN cValue + lValue:ToString():ToUpper()
	METHOD DefaultParams3(dValue := 2018.10.20 AS DATE) AS DATE
	RETURN dValue
	METHOD DefaultParams4(sValue := #MYSYMBOL AS SYMBOL) AS SYMBOL
	RETURN sValue
	METHOD DefaultParams5(uParam := NULL AS USUAL) AS LOGIC
	RETURN IsNil(uParam)
	METHOD DefaultParams6(uParam := NIL AS USUAL) AS LOGIC
	RETURN IsNil(uParam)

END CLASS

CLASS GeneralLBTestClass
	EXPORT fld_exp AS INT
	PROTECT fld_prot AS INT
	PRIVATE fld_priv AS INT
	INSTANCE fld_inst AS INT
	ACCESS acc_exp AS INT
	RETURN 0
	PROTECT ACCESS acc_prot AS INT
	RETURN 0
	PRIVATE ACCESS acc_priv AS INT
	RETURN 0

	ASSIGN asn_exp(n AS INT)
	PROTECT ASSIGN asn_prot(n AS INT)
	PROTECT ASSIGN asn_priv(n AS INT)

	METHOD meth_exp(a,b,c,d)
	RETURN NIL
	PROTECTED METHOD meth_prot(a,b,c,d)
	RETURN NIL
	PRIVATE METHOD meth_priv(a,b,c,d)
	RETURN NIL

    METHOD MethodOverloaded AS LOGIC
        RETURN TRUE

    METHOD MethodOverloaded(lParam AS LOGIC) AS LOGIC
        RETURN lParam

	METHOD MethodPtr() AS PTR
		LOCAL n AS INT
	RETURN @n
	METHOD MethodIntPtr() AS IntPtr
		LOCAL n AS INT
	RETURN @n
END CLASS


CLASS TestClassParent
    VIRTUAL ACCESS Name AS STRING
        RETURN "TestClassParent"
    VIRTUAL ASSIGN Name(cValue AS STRING)
        NOP
END CLASS
CLASS TestClassChild INHERIT TestClassParent
    OVERRIDE ACCESS Name AS STRING
        RETURN "TestClassChild"
END CLASS

CLASS TestStrong
    PROPERTY Param AS OBJECT AUTO
    CONSTRUCTOR(otest AS OBJECT)
        SELF:Param := oTest
END CLASS


CLASS NilTestClass
	METHOD NilMethod() AS USUAL
	RETURN NIL
	METHOD NilMethodUntyped()
	RETURN NIL
	ACCESS NilAccess
	RETURN NIL

	METHOD NoMethod(c)
		? c
		IF AsString(c) == "DOESNOTEXISTMETHOD"
			RETURN 2
		END IF
	RETURN NIL
	METHOD NoIVarGet(c AS SYMBOL) AS USUAL
		? c
		IF AsString(c) == "DOESNOTEXISTACCESS"
			RETURN 2
		END IF
	RETURN NIL
END CLASS

CLASS LBTestClass
	METHOD TestUntyped(u)
		DO CASE
		CASE IsString(u)
			u := "test"
		CASE IsNumeric(u)
			u := 123
		CASE IsDate(u)
			u := Today() - 1
		CASE IsLogic(u)
			u := FALSE
		END CASE
	RETURN NIL

	METHOD TestInt(n REF INT) AS VOID
	n := 123
	METHOD TestLogic(l REF LOGIC) AS VOID
	l := TRUE
	METHOD TestString(c REF STRING) AS VOID
	c := "changed"

END CLASS

CLASS OOpVisTestClass

    PRIVATE PROPERTY PrivateDummy AS INT AUTO GET SET
    PUBLIC PROPERTY PrivateSetDummy AS INT AUTO GET PRIVATE SET := 42


END CLASS
