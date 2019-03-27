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
			VAR aVars := IVarList(oObject)
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
			SetDecimalSep( (WORD) '.')
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
		RETURN
        
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
//        	Assert.Equal(32 , (INT)o:DefaultParams1(10,,20)) // doesn't work in VO either
        	Assert.Equal(113 , (INT)o:DefaultParams1(10,100))

        	Assert.Equal("TESTFALSE" , (STRING)o:DefaultParams2("TEST",FALSE))
        	Assert.Equal("TESTTRUE" , (STRING)o:DefaultParams2("TEST",TRUE))
        	Assert.Equal("TESTTRUE" , (STRING)o:DefaultParams2("TEST"))
        	Assert.Equal("abcTRUE" , (STRING)o:DefaultParams2())

        	Assert.Equal(2000.10.20 , (DATE)o:DefaultParams3(2000.10.20))
        	Assert.Equal(2018.10.20 , (DATE)o:DefaultParams3())

        	Assert.Equal(#MYSYMBOL , (SYMBOL)o:DefaultParams4())


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
    
        CLASS AzControl
       
            EXPORT cbWhen AS CODEBLOCK
       
        END CLASS   
	END CLASS


END NAMESPACE

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
	METHOD NoMethod(cMethodName, arg1, arg2, arg3)
		IF cMethodName = "ADD"
			RETURN arg1+arg2+arg3
		ELSEIF cMethodName = "MUL"
			RETURN arg1*arg2*arg3
		ENDIF
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
	
	METHOD MethodPtr() AS PTR
		LOCAL n AS INT
	RETURN @n
	METHOD MethodIntPtr() AS IntPtr
		LOCAL n AS INT
	RETURN @n
END CLASS


CLASS TestClassParent
END CLASS
CLASS TestClassChild INHERIT TestClassParent
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
	METHOD NoIVarGet(c)
		? c
		IF AsString(c) == "DOESNOTEXISTACCESS"
			RETURN 2
		END IF
	RETURN NIL
END CLASS

