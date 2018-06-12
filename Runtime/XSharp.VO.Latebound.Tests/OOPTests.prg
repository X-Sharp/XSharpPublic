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
			/// note that vulcan does not return true for IsClassOf(#Tester, "Object")
			oObject := CreateInstance(#Tester)
			Assert.NotEqual(null_object, oObject)
			IVarPut(oObject,"Name", "X#")
			Assert.Equal("X#", IVarGet(oObject, "Name"))
			IVarPut(oObject,"Age",42)
			Assert.Equal(42, (INT) IVarGet(oObject, "Age"))
		
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
			Assert.Equal(6363+1+2+3, (INT) Send(oObject, #TestMe3,1,2,3))
			Assert.Equal(6363+1+2+3, (INT) __InternalSend(oObject, #TestMe3,1,2,3))
			
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
			Assert.Equal(2121+1+2+3, (int)u:TestMe(1,2,3))
			Assert.Equal(4242+1+2+3, (int)u:TestMe2(1,2,3))
			Assert.Equal(6363+1+2+3, (int)u:TestMe3(1,2,3))
		RETURN

		[Fact, Trait("Category", "OOP")];
		METHOD LateBindingFields_Props() AS VOID
			LOCAL u AS USUAL
			u := Tester{}

			u:age := 42
			Assert.Equal(42, (int)u:AGE)

			u:FULLNAME := "Olympiacos"
			Assert.Equal("Olympiacos", u:fullNAme)
		RETURN

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
END CLASS
