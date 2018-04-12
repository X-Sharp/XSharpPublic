using System
using System.Collections.Generic
using System.Linq
using System.Text
using XUnit



begin namespace XSharp.VO.Tests
	
	
	
	class OOPTests
		
		[Fact, Trait("Category", "OOP")];
		method CreateInstanceTests() as void
			local oObject as object
			/// note that vulcan does not return true for IsClassOf(#Tester, "Object")
			oObject := CreateInstance(#Tester)
			Assert.NotEqual(null_object, oObject)
			IVarPut(oObject,"Name", "X#")
			Assert.Equal("X#", IVarGet(oObject, "Name"))
			IVarPut(oObject,"Age",42)
			Assert.Equal(42, (int) IvarGet(oObject, "Age"))
		
		[Fact, Trait("Category", "OOP")];
		method MetadataTests() as void
			local oObject as object
			local uValue as usual
			Assert.Equal(true, IsClass("tester"))
			Assert.Equal(true, IsClass(#tester))
			Assert.Equal(true, IsClassOf(#tester,#Father))
			oObject := CreateInstance(#Tester)
			// ClassName
			Assert.Equal("TESTER", Classname(oObject))
			// IsMethod
			Assert.Equal(false, IsMethod(oObject, "Doesnotexist"))
			Assert.Equal(true, IsMethod(oObject, "GetHashCode"))
			// InstanceOf
			Assert.Equal(true, IsInstanceOf(oObject, #Father))
			uValue := oObject
			Assert.Equal(true, IsInstanceOfUsual(uValue, #Father))
			// IVarLIst
			var aVars := IVarList(oObject)
			Assert.Equal(2, (int) Alen(aVars))
			// MethodList
			var aMethods := MethodList(oObject)
			Assert.Equal(7, (int) Alen(aMethods))		// 4 METHODS of the OBJECT CLASS + TestMe + TestMe2
			// ClassTree
			var aTree := ClassTree(oObject)
			Assert.Equal(3, (int) Alen(atree))		// TESTER, FATHER and OBJECT
			aTree := ClassTreeClass(#Tester)
			Assert.Equal(3, (int) Alen(atree))		// TESTER, FATHER and OBJECT

			aTree := OopTree(oObject)
			Assert.Equal(3, (int) alen(aTree))			// 3 classes, TESTER, FATHER and OBJECT
			Assert.Equal(3, (int) Alen(aTree[1]))		// symbol, Ivars, Methods
			Assert.Equal(2, (int) Alen(aTree[1][2]))	// 2 Ivars: Name & Age = same as IVarLIst
			Assert.Equal(9, (int) Alen(aTree[1][3]))	// 9 Methods = same as MethodList + 2 non public methods
			aTree := OopTreeClass(#tester)
			Assert.Equal(3, (int) alen(aTree))			// 3 classes, TESTER, FATHER and OBJECT
			Assert.Equal(3, (int) Alen(aTree[1]))		// symbol, Ivars, Methods
			Assert.Equal(2, (int) Alen(aTree[1][2]))	// 2 Ivars: Name & Age = same as IVarLIst
			Assert.Equal(9, (int) Alen(aTree[1][3]))	// 9 Methods = same as MethodList + 2 non public methods


		
		[Fact, Trait("Category", "OOP")];
		method SendTests() as void
			local oObject as object
			oObject := CreateInstance(#Tester)
			Assert.Equal(2121+1+2+3, (int) Send(oObject, #TestMe,1,2,3))
			Assert.Equal(4242+1+2+3, (int) Send(oObject, #TestMe2,1,2,3))
			Assert.Equal(6363+1+2+3, (int) Send(oObject, #TestMe3,1,2,3))
			Assert.Equal(6363+1+2+3, (int) __InternalSend(oObject, #TestMe3,1,2,3))
			
		[Fact, Trait("Category", "OOP")];
		method NoMethodTests() as void
			local oObject as object
			oObject := CreateInstance("ClassWithNoMethod")
			assert.Equal(9, (int) Send(oObject, "ADD",2,3,4))
			assert.Equal(24, (int) Send(oObject, "MUL",2,3,4))
			assert.Equal("DIV", (STRING) Send(oObject, "DIV",2,3,4))
		end class
		
	end namespace

class Tester inherit father
	property name as string auto
	property age as int auto
constructor clipper
	method TestMe(a as int,b as int,c as int) as long
		return 2121+a+b+c
	method TestMe2(a,b, c ) as long
		return 4242+a+b+c
	method TestMe3(a as usual,b as usual, c as usual) as long
		return 6363+a+b+c
		end class
	
class Father
end class


class ClassWithNoMethod
	method NoMethod(cMethodName, arg1, arg2, arg3)
		if cMethodName = "ADD"
			return arg1+arg2+arg3
		elseif cMethodName = "MUL"
			return arg1*arg2*arg3
		endif
		return cMethodName
end class