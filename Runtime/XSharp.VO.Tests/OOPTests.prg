USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit



BEGIN NAMESPACE XSharp.VO.Tests



	class OOPTests
	[Fact, Trait("Category", "OOP")];
		method CreateInstanceTests() as void
		local oObject as object
		Assert.Equal(true, IsClass("tester"))
		Assert.Equal(true, IsClass(#tester))
		Assert.Equal(true, IsClassOf(#tester,#Father))
		/// note that vulcan does not return true for IsClassOf(#Tester, "Object")
		oObject := CreateInstance(#Tester)
		Assert.Equal("TESTER", Classname(oObject))
		Assert.NotEqual(null_object, oObject)
		IVarPut(oObject,"Name", "X#")
		Assert.Equal("X#", IVarGet(oObject, "Name"))
		IVarPut(oObject,"Age",42)
		Assert.Equal(42, (int) IvarGet(oObject, "Age"))
		Assert.Equal(false, IsMethod(oObject, "Doesnotexist"))
		Assert.Equal(true, IsMethod(oObject, "GetHashCode"))
		Assert.Equal(true, IsInstanceOf(oObject, #Father))
		local uValue as Usual
		uValue := oObject
		Assert.Equal(true, IsInstanceOfUsual(uValue, #Father))
		local aVars as Array
		aVars := IVarList(oObject)
		Assert.Equal(2, (int) Alen(aVars))
		local aMethods as Array
		aMethods := MethodList(oObject)
		Assert.Equal(7, (int) Alen(aMethods))		// 4 METHODS of the OBJECT CLASS + TestMe + TestMe2
		local aTree as array
		aTree := ClassTree(oObject)
		Assert.Equal(3, (int) Alen(atree))	// TESTER, FATHER and OBJECT
		aTree := ClassTreeClass(#Tester)
		Assert.Equal(3, (int) Alen(atree))	// TESTER, FATHER and OBJECT
		Assert.Equal(2121+1+2+3, (int) Send(oObject, #TestMe,1,2,3))
		Assert.Equal(4242+1+2+3, (int) Send(oObject, #TestMe2,1,2,3))
		Assert.Equal(6363+1+2+3, (int) Send(oObject, #TestMe3,1,2,3))
		

	end CLASS

end namespace

class Tester inherit father
	property name as string AUTO
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