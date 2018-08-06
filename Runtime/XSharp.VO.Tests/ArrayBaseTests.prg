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


// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.VO.Tests

	

	CLASS ArrayBaseTests
		CLASS Developer
			PROPERTY FirstName AS STRING AUTO
			PROPERTY LastName AS STRING AUTO
			CONSTRUCTOR()
			CONSTRUCTOR (cFirst AS STRING, cLast AS STRING)
				FirstName := cFirst
				LastName  := cLast

		END CLASS

		METHOD BuildArray() AS ARRAY OF Developer
			LOCAL aDevs AS ARRAY OF Developer
			aDevs := {}
			AADD(aDevs, Developer{"Chris","Pyrgas"})
			AADD(aDevs, Developer{"Nikos","Kokkalis"})
			RETURN aDevs
 		[Trait("Category", "ArrayBase")];
		[Fact]; 
		METHOD TestIndices AS VOID
			VAR aDevs := BuildArray()
			Assert.Equal( 2 , (INT) ALen(aDevs))
			Assert.Equal("Chris", aDevs[1]:FirstName)
			Assert.Equal("Pyrgas", aDevs[1]:LastName)
			Assert.Equal("Nikos", aDevs[2]:FirstName)
			Assert.Equal("Kokkalis", aDevs[2]:LastName)
			Assert.Equal("Chris", aDevs[1,"FirstName"])
			Assert.Equal("Pyrgas", aDevs[1,"LastName"])
			Assert.Equal("Nikos", aDevs[2,"FirstName"])
			Assert.Equal("Kokkalis", aDevs[2,"LastName"])
			Assert.ThrowsAny<ArgumentException>({ => aDevs[1,"First"] })
			Assert.ThrowsAny<ArgumentException>({ => aDevs[3,"FirstName"] })

 		[Trait("Category", "ArrayBase")];
		[Fact]; 
		METHOD TestSort AS VOID
			VAR aDevs := BuildArray()
			ASort(aDevs, {x, y => x:LastName <= y:LastName})
			Assert.Equal("Kokkalis", aDevs[1]:LastName)
			Assert.Equal("Pyrgas", aDevs[2]:LastName)
			ASort(aDevs, {x, y => x:FirstName <= y:FirstName})
			Assert.Equal("Chris", aDevs[1]:FirstName)
			Assert.Equal("Nikos", aDevs[2]:FirstName)

		[Trait("Category", "ArrayBase")];
		[Fact]; 
		METHOD TestEval AS VOID
			VAR aDevs := BuildArray()
			LOCAL result AS STRING
			result := ""
			Aeval(aDevs, { x => result += x:FirstName})
			Assert.Equal("ChrisNikos", result)


		[Trait("Category", "ArrayBase")];
		[Fact]; 
		METHOD TestScan AS VOID
			VAR aDevs := BuildArray()
			Assert.Equal(1, (INT) AScan(aDevs, { x => X:FirstName == "Chris" .and. x:LastName == "Pyrgas"}))
			Assert.Equal(2, (INT) AScan(aDevs, { x => X:FirstName == "Nikos" .and. x:LastName == "Kokkalis"}))
			Assert.Equal(0, (INT) AScan(aDevs, { x => X:FirstName == "Fabrice" .and. x:LastName == "Foray"}))
			VAR chris := aDevs[1]
			Assert.Equal(1, (INT) Ascan(adevs, chris))

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests