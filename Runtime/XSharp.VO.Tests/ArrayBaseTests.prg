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
        // Normal non indexed developer
        CLASS Developer 
            PROPERTY FirstName AS STRING AUTO
            PROPERTY LastName AS STRING AUTO
            CONSTRUCTOR()
            CONSTRUCTOR (cFirst AS STRING, cLast AS STRING)
            FirstName := cFirst
            LastName  := cLast
		/// <summary>Implicitely convert a typed array to an array of USUALs.</summary>
            
        END CLASS
        // Special indexed developer
        CLASS DeveloperIndexed INHERIT Developer IMPLEMENTS IIndexedProperties
            CONSTRUCTOR()
            CONSTRUCTOR (cFirst AS STRING, cLast AS STRING)
            SUPER(cFirst, cLast)
            PROPERTY SELF[index AS INT] AS USUAL
                GET
                    SWITCH index
                    CASE 1
                        RETURN FirstName
                    CASE 2
                        RETURN LastName
                    END SWITCH
                    RETURN NIL
                END GET
                SET
                    SWITCH index
                    CASE 1
                        FirstName := VALUE
                    CASE 2
                        LastName := VALUE
                    END SWITCH
                    RETURN 
                END SET
            END PROPERTY
            PROPERTY SELF[name AS STRING] AS USUAL
                GET
                    SWITCH name:ToLower()
                    CASE "firstname"
                        RETURN FirstName
                    CASE "lastname"
                        RETURN LastName
                    END SWITCH
                    RETURN NIL
                END GET
                SET
                    SWITCH name:ToLower()
                    CASE "firstname"
                        FirstName := VALUE
                    CASE "lastname"
                        LastName := VALUE
                    END SWITCH
                    RETURN 
                END SET
            END PROPERTY
        END CLASS
        METHOD BuildArray() AS ARRAY OF Developer
            LOCAL aDevs AS ARRAY OF Developer
            aDevs := {}
            AADD(aDevs, Developer{"Chris","Pyrgas"})
            AADD(aDevs, Developer{"Nikos","Kokkalis"})
            RETURN aDevs
        METHOD BuildIndexedArray() AS ARRAY OF DeveloperIndexed
            LOCAL aDevs AS ARRAY OF DeveloperIndexed
            aDevs := {}
            AADD(aDevs, DeveloperIndexed{"Chris","Pyrgas"})
            AADD(aDevs, DeveloperIndexed{"Nikos","Kokkalis"})
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
            Assert.Equal(1, (INT) AScan(aDevs, { x => X:FirstName == "Chris" .AND. x:LastName == "Pyrgas"}))
            Assert.Equal(2, (INT) AScan(aDevs, { x => X:FirstName == "Nikos" .AND. x:LastName == "Kokkalis"}))
            Assert.Equal(0, (INT) AScan(aDevs, { x => X:FirstName == "Fabrice" .AND. x:LastName == "Foray"}))
            VAR chris := aDevs[1]
            Assert.Equal(1, (INT) Ascan(adevs, chris))
            
            
        [Trait("Category", "ArrayBase")];
        [Fact]; 
        METHOD TestIndices2 AS VOID
            VAR aDevs := BuildIndexedArray()
            Assert.Equal("Chris", aDevs[1,1])
            Assert.Equal("Pyrgas", aDevs[1,2])
            Assert.Equal("Chris", aDevs[1,"firstname"])
            Assert.Equal("Pyrgas", aDevs[1,"LastName"])
            aDevs[1,1] := "CHRIS"
            Assert.Equal("CHRIS", aDevs[1,1])
            Assert.Equal("CHRIS", aDevs[1,"firstname"])

        [Trait("Category", "ArrayBase")];
        [Fact]; 
        METHOD testAscan2 AS VOID
            VAR aDevs := BuildArray()
            Assert.Equal(1, (INT) AScan(aDevs, { |x| x[1] := "Chris" }))


    END CLASS
END NAMESPACE // XSharp.Runtime.Tests
