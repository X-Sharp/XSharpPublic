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


BEGIN NAMESPACE XSharp.VFP.Tests

	CLASS SQLStatementTests


	[Fact, Trait("SQLStatement", "Insert")];
		METHOD TestInsert AS VOID  STRICT
			//
			LOCAL lcString AS STRING
            lcString := "INSERT INTO employees (FULLNAME,SALARY,ADDATE, ADTIME) VALUES (?vlFULLNAME,?vlSALARY,?vlADDATE, ?vlADTIME)"
            //VAR cnt := CreateStatement()
            //cnt:ParseCommand( lcString, 
//			VAR result := cnt:ParseCommand( lcString, '?' , false )
//			// SQLCommand should change
//			Assert.Equal( "INSERT INTO employees (FULLNAME,SALARY,ADDATE, ADTIME) VALUES (?,?,?, ?)", result:Item1:Trim(<char>{c'\n',c'\r'} ) )
//			// Return a Value ?
//			Assert.Equal( false, result:Item2 )
//			// Number of Parameters : 4
//			Assert.Equal( 4, result:Item3:Count )
//			Assert.Equal( "vlSALARY", result:Item3[1]:Name )
			RETURN



//    METHOD CreateStatement() AS SQLStatement
//        LOCAL sCT AS SQLStatement
//        TRY
//            sCt := SQLStatement{"", false }
//        CATCH
//        END TRY
//        RETURN sCT
//    END METHOD

	END CLASS
END NAMESPACE
