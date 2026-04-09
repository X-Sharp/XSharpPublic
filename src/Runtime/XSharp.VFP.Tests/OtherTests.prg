//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.IO
USING XUnit


// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.VFP.Tests

	CLASS OtherTests

	    STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
            RegisterFoxMemVarSupport()


		[Fact, Trait("Category", "Other")];
		METHOD IOTests() AS VOID
            // In the VO Dialect this is allowed with a non existing path
            XSharp.RuntimeState.Dialect := XSharpDialect.VO
            SetDefault(WorkDir())
            VAR cOld := SetDefault()
            VAR cNew := "C:\NonExistingFolder\"
            Assert.Equal(cOld, SetDefault(cNew))
            Assert.Equal(cNew, SetDefault())

            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
            Assert.Equal(cNew, SetDefault(cOld))
            // a not existing path should throw an error in the FoxPro dialect
            Assert.Throws<XSharp.Error>( { =>  SetDefault(cNew)})

		[Fact, Trait("Category", "Other")];
        METHOD KeyboardTests() AS VOID
/*            NumLock(TRUE)
            Assert.True(NumLock())
            NumLock(FALSE)
            Assert.False(NumLock())


            CapsLock(TRUE)
            Assert.True(CapsLock())
            CapsLock(FALSE)
            Assert.False(CapsLock())


            InsMode(TRUE)
            Assert.True(InsMode())
            InsMode(FALSE)
            Assert.False(InsMode())*/
        [Fact, Trait("Category", "Other")];
        METHOD TypeTests()  AS VOID
            var state := XSharp.RuntimeState.Dialect
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
             Assert.True(Type ( "x" ) == "U")
            XSharp.RuntimeState.Dialect := XSharpDialect.VO
             Assert.True(Type ( "x" ) == "UE")
             XSharp.RuntimeState.Dialect := state

        [Fact, Trait("Category", "Other")];
        METHOD EVLTests()  AS VOID
            Assert.True( EVL("","abc") == "abc")
            Assert.True( EVL("abc","def") == "abc")
            Assert.True( EVL("",123) == 123)
            Assert.True( EVL("abc",123) == "abc")
            Assert.True( EVL(0,123) == 123)
            Assert.True( EVL(123,456) == 123)
            Assert.True( EVL(NULL_DATE,Today()) == Today())
            Assert.True( EVL(2000.01.01,Today()) == 2000.01.01)

        [Fact, Trait("Category", "Other")];
        METHOD ReleaseTests()  AS VOID
        	PUBLIC ppp,ccc
        	ccc := "test"
        	ppp := 123
        	ppp ++
            Assert.True( ppp == 124 )
            RELEASE ALL LIKE p*
            Assert.ThrowsAny<Exception>( { => ppp ++ })

			ppp := 1
			ppp ++
            Assert.True( ppp == 2 )
            RELEASE ALL LIKE "p*"
            Assert.ThrowsAny<Exception>( { => ppp ++ })

            Assert.True( ccc == "test" )

            RELEASE ALL
            LOCAL c AS STRING
            Assert.ThrowsAny<Exception>( { => c := ccc })

            ccc := "testing"
            ppp := 100
            RELEASE ALL LIKE "c?c"
            Assert.ThrowsAny<Exception>( { => c := ccc })

            Assert.True( ppp == 100 )

            RELEASE ALL
            Assert.ThrowsAny<Exception>( { => ppp ++ })
            Assert.ThrowsAny<Exception>( { => c := ccc })


        [Fact, Trait("Category", "UIAndWindows")];
        METHOD SysMetricTests() AS VOID
            VAR nWidth := SysMetric(1)
            VAR nHeight := SysMetric(2)

            Assert.True(nWidth > 0, "ScreenWidth should be greater than 0")
            Assert.True(nHeight > 0, "ScreenHeight should be greater than 0")
        END METHOD

        [Fact];
        METHOD SetDeviceToFileTest() AS VOID
            VAR cFile := Path.Combine(Path.GetTempPath(), Guid.NewGuid():ToString() + ".txt")
            VAR cTestContent := "May the Force be with you, X#"
            TRY
                SET DEVICE TO FILE (cFile)
                ? cTestContent
            FINALLY
                SET DEVICE TO SCREEN
            END TRY

            Assert.True(File(cFile))

            VAR cContent := File.ReadAllText(cFile)
            Assert.True(cTestContent $ cContent)

            File.Delete(cFile)
        END METHOD
	END CLASS

END NAMESPACE
