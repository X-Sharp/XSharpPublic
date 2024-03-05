// Note: Inside Visual Studio you should open the Test - Windows - Test Explorer Window
// From there you can run the tests and see which tests succeeded and which tests failed
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using Microsoft.VisualStudio.TestTools.UnitTesting
USING VFPXPorterLib

BEGIN NAMESPACE VFPXPorterTest
	[TestClass]	;
	CLASS TestCodeConverter
		CONSTRUCTOR()  STRICT
			RETURN
			
		METHOD GetStatements() AS List<String>
			VAR stmt := List<String>{}
			stmt:Add( "Refresh" )
			stmt:Add( "Release" )
			stmt:Add( "Click" )
			RETURN stmt

		METHOD GetCode1() AS STRING
			LOCAL code AS STRING
			TEXT TO code
PROCEDURE RefreshMe
luResult=this.Refresh
			ENDTEXT
			RETURN code

		METHOD GetCode1_2() AS STRING
			LOCAL code AS STRING
			TEXT TO code
PROCEDURE GetCode1_2
luResult=this.Refresh
			ENDTEXT
			RETURN code

		METHOD GetCode2() AS STRING
			LOCAL code AS STRING
			TEXT TO code
PROCEDURE RefreshMe
luResult=this.Refresh2
			ENDTEXT
			RETURN code

		METHOD GetCode3() AS STRING
			LOCAL code AS STRING
			TEXT TO code
PROCEDURE RefreshMe
RELEASE This
			ENDTEXT
			RETURN code
					
					
		METHOD GetCode4() AS STRING
			LOCAL code AS STRING
			TEXT TO code
PROCEDURE RefreshMe
DO CASE
	CASE lcCommand=="refresh"
		luResult=this.Refresh2()
	CASE lcCommand=="releasehost"
		luResult=this.ReleaseHost()
	CASE lcCommand=="runaction"
		luResult=this.RunAction(lcParam1)
ENDCASE
RELEASE This
			ENDTEXT
			RETURN code

			
		[TestMethod];
		[TestCategory("CodeConverter tests")];
		METHOD CheckProcName AS VOID  STRICT
			VAR cv := CodeConverter{ false, false, false, true, true }
			cv:Statements := SELF:GetStatements()
			//
			cv:ProcessProcedure( SELF:GetCode1(), __FUNCTION__ )
			Assert.AreEqual( true, cv:Source:Count > 1 )
			If  cv:Source:Count > 1
				 Assert.AreEqual( "PROCEDURE RefreshMe", cv:Source[0] )
			ENDIF
			RETURN

		[TestMethod];
		[TestCategory("CodeConverter tests")];
		METHOD CheckAddParenthesis1() AS VOID  STRICT
			VAR cv := CodeConverter{ false, false, false, true, true }
			cv:Statements := SELF:GetStatements()
			//
			VAR testItemCode := TestItemCode{}
			VAR testItem := testItemCode:SCXVCXItemCreation(null,SELF:GetCode1())
			//
			Assert.AreEqual( 1, testItem:XPortedCode:Events:Count )
			//
			VAR evtCode := testItem:XPortedCode:Events[0]
			Assert.AreEqual( "RefreshMe", evtCode:Name )
			//
			cv:ProcessEvent( evtCode )
			Assert.AreEqual(1, cv:Source:Count )
			Assert.AreEqual(true, cv:Source[0]:EndsWith("()"))
			RETURN

		[TestMethod];
		[TestCategory("CodeConverter tests")];
		METHOD CheckAddParenthesis2() AS VOID  STRICT
			VAR cv := CodeConverter{ false, false, false, true, true }
			cv:Statements := SELF:GetStatements()
			//
			VAR testItemCode := TestItemCode{}
			VAR testItem := testItemCode:SCXVCXItemCreation(null,SELF:GetCode1_2())
			//
			Assert.AreEqual( 1, testItem:XPortedCode:Events:Count )
			//
			VAR evtCode := testItem:XPortedCode:Events[0]
			Assert.AreEqual( "GetCode1_2", evtCode:Name )
			//
			cv:ProcessEvent( evtCode )
			Assert.AreEqual(1, cv:Source:Count )
			Assert.AreEqual(true, cv:Source[0]:EndsWith("()"))
			RETURN

		[TestMethod];
		[TestCategory("CodeConverter tests")];
		METHOD CheckAddParenthesis3() AS VOID  STRICT
			VAR cv := CodeConverter{ false, false, false, true, true }
			cv:Statements := SELF:GetStatements()
			//
			VAR testItemCode := TestItemCode{}
			VAR testItem := testItemCode:SCXVCXItemCreation(null,SELF:GetCode3())
			//
			Assert.AreEqual( 1, testItem:XPortedCode:Events:Count )
			//
			VAR evtCode := testItem:XPortedCode:Events[0]
			Assert.AreEqual( "RefreshMe", evtCode:Name )
			//
			cv:ProcessEvent( evtCode )
			Assert.AreEqual(1, cv:Source:Count )
			Assert.AreEqual(true, cv:Source[0]:IndexOf("()") == -1 )
			RETURN
			
	END CLASS
END NAMESPACE
