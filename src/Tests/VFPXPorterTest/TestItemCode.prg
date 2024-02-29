// TestItemCode.prg
// Created by    : fabri
// Creation Date : 10/19/2022 4:05:45 PM
// Created for   : 
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using Microsoft.VisualStudio.TestTools.UnitTesting
USING VFPXPorterLib

BEGIN NAMESPACE VFPXPorterTest

	/// <summary>
    /// The TestItemCode class.
    /// </summary>
	[TestClass];
	CLASS TestItemCode
		
		CONSTRUCTOR()
			RETURN

		METHOD GetProps() AS STRING
			LOCAL props AS STRING
			TEXT TO props
Top = 12
Left = 81
Height = 36
Width = 36
Picture = ..\img\suiv.bmp
Caption = ""
Name = "cmd_suiv"
			ENDTEXT
			RETURN props

		METHOD GetMethods() AS STRING
			LOCAL code AS STRING
			TEXT TO code
PROCEDURE Click
SELECT (ThisForm.DataEnvironment.InitialSelectedAlias)
IF !EOF()
	SKIP 1
ENDIF
IF EOF()
	GO BOTTOM
ENDIF
ThisForm.Refresh
RELEASE obj
ENDPROC
			ENDTEXT
			RETURN code

		
		[TestMethod];
		[TestCategory("ItemCode tests")];
		METHOD ItemCodeCreation( ) AS void STRICT
			VAR item := SELF:SCXVCXItemCreation( NULL, NULL )
			//
			Assert.AreEqual( 1, item:XPortedCode:Events:Count )

		[TestMethod];
		[TestCategory("EventCode tests")];
		METHOD EventCodeCreation( ) AS void STRICT
			VAR item := SELF:SCXVCXItemCreation( NULL, NULL )
			//
			Assert.AreEqual( 1, item:XPortedCode:Events:Count )
			//
			VAR evtCode := item:XPortedCode:Events[0]
			Assert.AreEqual( "Click", evtCode:Name )
			Assert.AreEqual( false, evtCode:IsChild )
			Assert.AreEqual( 10, evtCode:Source:Count )

		METHOD SCXVCXItemCreation( props AS STRING, code AS STRING) AS SCXVCXItem
			LOCAL item AS SCXVCXItem
			//
			if props == NULL
				props := SELF:GetProps()
			ENDIF
			if code == NULL
				code := SELF:GetMethods()
			ENDIF
			//
			item := SCXVCXItem{}
			item:PLATFORM := "WINDOWS"
			item:UNIQUEID := "_5G9141QMH"
			item:CLASSName := "commandbutton"
			item:BASECLASS := "commandbutton"
			item:OBJNAME := "cmd_suiv"
			item:PARENT := ""
			item:PROPERTIES := props
			item:PROTECTED := ""
			item:METHODS := code
			//
			item:XPortedCode := ItemCode{ item, false }
			RETURN item

			

	END CLASS
END NAMESPACE // VFPXPorterTest