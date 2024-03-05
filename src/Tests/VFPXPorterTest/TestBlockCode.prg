// TestBlockCode.prg
// Created by    : fabri
// Creation Date : 10/18/2022 6:12:47 PM
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
	/// The TestBlockCode class.
	/// </summary>
	[TestClass];
	CLASS TestBlockCode
		
		CONSTRUCTOR()
			RETURN

		METHOD GetCode1() AS STRING
			LOCAL code AS STRING
			TEXT TO code
PROCEDURE Code1( x, y )
LOCAL lcBaseClass

IF this.lRelease
	NODEFAULT
	RETURN .F.
ENDIF
this.lRelease=.T.
lcBaseClass=LOWER(this.BaseClass)
this.oHost=.NULL.
this.ReleaseObjRefs
IF NOT INLIST(lcBaseClass+" ","form ","formset ","toolbar ")
	RELEASE this
ENDIF
			ENDTEXT
			RETURN code
			
		[TestMethod];
		[TestCategory("BlockCode tests")];
		METHOD CheckBlockCodeCreation( ) AS void STRICT
			VAR codes := List<BlockCode>{}
			VAR code := SELF:BlockCodeCreation( SELF:GetCode1(), codes )
			//
			Assert.AreEqual( 1, codes:Count )
			Assert.AreEqual( "Code1", code:Name)
			Assert.AreEqual( "Code1( x, y )", code:Definition )

		METHOD BlockCodeCreation( source AS STRING, codes AS List<BlockCode> ) AS BlockCode
			// Create a List of Lines
			VAR sourceLines := ReadSource( source  )
			//
			LOCAL code := NULL AS BlockCode
			FOREACH line AS STRING IN sourceLines
				IF line:StartsWith("PROCEDURE ")
					IF code != NULL
						codes:Add( code )
					ENDIF
					code := BlockCode{ line }
				ELSE
					IF code != NULL
						// Add all Lines associated with this event
						code:Source:Add( line )
					ENDIF
				ENDIF
			NEXT
			IF code != NULL
				codes:Add( code )
			ENDIF
			//
			RETURN code



			
	END CLASS
END NAMESPACE // VFPXPorterTest