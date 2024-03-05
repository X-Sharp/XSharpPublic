USING System
USING System.Collections.Generic
USING System.Text
USING System.IO

BEGIN NAMESPACE VFPXPorterLib

	/// <summary>
	/// The ItemCode class : 
	/// Contains all EventCode for the current Item.
	/// Split code to Events
	/// </summary>
	CLASS ItemCode
		// All Events attached to this Control
		PROPERTY Events AS List<EventCode> AUTO 
		// The Name of the Control
		PROPERTY Name AS STRING GET SELF:Owner:Name
		// The Parent Name
		PROPERTY Parent AS STRING GET SELF:Owner:Parent 
		// The ClassName of the Item (the Control) (After Conversion if any)
		PROPERTY ClassName AS STRING GET SELF:Owner:ClassName 
		// The ClassName of the Item (the Control) (Original one)
		PROPERTY FoxClassName AS STRING GET SELF:Owner:FoxClassName 

		PROPERTY Owner AS SCXVCXItem AUTO

		PROPERTY IsChild AS LOGIC AUTO
		
		// XmlSerialise Constructor
		CONSTRUCTOR( )
		
		
			// Copy Constructor
		CONSTRUCTOR( itemCde AS ItemCode )
			SELF:Owner := itemCde:Owner
			SELF:IsChild := itemCde:IsChild
			//
			SELF:Events := List<EventCode>{}
			FOREACH evtCode AS EventCode IN itemCde:Events
				LOCAL newCde AS EventCode
				newCde := EventCode{ evtCode }
				newCde:Owner := SELF
				SELF:Events:Add( newCde )
			NEXT
			//
			
			// Standard Constructor
		CONSTRUCTOR( own AS SCXVCXItem, isChild AS LOGIC )
			LOCAL sourceLines AS List<STRING>
			SELF:Owner := own
			SELF:IsChild := isChild
			SELF:Events := List<EventCode>{}
			// Create a List of Lines
			sourceLines := ReadSource( SELF:Owner:RawCode )
			//
			LOCAL code := NULL AS BlockCode
			FOREACH line AS STRING IN sourceLines
				IF line:StartsWith("PROCEDURE ")
					IF code != NULL
						SELF:Events:Add( EventCode{ code, SELF } )
					ENDIF
					code := BlockCode{ line }
				ELSE
					IF code != NULL
						// Add all Lines associated with this event
						code:Source:Add( line )
					ENDIF
				ENDIF
			NEXT
			//
			IF code != NULL
				SELF:Events:Add( EventCode{ code, SELF  } )
			ENDIF
			RETURN
			
	
	END CLASS
END NAMESPACE // FabVFPXPorter