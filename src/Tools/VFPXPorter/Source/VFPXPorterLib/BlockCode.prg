// BlockCode.prg
// Created by    : fabri
// Creation Date : 2/16/2021 8:19:06 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE VFPXPorterLib
	
	/// <summary>
	/// The BlockCode class : Store the Code 
	/// </summary>
	CLASS BlockCode
		
		// Name of the Handler (The FoxPro Event)
		PROPERTY Name AS STRING AUTO 
			// The FoxPro Definition/Prototype of the Handler
		PROPERTY Definition AS STRING AUTO 
			
			// The lines of source code that will handle that Event
		PROPERTY Source AS List<STRING> AUTO
			
		CONSTRUCTOR()
			RETURN
			
			// Copy Constructor
		CONSTRUCTOR( code AS BlockCode )
			SELF:Name := code:Name
			SELF:Definition := code:Definition
			SELF:Source := List<STRING>{}
			FOREACH line AS STRING IN code:Source
				SELF:Source:Add( line )
			NEXT			
			
			// Standard Constructor
		CONSTRUCTOR(  name AS STRING, def AS STRING )
			SELF:Name := name
			SELF:Definition := def
			SELF:Source := List<STRING>{}
			RETURN
			
		CONSTRUCTOR( firstLine AS STRING )
			LOCAL blockName, lne AS STRING
			blockName := "Unknown"
			lne := blockName
			IF firstLine:StartsWith("PROCEDURE ")
				// "PROCEDURE ":Length == 10
				lne := firstLine:Substring( 10 )
				lne := lne:Trim()
				// Retrieve the Name of the Procedure ( So, the Name of the Event)
				VAR words := lne:Split( <CHAR>{ '(', ' ' } )
				// Create an BlockCode with the EventName, and the Definition Line
				blockName := words[1]
			ENDIF
			SELF:Name := blockName
			SELF:Definition := lne
			SELF:Source := List<STRING>{}

				
		METHOD ToString() AS STRING
			VAR code := StringBuilder{ }
			//code:AppendLine( SELF:Definition )
			FOREACH VAR line IN Source
				code:AppendLine( line )
			NEXT
			RETURN code:ToString()
			
	END CLASS
END NAMESPACE // VFPXPorterLib