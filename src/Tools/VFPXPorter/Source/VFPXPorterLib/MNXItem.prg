USING System
USING System.Collections.Generic
USING System.Text
USING System.Xml.Serialization

BEGIN NAMESPACE VFPXPorterLib
	
	ENUM MenuObjType
		// Init code for the Menu : Contains procedure, setup and cleanup
		// It seems value 5 is the same ?
		MEMBER Menu := 1
		MEMBER SubMenu := 2
		MEMBER Item := 3
		MEMBER ShortCut := 4
		MEMBER SdiMenu := 5
		//
		MEMBER Info2 := 0
		
	END ENUM
	
	ENUM MenuObjCode
		MEMBER Info := 0
		MEMBER SysMenu := 1
		MEMBER Init := 22
		
		MEMBER Command := 67
		MEMBER SubMenu := 77
		MEMBER SystemBar := 78
		MEMBER Procedure  := 80
		
	END ENUM
	
	/// <summary>
	/// The MenuItem class.
	/// </summary>
	CLASS MNXItem
		
		PROPERTY Childs AS List<MNXItem> AUTO
		PROPERTY SubItemsToAdd AS INT AUTO
		PROPERTY ParentName AS STRING AUTO	
		
		CONSTRUCTOR()
			SELF:ParentName := ""
			SELF:Childs := List<MNXItem>{}		
		
		CONSTRUCTOR( lFillWithDB AS LOGIC )
			SELF:ParentName := ""
			SELF:Childs := List<MNXItem>{}
			IF lFillWithDB
				SELF:OBJTYPE := FieldGet(FieldPos("OBJTYPE"))
				SELF:OBJCODE := FieldGet(FieldPos("OBJCODE"))
				SELF:Name := FieldGet(FieldPos("NAME"))
				SELF:PROMPT := FieldGet(FieldPos("PROMPT"))
				SELF:COMMAND := FieldGet(FieldPos("COMMAND"))
				SELF:MESSAGE := FieldGet(FieldPos("MESSAGE"))
				SELF:PROCTYPE := FieldGet(FieldPos("PROCTYPE"))
				SELF:PROCEDURE := FieldGet(FieldPos("PROCEDURE"))
				SELF:SETUPTYPE := FieldGet(FieldPos("SETUPTYPE"))
				SELF:SETUP := FieldGet(FieldPos("SETUP"))
				SELF:CLEANTYPE := FieldGet(FieldPos("CLEANTYPE"))
				SELF:CLEANUP := FieldGet(FieldPos("CLEANUP"))
				SELF:MARK := FieldGet(FieldPos("MARK"))
				SELF:KEYNAME := FieldGet(FieldPos("KEYNAME"))
				SELF:KEYLABEL := FieldGet(FieldPos("KEYLABEL"))
				SELF:SKIPFOR := FieldGet(FieldPos("SKIPFOR"))
				SELF:NAMECHANGE := FieldGet(FieldPos("NAMECHANGE"))
				SELF:NUMITEMS := FieldGet(FieldPos("NUMITEMS"))
				SELF:LEVELNAME := FieldGet(FieldPos("LEVELNAME"))
				SELF:ITEMNUM := FieldGet(FieldPos("ITEMNUM"))
				SELF:COMMENT := FieldGet(FieldPos("COMMENT"))
				SELF:LOCATION := FieldGet(FieldPos("LOCATION"))
				SELF:SCHEME := FieldGet(FieldPos("SCHEME"))
				SELF:SYSRES := FieldGet(FieldPos("SYSRES"))
				SELF:ResName := FieldGet(FieldPos("RESNAME"))				
			ENDIF
		
		CONSTRUCTOR( itemToCopy AS MNXItem ) 
			// Copy all Attributes
			SELF:OBJTYPE := itemToCopy:OBJTYPE
			SELF:OBJCODE := itemToCopy:OBJCODE
			SELF:Name := itemToCopy:Name
			SELF:PROMPT := itemToCopy:PROMPT
			SELF:COMMAND := itemToCopy:COMMAND
			SELF:MESSAGE := itemToCopy:MESSAGE
			SELF:PROCTYPE := itemToCopy:PROCTYPE
			SELF:PROCEDURE := itemToCopy:PROCEDURE
			SELF:SETUPTYPE := itemToCopy:SETUPTYPE
			SELF:SETUP := itemToCopy:SETUP
			SELF:CLEANTYPE := itemToCopy:CLEANTYPE
			SELF:CLEANUP := itemToCopy:CLEANUP
			SELF:MARK := itemToCopy:MARK
			SELF:KEYNAME := itemToCopy:KEYNAME
			SELF:KEYLABEL := itemToCopy:KEYLABEL
			SELF:SKIPFOR := itemToCopy:SKIPFOR
			SELF:NAMECHANGE := itemToCopy:NAMECHANGE
			SELF:NUMITEMS := itemToCopy:NUMITEMS
			SELF:LEVELNAME := itemToCopy:LEVELNAME
			SELF:ITEMNUM := itemToCopy:ITEMNUM
			SELF:COMMENT := itemToCopy:COMMENT
			SELF:LOCATION := itemToCopy:LOCATION
			SELF:SCHEME := itemToCopy:SCHEME
			SELF:SYSRES := itemToCopy:SYSRES
			SELF:ResName := itemToCopy:ResName			
			
			// And don't forget Childs....
			SELF:Childs := List<MNXItem>{}
			FOREACH VAR child IN itemToCopy:Childs
				SELF:Childs:Add( MNXItem{ child } )
			NEXT
			SELF:ParentName := itemToCopy:ParentName
			
			RETURN
		
		
		[XmlIgnore];
		PROPERTY _Name AS STRING AUTO GET PRIVATE SET			
		
		PROPERTY Name AS STRING
			GET
				IF String.IsNullOrEmpty( SELF:_Name )
					RETURN SELF:GeneratedName
				ELSE
					RETURN SELF:_Name
				ENDIF
			END GET
			SET
				SELF:_Name := VALUE
			END SET
		END PROPERTY
		
		[XmlIgnore];
		PROPERTY GeneratedName AS STRING GET SELF:ParentName + "Item" + SELF:ITEMNUM:Trim()			
		
		
		/// <summary>
		/// Resource Name/FilePath
		/// </summary>			
		PROPERTY ResName AS STRING AUTO
		
		// OBJTYPE,N,2,0
		PROPERTY OBJTYPE AS INT AUTO
		// OBJCODE,N,2,0
		PROPERTY OBJCODE AS INT AUTO
		// NAME,M,4,0
		//PROPERTY NAME AS STRING AUTO
		// PROMPT,M,4,0
		PROPERTY PROMPT AS STRING AUTO
		// COMMAND,M,4,0
		PROPERTY COMMAND AS STRING AUTO
		// MESSAGE,M,4,0
		PROPERTY MESSAGE AS STRING AUTO
		// PROCTYPE,N,1,0
		PROPERTY PROCTYPE AS INT AUTO
		// PROCEDURE,M,4,0
		PROPERTY PROCEDURE AS STRING AUTO
		// SETUPTYPE,N,1,0
		PROPERTY SETUPTYPE AS INT AUTO
		// SETUP,M,4,0
		PROPERTY SETUP AS STRING AUTO
		// CLEANTYPE,N,1,0
		PROPERTY CLEANTYPE AS INT AUTO
		// CLEANUP,M,4,0
		PROPERTY CLEANUP AS STRING AUTO
		// MARK,C,1,0
		PROPERTY MARK AS STRING AUTO
		// KEYNAME,M,4,0
		PROPERTY KEYNAME AS STRING AUTO
		// KEYLABEL,M,4,0
		PROPERTY KEYLABEL AS STRING AUTO
		// SKIPFOR,M,4,0
		PROPERTY SKIPFOR AS STRING AUTO
		// NAMECHANGE,L,1,0
		PROPERTY NAMECHANGE AS LOGIC AUTO
		// NUMITEMS,N,2,0
		PROPERTY NUMITEMS AS INT AUTO
		// LEVELNAME,C,10,0
		PROPERTY LEVELNAME AS STRING AUTO
		// ITEMNUM,C,3,0
		PROPERTY ITEMNUM AS STRING AUTO
		// COMMENT,M,4,0
		PROPERTY COMMENT AS STRING AUTO
		// LOCATION,N,2,0
		PROPERTY LOCATION AS INT AUTO
		// SCHEME,N,2,0
		PROPERTY SCHEME AS INT AUTO
		// SYSRES,N,1,0
		PROPERTY SYSRES AS INT AUTO
		// RESNAME,M,4,0
		//PROPERTY RESNAME AS STRING AUTO		
		
		
	END CLASS
	
	
END NAMESPACE // VFPXPorterLib