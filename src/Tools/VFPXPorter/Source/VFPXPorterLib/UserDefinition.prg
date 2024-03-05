// UserDefinition.prg
// Created by    : fabri
// Creation Date : 2/16/2021 7:59:17 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE VFPXPorterLib

	/// <summary>
	/// Store information about User defined element in a class
	/// </summary>
	CLASS UserDefinition
		
		ENUM Visibility
			MEMBER Public
			MEMBER Protected
			MEMBER Private
		END ENUM
		
		ENUM ItemKind
			MEMBER Method
			MEMBER Field
			MEMBER FieldArray
		END ENUM 
		
		PROPERTY Name AS STRING AUTO
		PROPERTY ArraySize AS STRING AUTO
		PROPERTY Modifier AS Visibility AUTO
		PROPERTY Kind AS ItemKind AUTO
		PROPERTY Help AS STRING AUTO
		
		PROPERTY Code AS BlockCode AUTO
		
		CONSTRUCTOR()
			RETURN
		
		CONSTRUCTOR( itemToCopy AS UserDefinition )
			SELF:Name := itemToCopy:Name
			SELF:ArraySize := itemToCopy:ArraySize
			SELF:Modifier := itemToCopy:Modifier
			SELF:Kind := itemToCopy:Kind
			SELF:Help := itemToCopy:Help
			IF itemToCopy:Code != NULL
				SELF:Code := BlockCode{ itemToCopy:Code }
			ENDIF
			RETURN			
		
		/// <summary>
		///	Create a UserDerfinition, based on the information found in the DataBase:Reserved3
		///
		/// fieldDef This is the Help Text
		/// *methodef This is the help text for the method
		/// ^arrayfield[x,y] This is the help text for an array Field
		/// </summary>
		CONSTRUCTOR( line AS STRING )
			VAR pos := line:IndexOf( " " )
			IF ( pos >= 0 )
				SELF:Name := line:Substring( 0, pos )
				SELF:Help := line:Substring( pos+1 )
			ELSE
				SELF:Name := line
				SELF:Help := String.Empty
			ENDIF					
			IF ( SELF:Name:StartsWith("*") )
				SELF:Name := SELF:Name:Substring(1)
				SELF:Kind := ItemKind.Method
			ELSEIF ( SELF:Name:StartsWith("^") )
				SELF:Name := SELF:Name:Substring(1)
				SELF:Kind := ItemKind.FieldArray
			ELSE
				SELF:Kind := ItemKind.Field
			ENDIF
			//
			pos := SELF:Name:IndexOf( "[" )
			IF ( pos >= 0 )
				SELF:ArraySize := SELF:Name:Substring( pos )
				SELF:Name := SELF:Name:Substring( 0, pos )
			ELSE
				SELF:ArraySize := String.Empty
			ENDIF
			//
			SELF:Modifier := Visibility.Public // Public per default (Could be Protected, Private )
			RETURN
		
		
		PROPERTY Declaration AS STRING
			GET
				LOCAL declaration AS StringBuilder
				declaration := StringBuilder{}
				//
				IF SELF:Kind == UserDefinition.ItemKind.Method .AND. ;
					!String.IsNullOrEmpty(SELF:Help)
					declaration:AppendLine("/// <summary>")
					declaration:Append("/// ")
					declaration:AppendLine( SELF:Help )
					declaration:AppendLine("/// </summary>")
				ENDIF
				declaration:Append( SELF:Modifier:ToString() )
				declaration:Append(" ")
				IF SELF:Kind == UserDefinition.ItemKind.Method
					declaration:Append("METHOD ")
				ENDIF
				IF SELF:Kind == UserDefinition.ItemKind.FieldArray
					declaration:Append("DIM ")
				ENDIF
				declaration:Append(SELF:Name)
				IF SELF:Kind == UserDefinition.ItemKind.FieldArray
					declaration:Append(SELF:ArraySize)
				ENDIF				
				IF SELF:Kind == UserDefinition.ItemKind.Method
					declaration:Append("()")
				ENDIF
				declaration:Append(" AS USUAL")
				//
				IF SELF:Kind != UserDefinition.ItemKind.Method .AND. ;
					!String.IsNullOrEmpty(SELF:Help)
					declaration:Append(" // ")
					declaration:Append(SELF:Help)
				ENDIF
				//
				RETURN declaration:ToString()
			END GET
		END PROPERTY
			
			
	END CLASS
END NAMESPACE // VFPXPorterLib