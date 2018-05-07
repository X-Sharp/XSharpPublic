#include "GlobalDefines.vh"
#using FabToolsNS

BEGIN NAMESPACE Fab_VO_Entities

    CLASS FabEntityBase	INHERIT	FabIDEBaseObject
        
        STATIC PROTECT __aKeywords AS xARRAY
        // This one will
        // Used by PRG Entity, PRG File, MEF Entity
        //
        PROTECT cPrototype 	AS	STRING
        PROTECT cProtoTmp	AS	STRING
        //
        //
        CONSTRUCTOR()  
        SUPER()
        RETURN

/*        ACCESS Accessibility AS SYMBOL  
        //d Return the Accessibility of the entity as a symbol.
        //r The Accessibility returned can be:\line
        //r \tab #STATIC, #HIDDEN, #PROTECT\line
        //r \tab or #PUBLIC for all others.
	        LOCAL cVis AS	STRING
	        // The type info can be found in the prototype
	        cVis := Upper( FabTools.GetToken( SELF:Prototype, 1 ) )
	        IF !(  ( cVis == "STATIC" ) .or.	;
		        ( cVis == "HIDDEN" ) .or.	;
		        ( cVis == "PROTECT" ) )
		        // Visible
		        cVis := "PUBLIC"
	        ENDIF
	        //
        RETURN String2Symbol( cVis )*/

/*        VIRTUAL ACCESS CreateDate AS DATE  
        RETURN NULL_DATE


        VIRTUAL ACCESS CreateTime AS STRING  
        RETURN ""*/


        ACCESS DisplayName AS STRING 	
        RETURN SELF:Name

        ACCESS DisplayNameLong AS STRING 	
        RETURN SELF:Info

        VIRTUAL ACCESS Info AS STRING 	
        RETURN ""

        ACCESS IsVisible AS LOGIC  
        RETURN TRUE


/*        VIRTUAL ACCESS LastBuildDate AS DATE  
        RETURN NULL_DATE


        VIRTUAL ACCESS LastBuildTime AS STRING  
        RETURN ""*/


/*        ACCESS Prototype AS STRING  
        //p Return the prototype of the Entity
        //d Sometimes the keywords are shorten in the AEF/MEF file. This is due to the way you type it.
        //d As you can type FUNC instead of FUNCTION, this access will try to resolve all keywords so they appear in the complete form.
	        LOCAL cProto	AS	STRING
	        LOCAL cKey		AS	STRING
	        LOCAL nPos		AS	DWORD
	        LOCAL nCpt		AS DWORD
	        LOCAL nLen		AS	DWORD
	        LOCAL aKeyw		AS	xARRAY
	        //
	        IF Empty( SELF:cPrototype )
		        //
		        aKeyw := FabEntityBase.GetKeywords()
		        // Get the current string
		        cProto := SELF:VOPrototype
		        // For each Keyword
		        FOR nCpt := 1 TO ALen( aKeyw )
			        // Search possible versions : From 4 Chars to the Max
			        FOR nLen := 4 TO SLen( (STRING)aKeyw[ nCpt ] )
				        // Get the possible Keyword
				        cKey := SubStr( (STRING)aKeyw[ nCpt ], 1, nLen ) + " "
				        // And search for a match in the string
				        IF ( ( nPos := At( cKey, Upper( cProto ) ) ) != 0 )
					        // If in First Pos, or with a Space before
					        IF ( nPos == 1) .or. Empty( SubStr( cProto, nPos - 1,1 ) )
						        // Rebuild the String, by replacing the Keyword by it's long form
						        cProto := Stuff( cProto, nPos, SLen( cKey ), aKeyw[ nCpt ] + " " )
						        // Exit, We have found what we were search for
						        EXIT
					        ENDIF
				        ENDIF
			        NEXT
		        NEXT
		        // Search for ":=" that in DEFINE entity may not be separated by a space
		        cProto := StrTran( cProto, ":=", " :=" )
		        //
		        SELF:cPrototype := cProto
		        //
	        ENDIF
	        //
        RETURN SELF:cPrototype
*/

        VIRTUAL ACCESS Source AS STRING  
        RETURN ""


        VIRTUAL ACCESS TypeSymbol AS STRING
        RETURN NULL


        ACCESS VOPrototype AS STRING  
        RETURN SELF:cProtoTmp
        
        VIRTUAL ACCESS ClassName AS STRING
        RETURN ""

        VIRTUAL ACCESS NameSpace AS STRING
        RETURN ""
        
        STATIC METHOD GetKeywords() AS xARRAY
            IF (FabEntityBase.__aKeywords == NULL )
                FabEntityBase.InitEntityMarkers()
            ENDIF
//        RETURN AClone( FabEntityBase.__aKeywords )
        RETURN FabEntityBase.__aKeywords

        PROTECT STATIC METHOD InitEntityMarkers() AS VOID
            //
/*            FabEntityBase.__aKeywords := xARRAY{ 	"CLASS", "METHOD", "ACCESS", 
            "ASSIGN", "INHERIT", ;
					            "FUNCTION", "PROCEDURE", "RESOURCE", ;
					            "STATIC", "DEFINE", "HIDDEN", "PROTECT", 
					            "PROTECTED", "PRIVATE", "PUBLIC", "EXPORT", ;
					            "UNION", "STRUCTURE", "TEXTBLOCK", "GLOBAL" }*/
FabEntityBase.__aKeywords := xARRAY{}
FabEntityBase.__aKeywords:aAdd("CLASS")
FabEntityBase.__aKeywords:aAdd("METHOD")
FabEntityBase.__aKeywords:aAdd("ACCESS")
FabEntityBase.__aKeywords:aAdd("ASSIGN")
FabEntityBase.__aKeywords:aAdd("INHERIT")
FabEntityBase.__aKeywords:aAdd("FUNCTION")
FabEntityBase.__aKeywords:aAdd("PROCEDURE")
FabEntityBase.__aKeywords:aAdd("RESOURCE")
FabEntityBase.__aKeywords:aAdd("STATIC")
FabEntityBase.__aKeywords:aAdd("DEFINE")
FabEntityBase.__aKeywords:aAdd("HIDDEN")
FabEntityBase.__aKeywords:aAdd("PROTECT")
FabEntityBase.__aKeywords:aAdd("PROTECTED")
FabEntityBase.__aKeywords:aAdd("PRIVATE")
FabEntityBase.__aKeywords:aAdd("PUBLIC")
FabEntityBase.__aKeywords:aAdd("EXPORT")
FabEntityBase.__aKeywords:aAdd("UNION")
FabEntityBase.__aKeywords:aAdd("STRUCTURE")
FabEntityBase.__aKeywords:aAdd("TEXTBLOCK")
FabEntityBase.__aKeywords:aAdd("GLOBAL")

        RETURN

    END CLASS
    
END NAMESPACE


