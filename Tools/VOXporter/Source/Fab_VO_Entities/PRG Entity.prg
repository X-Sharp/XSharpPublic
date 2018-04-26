#include "GlobalDefines.vh"

#using FabToolsNS

BEGIN NAMESPACE Fab_VO_Entities

CLASS	FabPRGEntity	INHERIT FabEntityBase
//p Provide some info on a particular MEF Entity.
//l Class to keep track of a particular MEF Entity.
//s
	// PRGFile Object where Source resides.
	PROTECT	oOwner		AS	FabPRGFile
	// Start and End lines in the Owner
	PROTECT nStartLine	AS	LONG
	PROTECT nEndLine	AS	LONG
	// Date and Time of the Entity ( Same as File )
	PROTECT dCDate		AS	OBJECT
	PROTECT cCTime		AS	STRING
	PROTECT dBDate		AS	OBJECT
	PROTECT cBTime		AS	STRING


/*    ACCESS CreateDate AS DATE  
    RETURN SELF:dCDate


    ACCESS CreateTime AS STRING  
    RETURN SELF:cCTime*/


    CONSTRUCTOR( cName AS STRING, cProto AS STRING, oPrgFile AS FabPrgFile, nStart AS LONG, nEnd AS LONG, dDate1 AS OBJECT, cTime1 AS STRING, dDate2 AS OBJECT, cTime2 AS STRING )	
    //p Constructor of a FabPRGEntity object.
    //a cName is the name in the form it appears in the AEF/MEF File\line
    //a cProto is the prototype as it appears in the AEF/MEF File\line
    //a ptrSrc is a pointer to a PSZ memory where the source is stored
	    //
	    SUPER()
	    //
	    SELF:cName := AllTrim( StrTran( cName, Chr09, " "  )  )
	    //
	    SELF:cProtoTmp := StrTran( cProto, Chr09, " " )
	    SELF:cProtoTmp := StrTran( SELF:cProtoTmp, "(", " ( " )
	    SELF:cProtoTmp := StrTran( SELF:cProtoTmp, ")", " ) " )
	    SELF:cProtoTmp := AllTrim( SELF:cProtoTmp  )
	    //
	    SELF:oOwner := oPrgFile
	    SELF:nStartLine := nStart
	    SELF:nEndLine := nEnd
	    // Date and Time of the PRG File
	    SELF:dCDate := dDate1
	    SELF:cCTime := cTime1
	    SELF:dBDate := dDate2
	    SELF:cBTime := cTime2
	    //
	    SELF:cName := SELF:Info

    RETURN 

/*    ACCESS LastBuildDate AS DATE 	
    RETURN SELF:dBDate


    ACCESS LastBuildTime AS STRING  	
    RETURN SELF:cBTime*/


    ACCESS Source AS STRING  
    //p Return the Source code associated with this entity.
    RETURN SELF:oOwner:ExportSource( SELF:nStartLine, SELF:nEndLine )


/*    ACCESS TypeSymbol AS STRING
    //p Return the type of the entity as a symbol.
    //r The type identify the entity as :\line
    //r \tab #METHOD, #CLASS, #ACCESS, #ASSIGN, #FUNCTION, ....
	    LOCAL cKeyword	AS	STRING
	    // The type info can be found in the prototype
	    cKeyword := Upper( FabTools.GetToken( SELF:Prototype, 1 ) )
	    IF 	( cKeyword == "STATIC" ) .or.	;
		    ( cKeyword == "HIDDEN" ) .or.	;
		    ( cKeyword == "PROTECT" ) .or.	;
		    ( cKeyword == "_DLL" )
		    // We need to get the next token
		    cKeyword := Upper( FabTools.GetToken( SELF:Prototype, 2 ) )
	    ENDIF
	    //
    RETURN cKeyword
*/
/*        VIRTUAL ACCESS Info AS STRING 	
	        LOCAL cEnt		AS	STRING
	        LOCAL cTemp		AS	STRING
	        LOCAL cProto	AS	STRING
	        //
	        cProto := StrTran( SELF:Prototype, "(", " " )
	        cProto := StrTran( cProto, ")", " " )
	        cEnt := Upper( FabTools.GetToken( cProto, 1 ) )
	        //
	        IF 	( cEnt == "STATIC" ) .or.	;
		        ( cEnt == "HIDDEN" ) .or.	;
		        ( cEnt == "PROTECT" ) .or.	;
		        ( cEnt == "_DLL" )
		        // We need to get the next token
		        cEnt := FabTools.GetToken( cProto, 3 )
		        IF ( Upper( cEnt ) == "DIM" )
			        // If DIM in Token #3, the entity is #4
			        cEnt := FabTools.GetToken( cProto, 4, "[ " )
		        ENDIF
	        ELSE
		        cEnt := FabTools.GetToken( cProto, 2 )
		        IF ( Upper( cEnt ) == "DIM" )
			        // If DIM in Token #2, the entity is #3
			        cEnt := FabTools.GetToken( cProto, 3, "[ " )
		        ENDIF
	        ENDIF
	        //
	        IF ( SELF:TypeSymbol == #METHOD ) .or.	;
		        ( SELF:TypeSymbol == #ACCESS ) .or.	;
		        ( SELF:TypeSymbol == #ASSIGN )
		        // Get Current "full" prototype
		        cTemp := cProto
		        // Extract data after CLASS Keyword
		        cTemp := SubStr( cTemp, At( " CLASS ", Upper(cTemp) ) + 7 )
		        // Add a Space to have a End Marker
		        cTemp := AllTrim( cTemp ) + " "
		        // After the class we may found EXPORT LOCAL, so let's search
		        cTemp := FabTools.GetToken( cTemp, 1, " ;" )
		        // And if we are looking at a _DLL definition we must extract info before ":"
		        IF ( At( ":", cTemp ) > 0 )
			        cTemp := SubStr( cTemp, 1, At( ":", cTemp )-1 )
		        ENDIF
		        //
		        cEnt := cTemp + ":" + cEnt
	        ELSEIF ( SELF:TypeSymbol == #TEXTBLOCK )
		        //
		        cTemp := cProto
		        cTemp := SubStr( cTemp, 11 )
		        cTemp := AllTrim( cTemp )
		        //
		        cEnt := cTemp
	        ENDIF
	        //
	        IF Empty( cEnt )
		        cEnt := "<Anonymous>"
	        ENDIF
	        //
        RETURN cEnt*/

END CLASS

END NAMESPACE
