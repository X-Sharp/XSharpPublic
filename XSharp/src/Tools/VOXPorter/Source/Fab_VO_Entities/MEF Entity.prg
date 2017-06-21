#include "GlobalDefines.vh"
#using System.IO
#using FabToolsNS

BEGIN NAMESPACE Fab_VO_Entities

CLASS	FabMEFEntity	INHERIT	FabEntityBase
//p Provide some info on a particular MEF Entity.
//l Class to keep track of a particular MEF Entity.
//s
	// Pointer to source code
	PROTECT	PosSource	    AS  LONG
	PROTECT lSize           AS  LONG
	PROTECT oMS             AS  MemoryStream
	
	// Thanks to Paul Piko for these
	PROTECT	dwLastBuild		AS	DWORD
	PROTECT dwCreateTime	AS	DWORD	


/*    ACCESS CreateDate AS DATE  
	    LOCAL dDate	AS	DATE
	    //
	    dDate := SToD( "19700101" )
	    ddate := ddate + Integer( SELF:dwCreateTime / 86400 )
    RETURN dDate


    ACCESS CreateTime AS STRING  
	    LOCAL cTime	AS	STRING
	    //
	    cTime := FabTools.TString( SELF:dwCreateTime % 86400 )
    RETURN cTime*/


    CONSTRUCTOR( oInfoEnt AS FabEntInfo )	
    //p Constructor of a FabMEFEntity object.
    //a cName is the name in the form it appears in the AEF/MEF File\line
    //a cProto is the prototype as it appears in the AEF/MEF File\line
    //a ptrSrc is a pointer to a PSZ memory where the source is stored
	    //
	    SUPER()
	    //
	    //Default( @dwLastBld, 0 )
	    //Default( @dwCrTime, 0 )
	    //
	    SELF:cName := AllTrim( StrTran( oInfoEnt:Name, Chr(09), " "  )  )
	    //
	    SELF:cProtoTmp := StrTran( oInfoEnt:Proto, Chr(09), " " )
	    SELF:cProtoTmp := StrTran( SELF:cProtoTmp, "(", " ( " )
	    SELF:cProtoTmp := StrTran( SELF:cProtoTmp, ")", " ) " )
	    SELF:cProtoTmp := AllTrim( SELF:cProtoTmp  )
	    //
	    SELF:dwLastBuild := FabTools.GMTUnixTimeToLocalUnixTime( oInfoEnt:LastBuild )
	    SELF:dwCreateTime := FabTools.GMTUnixTimeToLocalUnixTime( oInfoEnt:CreateTime )
	    //
	    SELF:PosSource := oInfoEnt:Pos
	    SELF:oMS := oInfoEnt:MemStream
	    SELF:lSize := oInfoEnt:Size
	    //
    RETURN 

/*    ACCESS LastBuildDate AS DATE 	
	    LOCAL dDate	AS	DATE
	    //
	    dDate := SToD( "19700101" ) 
	    ddate := ddate + Integer( SELF:dwlastbuild / 86400 )
    RETURN dDate


    ACCESS LastBuildTime AS STRING 	
	    LOCAL cTime	AS	STRING
	    //	
	    cTime := FabTools.TString( SELF:dwlastbuild % 86400 )
    RETURN cTime*/


    ACCESS Source	AS STRING 	
    //p Return the Source code associated with this entity.
	    LOCAL cSrc	AS	STRING
	    //
	    IF ( SELF:lSize != 0 )    
	        SELF:oMS:Position := SELF:PosSource
		    cSrc := FabTools.ReadPszString( SELF:oMS, SELF:lSize )
	    ENDIF
    RETURN cSrc


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
        RETURN cEnt
*/

END CLASS

END NAMESPACE
