#include "GlobalDefines.vh"

BEGIN NAMESPACE Fab_VO_Entities

    CLASS FabModuleBase	INHERIT	FabIDEBaseObject
        //
        PROTECT	cFilePath		AS	STRING
        //
        //
        //
        //
        CONSTRUCTOR()  
        SUPER()
        RETURN

        VIRTUAL METHOD Close() AS VOID  

        RETURN

/*        VIRTUAL ACCESS CreateDate AS DATE  
        RETURN NULL_DATE


        VIRTUAL ACCESS CreateTime AS STRING  
        RETURN "00:00:00"*/


        VIRTUAL ACCESS	EntityCount AS DWORD  
        RETURN ALen( SELF:EntityList )


        VIRTUAL METHOD EntityFind( cEntity AS STRING, nType AS DWORD ) AS OBJECT  
        RETURN NULL_OBJECT


        VIRTUAL ACCESS	EntityList AS xARRAY  
        RETURN xARRAY{}


        VIRTUAL ACCESS	EntityNameList AS xARRAY  
	        LOCAL nCpt	AS	DWORD
	        LOCAL aTemp	AS	xARRAY
	        LOCAL oEnt	AS	FabEntityBase
	        LOCAL aEnt	AS	xARRAY
	        //
	        aTemp := xARRAY{}
	        aEnt := SELF:EntityList
	        FOR nCpt := 1 TO SELF:EntityCount
		        oEnt := (FabEntityBase)aEnt[ nCpt ]
		        AAdd( aTemp, oEnt:Name )
	        NEXT
        RETURN aTemp


        VIRTUAL METHOD ExportModule( cFileName AS STRING ) AS LOGIC  
        RETURN FALSE


        VIRTUAL ACCESS ExternalFile	AS STRING  
        RETURN ""

        VIRTUAL ACCESS ExternalSource AS STRING  
        RETURN ""

        VIRTUAL ACCESS FullPath AS STRING  
        RETURN ""

        VIRTUAL ASSIGN FullPath( cNew AS STRING )   
        SELF:cFilePath := cNew

        VIRTUAL ACCESS IsExternal AS LOGIC  
        RETURN FALSE

        VIRTUAL ACCESS IsMef AS LOGIC  
        RETURN FALSE

        VIRTUAL ACCESS IsPrg AS LOGIC  
        RETURN FALSE

        ACCESS IsValid AS LOGIC  
        RETURN ( SELF:IsMef .OR. SELF:IsPrg )


/*        VIRTUAL ACCESS LastBuildDate AS DATE 	
        RETURN NULL_DATE


        VIRTUAL ACCESS LastBuildTime AS STRING  
        RETURN "00:00:00"*/


        PROTECT VIRTUAL METHOD Scan( ) AS VOID  

        RETURN

        PUBLIC VIRTUAL METHOD SortByName( ) AS VOID  

        RETURN
        
        PROTECT METHOD ExpurgateLine( cLine REF STRING, lInComment REF LOGIC ) AS VOID
            // remove from the line all chars in comments
            // 
	        LOCAL dwMax		AS	DWORD
	        LOCAL nCpt		AS	DWORD
	        LOCAL cChar		AS	STRING
	        LOCAL lInString	AS	LOGIC
	        LOCAL cPrev		AS	STRING
	        LOCAL cTemp		AS	STRING
	        //
	        dwMax := SLen( cLine )
	        //
	        cChar := SubStr( cLine, 1, 1 )
	        IF !lInComment
		        cTemp := cChar
		        IF InStr( cChar, e"\"'" )
			        lInString := TRUE
		        ENDIF
	        ENDIF
	        //
	        FOR nCpt := 2 TO dwMax
		        // Get a Char
		        cPrev := cChar
		        cChar := SubStr( cLine, nCpt, 1 )
		        IF !lInComment
			        //
			        cTemp := cTemp + cChar
			        // A String Marker ( Open/Close )
			        IF InStr( cChar, e"\"'" )
				        // We are starting a string, or closing a string
				        lInString := !lInString
				        LOOP
			        ENDIF
		        ENDIF
		        IF !lInString
			        IF ( cChar == "/" )
				        //
				        IF ( cPrev == "*" )
					        // Previous was star: End Of Comment
					        lInComment := FALSE
					        LOOP
				        ENDIF
			        ELSEIF cChar == "*"
				        // Start of Comment ?
				        IF ( cPrev == "/" )
					        lInComment := TRUE
					        // Remove the Comment marker
					        cTemp := SubStr( cTemp, 1, SLen( cTemp ) - 2 )
					        LOOP
				        ENDIF
			        ENDIF						
		        ENDIF
		        //
	        NEXT
	        //
	        cLine := cTemp
        RETURN
            
    END CLASS

END NAMESPACE
