#include "GlobalDefines.vh"
#using System.Collections.Generic
#using System.IO
#using FabToolsNS

BEGIN NAMESPACE Fab_VO_Entities

    CLASS FabPRGFile 	INHERIT	FabModuleBase
	// Init was done by a FileName
	PROTECT	lFile			AS LOGIC
	// Init / Last operation successful
	PROTECT	lSuccess		AS LOGIC	
	// Array with Entity Name, Prototype and Start
	PROTECT	aEnt			AS xARRAY
	// Date and Time of the File
	PROTECT dCDate			AS	OBJECT
	PROTECT cCTime			AS	STRING
	PROTECT dBDate			AS	OBJECT
	PROTECT cBTime			AS	STRING
	// StringList with source code
	PROTECT	oSource			AS	List<STRING>
	
	DESTRUCTOR()	
	    SELF:Close()
    RETURN 

    METHOD Close() AS VOID  
	    //
	    SELF:oSource:Clear()
	    //
	    SELF:lSuccess := FALSE
	    //
    RETURN

    METHOD EntityFind( cEntity AS STRING, nType AS DWORD ) AS OBJECT  
	    LOCAL nCpt		AS	DWORD
	    LOCAL oEnt		AS	FabPRGEntity
	    LOCAL lFound	AS	LOGIC
	    //
	    FOR nCpt := 1 TO SELF:EntityCount
		    oEnt := (FabPRGEntity)SELF:aEnt[ nCpt ]
		    IF ( Lower( oEnt:Name ) == Lower( cEntity ) )
			    lFound := TRUE
			    EXIT
		    ENDIF			
	    NEXT
	    IF !lFound
		    oEnt := NULL_OBJECT
	    ENDIF
	    //
    RETURN oEnt


    VIRTUAL ACCESS	EntityList AS xARRAY 	
    RETURN SELF:aEnt


    METHOD ExportModule( cFileName AS STRING ) AS LOGIC  
	    LOCAL oFile	AS	StreamWriter
	    LOCAL dwMax	AS	LONG
	    LOCAL nCpt	AS	LONG
	    LOCAL lSuccess	AS	LOGIC
	    //
	    IF(!Empty(cFileName))
		    //
		    oFile := StreamWriter{ cFileName }
		    //
		    dwMax := SELF:oSource:Count
		    FOR nCpt := 0 TO (dwMax-1)
			    //
			    oFile:WriteLine( SELF:oSource:Item[ nCpt - 1] )
		    NEXT
		    //
		    oFile:Close()
		    lSuccess := TRUE
	    ENDIF
    RETURN lSuccess

    METHOD ExportSource( nStartLine AS LONG, nEndLine AS LONG ) AS STRING
	    LOCAL nCpt		AS	LONG
	    LOCAL cSource	AS	STRING
	    LOCAL dwMax     AS  LONG
	    //
	    dwMax := SELF:oSource:Count-1
	    nEndLine := Max( nEndLine, dwMax )
	    FOR nCpt := nStartLine TO nEndLine
		    //
		    cSource := cSource + SELF:oSource:Item[ nCpt ] + CRLF
	    NEXT
	    //
    RETURN cSource


    ACCESS FileName	AS STRING  
    RETURN SELF:cName


    ACCESS FullPath AS STRING  
    RETURN SELF:cFilePath

    CONSTRUCTOR( cFile AS STRING  ) 
	    SUPER()

	    LOCAL oFile	AS	StreamReader
	    LOCAL cAll  AS  STRING
	    LOCAL oFI   AS  FileInfo
	    LOCAL dt    AS  DateTime
//	    LOCAL dCreate AS OBJECT
	    LOCAL cCreate AS STRING
    //	LOCAL oError	AS	FabError
	    //
	    //
	    SELF:oSource := List<STRING>{ }
	    // It's a string, and the corresponding file exist.
	    IF File( cFile )
		    //
		    oFile := StreamReader{ cFile }
		    cAll := oFile:ReadToEnd()
		    oFile:Close()
		    //
		    oFI := FileInfo{ cFile }
		    dt := oFI:LastAccessTime
		    //
//		    dCreate := SToD( dt:Year:ToString() + dt:Month:ToString() + dt:Day:ToString() )
		    cCreate := dt:Hour:ToString() + ":" + dt:Minute:ToString() + ":" + dt:Second:ToString()
		    //
		    SELF:Init( cFile, cAll, NULL, cCreate, NULL, cCreate )
	    ENDIF
	    //
    RETURN 

    CONSTRUCTOR( cFile AS STRING, cSource AS STRING ) 
	//
	SUPER()
    SELF:Init( cFile, cSource, NULL, NULL, NULL, NULL )

    CONSTRUCTOR( cFile AS STRING, cSource AS STRING, dCreate AS OBJECT,cCreate AS STRING,dBuild AS OBJECT,cBuild AS STRING ) 
	//
	SUPER()
    SELF:Init( cFile, cSource, dCreate, cCreate, dBuild, cBuild )

    PROTECT METHOD Init( cFile AS STRING, cSource AS STRING, dCreate AS OBJECT,cCreate AS STRING,dBuild AS OBJECT,cBuild AS STRING ) AS VOID
	    LOCAL aTemp  AS STRING[]
	    LOCAL aDelim AS STRING[]
    //	LOCAL oError	AS	FabError
	    //
	    aDelim := <STRING>{ CRLF }
	    aTemp := cSource:Split( aDelim, StringSplitOptions.None )
	    SELF:oSource := List<STRING>{ }
	    SELF:oSource:AddRange( aTemp )
	    SELF:dCDate := dCreate
	    SELF:cCTime := cCreate
	    SELF:dBDate := dBuild
	    SELF:cBTime := cBuild
	    //
	    SELF:cName := FabTools.ExtractFileName( cFile )
	    SELF:cFilePath := cFile
	    //
	    SELF:aEnt   := xARRAY{}
	    //
	    SELF:lSuccess := TRUE
	    // Fill Arrays with infos
	    /*
	    IF !IsNil( oDlg )
		    oDlg:oDCWaitText:TEXTValue := "Count and Search Entities"
		    UpdateWindow( oDlg:Handle() )
		    FabAllButPaint( oDlg:Handle() )
	    ENDIF
	    */
	    SELF:Scan()
	    /*
	    IF !IsNil( oDlg )
		    oDlg:oDCWaitText:TEXTValue := "Sort by Name"
		    UpdateWindow( oDlg:Handle() )
		    FabAllButPaint( oDlg:Handle() )
	    ENDIF
	    */
	    SELF:SortByName()
    RETURN 

    ACCESS IsMef AS LOGIC 	
    RETURN FALSE


    ACCESS IsPrg AS LOGIC 	
    RETURN TRUE

    PROTECT METHOD Scan() AS VOID  
	    LOCAL nCpt		AS	LONG
	    LOCAL nCpt2		AS	LONG
	    LOCAL dwMax		AS	LONG
	    LOCAL cKeyWord	AS	STRING
	    LOCAL nKeySize	AS	DWORD
	    LOCAL cTemp		AS	STRING
    //	LOCAL cName		AS	STRING
	    LOCAL cProto	AS	STRING
	    LOCAL cLine		AS	STRING
	    LOCAL lContinue	AS	LOGIC
	    LOCAL nStart	AS	LONG
	    LOCAL nEnd		AS	LONG
    //	LOCAL nPosName	AS	DWORD
	    LOCAL lInComment	AS	LOGIC
	    LOCAL aVisi		AS	xARRAY
	    LOCAL aKeyw		AS	xARRAY
	    //
	    aKeyw := FabEntityBase.GetKeywords()
	    dwMax := SELF:oSource:Count
	    aVisi := xARRAY{ "HIDDEN", "PROTECT", "STATIC", "_DLL" }
	    //
	    FOR nCpt := 0 TO dwMax-1
		    // Get the line
		    cLine := SELF:oSource:Item[ nCpt ]
		    // Remove all tab chars
		    cLine := StrTran( cLine, Chr(09), " " )
		    // and add spaces for brackets
		    cLine := StrTran( cLine, "(", " ( " )
		    cLine := StrTran( cLine, ")", " ) " )
		    // Now remove leading and trailing spaces
		    cLine := AllTrim( cLine  )
		    IF Empty( cLine )
			    LOOP
		    ENDIF
		    // Watch out for Comment Markers !!!!!
		    IF (!lInComment .AND. Instr( "/*", cLine )) .OR. ;
			    ( lInComment .AND. Instr( "*/", cLine ))
			    //
			    SELF:ExpurgateLine( cLine, lInComment )
			    //
			    IF Empty( cLine )
				    LOOP
			    ENDIF
		    ENDIF
		    IF lInComment
			    LOOP
		    ENDIF
		    // Now, we have remove any comments, spaces, ...
		    // so we have the "pure" line
		    // The type info can be found in the prototype
		    cKeyword := Upper( FabTools.GetToken( cLine, 1 ) )
		    nKeySize := SLen( cKeyword )
		    IF 	( nKeySize >= 4 ) .AND. ( AScan( aVisi, cKeyword, nKeySize ) != 0 )
			    // We need to get the next token
			    cKeyword := Upper( FabTools.GetToken( cLine, 2 ) )
			    nKeySize := SLen( cKeyword )
		    ENDIF
		    // Remember that VO accept Keyword with 4 chars... FUNC == FUNCTION, PROC == PROCEDURE, ...
		    IF ( nKeySize >= 4 ) .AND. ( AScan( aKeyw, cKeyword, nKeySize ) != 0 )
			    // Known Keyword, so Start of an entity
			    IF ( nStart == 0 )
				    // First in the PRG, so no End
				    // Mark the Start
				    nStart := nCpt
			    ELSE
				    // The End is one line before the end of the StringList
				    nEnd := nCpt - 1
				    // Now search for the prototype between the First and the Last Line
				    cProto := ""
				    FOR nCpt2 := nStart TO nEnd
					    cTemp := SELF:oSource:Item[ nCpt2 ]
					    cTemp := StrTran( cTemp, Chr(09), " " )
					    cTemp := StrTran( cTemp, "(", " ( " )
					    cTemp := StrTran( cTemp, ")", " ) " )
					    cTemp := AllTrim( cTemp  )
					    IF InStr( "//", cTemp )
						    cTemp := SubStr( cTemp, 1, At( "//", cTemp ) - 1 )
						    cTemp := AllTrim( cTemp )
					    ENDIF
					    IF ( Right( cTemp, 1 ) == ";" )
						    cProto := cProto + SubStr( cTemp, 1, SLen( cTemp ) - 1 ) + " "
						    lContinue := TRUE
						    LOOP
					    ENDIF
					    //
					    cProto := cProto + cTemp
					    EXIT
				    NEXT
				    // Create a FabPRGEntity
				    AAdd( SELF:aEnt, FabPRGEntity{ "",  cProto, SELF, nStart, nEnd, ;
									    SELF:dCDate, SELF:cCTime, SELF:dBDate, SELF:cBTime } )
				    // And set the new Start
				    nStart := nCpt
			    ENDIF
		    ENDIF
		    //
		    IF !lContinue
			    cLine := ""
		    ENDIF
		    //
		    cTemp := SELF:oSource:Item[ nCpt ]
		    cTemp := StrTran( cTemp, Chr(09), " " )
		    cTemp := AllTrim( cTemp )
		    IF ( Right( cTemp, 1 ) == ";" )
			    cLine := cLine + SubStr( cTemp, 1, SLen( cTemp ) - 1 ) + " "
			    lContinue := TRUE
			    LOOP
		    ENDIF
		    //
		    IF !lContinue
			    cLine := cLine + cTemp
		    ENDIF
		    lContinue := FALSE
		    // Keyword		
	    NEXT
	    // if we have a Start an no End, we must use the end of File as End Marker
	    IF ( nStart != 0 ) .AND. ( nEnd < nStart )
		    // Mark the End
		    nEnd := dwMax
		    // Now search for the prototype between the First and the Last Line
		    cProto := ""
		    FOR nCpt2 := nStart TO nEnd
			    cTemp := SELF:oSource:Item[ nCpt2 ]
			    cTemp := StrTran( cTemp, Chr(09), " " )
			    cTemp := StrTran( cTemp, "(", " ( " )
			    cTemp := StrTran( cTemp, ")", " ) " )
			    cTemp := AllTrim( cTemp  )
			    IF InStr( "//", cTemp )
				    cTemp := SubStr( cTemp, 1, At( "//", cTemp ) - 1 )
				    cTemp := AllTrim( cTemp )
			    ENDIF
			    IF ( Right( cTemp, 1 ) == ";" )
				    cProto := cProto + SubStr( cTemp, 1, SLen( cTemp ) - 1 ) + " "
				    lContinue := TRUE
				    LOOP
			    ENDIF
			    //
			    cProto := cProto + cTemp
			    EXIT
		    NEXT
		    // Create a FabPRGEntity
		    AAdd( SELF:aEnt, FabPRGEntity{ "",  cProto, SELF, nStart, nEnd, ;
									    SELF:dCDate, SELF:cCTime, SELF:dBDate, SELF:cBTime } )
	    ENDIF


        RETURN

    PROTECT METHOD	SortByName( ) AS VOID  
        LOCAL lOk   AS LOGIC
        LOCAL nCpt  AS LONG
        LOCAL nMax  AS LONG
        LOCAL oEnt1	AS	FabPRGEntity
        LOCAL oEnt2	AS	FabPRGEntity
	    // Sort Entities by Names
	    nMax := (LONG)SELF:EntityCount
	    IF ( nMax <= 0 )
	        RETURN
	    ENDIF
	    //
        REPEAT
            lOk := TRUE
            FOR nCpt := 1 TO (nMax-1)
                //
                oEnt1 := (FabPRGEntity)SELF:aEnt[ (DWORD)nCpt ]
                oEnt2 := (FabPRGEntity)SELF:aEnt[ (DWORD)nCpt + 1 ]
                IF ( oEnt1:Name < oEnt2:Name )
                    SELF:aEnt[ (DWORD)nCpt ]   := oEnt2
                    SELF:aEnt[ (DWORD)nCpt+1 ] := oEnt1
                    lOk := FALSE
                ENDIF
            NEXT
        UNTIL ( lOk )
        //
        RETURN

    ACCESS Success AS LOGIC 	
    RETURN SELF:lSuccess

END CLASS

END NAMESPACE
