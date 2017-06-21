/*
#include "GlobalDefines.vh"
#include "VOSystemLibrary.vh"
#include "VOWin32APILibrary.vh"
*/
//#include "GlobalDefines.vh"
#using System.IO
#using FabToolsNS

BEGIN NAMESPACE Fab_VO_Entities

CLASS FabAEFFile	INHERIT		FabApplicationAbstract
    // Open the AEF file through a FileStream
    PROTECT oFS             AS FileStream
    // We will copy the file into memory
    PROTECT oMS             AS MemoryStream
	// Pointer to first RecHeader
	PROTECT	oRecHeader		AS FabRecHeader
	// Init / Last operation successful
	PROTECT	lSuccess		AS LOGIC
	// Is a Valid AEF ?
	PROTECT	lIsAef			AS LOGIC
	// Qty of Modules
	PROTECT	iMods			AS INT
	// Array with Modules Names and start
	PROTECT	aMods			AS xARRAY
	// Version of VO who creates the file
	PROTECT	cCreator		AS STRING
	//
	PROTECT	cFilePath		AS	STRING
	// Thanks to Paul Piko for these
	PROTECT dwLastBuild		AS	DWORD
	PROTECT dwCreateTime	AS	DWORD
	//
	PROTECT cDescription	AS	STRING
	PROTECT aLibs			AS	xARRAY
	PROTECT aUDCs			AS	xARRAY
	PROTECT cType			AS	STRING
	PROTECT cExeName		AS	STRING
	
	EXPORT lOptionIntegerDivisions AS LOGIC
	EXPORT lOptionOverflow AS LOGIC


    DESTRUCTOR()	
	    SELF:Close()
    RETURN 

    METHOD Close() AS VOID  
	    //
	    SELF:oMS:Close()
	    SELF:oFS:CLose()
	    //
	    SELF:lSuccess := FALSE
	    //
    RETURN 

/*    ACCESS CreateDate AS DATE 	
	    LOCAL dDate	AS	DATE
	    //
	    dDate := SToD( "19700101" )
	    ddate := ddate + Integer( SELF:dwCreateTime / 86400 )
    RETURN dDate

    ACCESS CreateTime AS STRING 	
	    LOCAL cTime	AS	STRING
	    //
	    cTime := TString( SELF:dwCreateTime % 86400 )
    RETURN cTime
*/
    ACCESS Description AS STRING  
    RETURN SELF:cDescription

    ACCESS ExeName AS STRING  
    RETURN SELF:cExeName

    METHOD ExportModule( cModName AS STRING, cFileName AS STRING ) AS LOGIC  
        // TODO ExportModule
        /*
	    LOCAL 	i 			AS INT
	    LOCAL	dw			AS DWORD
	    LOCAL	RecHeader	IS  AEFRecHeader
	    LOCAL	hfExport	AS PTR
	    LOCAL	pbData		AS BYTE PTR
	    LOCAL   lSuccess		AS LOGIC
	    //
	    IF(LOGIC(_CAST, i:=AScan(SELF:aMods, {|x|x[1]==cModName})))
		    IF(!Empty(cFileName))
			    hfExport := CreateFile( String2Psz(cFileName), DWORD(_CAST, _OR(GENERIC_READ, GENERIC_WRITE)), 0, NULL_PTR, CREATE_ALWAYS, 0, NULL_PTR)
			    // Creation OK ?
			    IF(hfExport != INVALID_HANDLE_VALUE)
				    // Add Header
				    RecHeader:uiType 	:= AEF_REC_HEADER
				    RecHeader:ulLength  := MEFFILE_VERSION_LEN+MEFFILE_TEXT_LEN
				    // Write Header
				    WriteFile(hfExport, @RecHeader, _sizeof(AEFRecHeader), @dw, NULL_PTR)
				    WriteFile(hfExport, PTR(_CAST,_CHR(0x80)+_Chr(0x02)), 2, @dw, NULL_PTR)
				    // And Text
				    WriteFile(hfExport, PTR(_CAST, MEFFILE_TEXT), MEFFILE_TEXT_LEN, @dw, NULL_PTR)
				    // Get start of Module
				    SELF:pRecHeader 	:= aMods[i, 2]
				    // Get it as a Pointer to BYTEs
				    pbData 		:= pRecHeader
				    pbData		+= _sizeof(AEFRecHeader)
				    WHILE(TRUE)
					    // Add write data
					    WriteFile(hfExport, SELF:pRecHeader, _sizeof(AEFRecHeader), @dw, NULL_PTR)
					    WriteFile(hfExport, pbData, SELF:pRecHeader:ulLength, @dw, NULL_PTR)
					    //
					    SELF:pRecHeader 	:= pbData + SELF:pRecHeader:ulLength
					    pbData 		:= SELF:pRecHeader
					    pbData		+= _sizeof(AEFRecHeader)
					    // Until End of Module or End of FabAEFFile
					    IF( SELF:pRecHeader:uiType == AEF_REC_MODNAME .or. SELF:pRecHeader:uiType == AEF_REC_END)
						    EXIT
					    ENDIF
				    ENDDO
				    // Add End Marker
				    RecHeader:uiType 	:= AEF_REC_END
				    RecHeader:ulLength  := 0
				    WriteFile(hfExport, @RecHeader, _sizeof(AEFRecHeader), @dw, NULL_PTR)
				    // Close File
				    CloseHandle(hfExport)
				    lSuccess := TRUE
			    ENDIF
		    ENDIF
 	    ENDIF
    RETURN lSuccess
    */
    RETURN FALSE

    ACCESS FullPath AS STRING  
    RETURN SELF:cFilePath

    ASSIGN FullPath( cNew AS STRING )   
    SELF:cFilePath := cNew

    CONSTRUCTOR(cFile AS STRING) 
	    SUPER()
	    
	    LOCAL wVersion	AS WORD
	    LOCAL br AS BinaryReader
	    //
	    //
	    SELF:oRecHeader := FabRecHeader{}
	    // Open the file
	    SELF:oFS := FileStream{ cFile, FileMode.Open, FileAccess.Read }
	    // Create a MemoryStream to hold the whole file
	    SELF:oMS := MemoryStream{  }
	    SELF:oMS:SetLength((INT)SELF:oFS:Length)
	    // Read file to memory
	    SELF:oFS:Read( SELF:oMS:GetBuffer(), 0, (INT)SELF:oFS:Length )
	    //Start of FabAEFFile record
	    SELF:FillRecHeader()
	    // Check if we have a FabAEFFile or a MEF
	    IF	( SELF:oRecHeader:uiType != FabVODefinitions.REC_HEADER ) //.or. ;
		    //( SELF:oRecHeader:ulLength != FabAEFFileFILE_VERSION_LEN + FabAEFFileFILE_TEXT_LEN )
		    // It's not an FabAEFFile...
		    SELF:Close()
		    RETURN
	    ENDIF
	    // Get Version Info
	    SELF:oMS:Position := 0
	    SELF:oMS:Position += FabRecHeader.Size
	    //
	    br := BinaryReader{ SELF:oMS }
	    wVersion := br:ReadUInt16()
	    // Back to Original Pos
	    SELF:oMS:Position := 0
	    SELF:oMS:Position += FabRecHeader.Size
	    //
	    // Unknown Version type !!
	    IF(  _And(wVersion, 0x0080) == 0 )
		    SELF:Close()
    /*		oError := FabError{}
		    oError:Description := "Invalid AEF File version"
		    oError:Raise()*/
		    SELF:cError := "Invalid AEF File version"
		    SELF:dwError := 1
		    RETURN
	    ENDIF
	    //
	    wVersion := (WORD)_And( (DWORD)wVersion, _not(0x0080) )
	    //
	    IF(wVersion = 256)
		    //
		    SELF:cCreator := "CA-Visual Objects 1.0"
	    ELSEIF(wVersion = 512)
		    //
		    SELF:cCreator := "CA-Visual Objects 2.0"
	    ELSE
		    SELF:Close()
    /*		oError := FabError{}
		    oError:Description := "Invalid AEF File version"
		    oError:Raise() */
		    SELF:cError := "Invalid AEF File version"
		    SELF:dwError := 1
		    RETURN
	    ENDIF
	    //
	    SELF:aMods   := xARRAY{}
	    SELF:aLibs := xARRAY{}
	    SELF:aUDCs := xARRAY{}
	    //
	    SELF:lSuccess := TRUE
	    SELF:lIsAEF := TRUE
	    // Fill Arrays with infos
	    SELF:Scan()
	    SELF:cFilePath := cFile
	    //
    RETURN 

    ACCESS IsDLL AS LOGIC  
    RETURN ( SELF:cType == "D" )


    ACCESS IsLibrary AS LOGIC  
    RETURN ( SELF:cType == "L" )

    ACCESS IsConsole AS LOGIC  
    RETURN ( SELF:cType == "C" )


    ACCESS	IsValid AS LOGIC 	
    RETURN	SELF:lIsAef


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
    RETURN cTime
*/
    ACCESS LibraryNameListList AS System.Collections.Generic.List<STRING>
    	LOCAL oList AS System.Collections.Generic.List<STRING>
    	LOCAL aList AS xARRAY
    	oList := System.Collections.Generic.List<STRING>{}
    	aList := SELF:LibraryNameList
    	FOR LOCAL n := 1 AS DWORD UPTO ALen(aList)
    		oList:Add((STRING)aList[n])
    	NEXT
    RETURN oList

    ACCESS LibraryNameList AS xARRAY  
    RETURN SELF:aLibs


    ACCESS	ModuleCount AS DWORD 	
    RETURN ALen( SELF:aMods )


/*    METHOD ModuleFind( cModName AS STRING ) AS OBJECT 	
	    LOCAL 	i 			AS DWORD
	    LOCAL	PosData		AS LONG
	    LOCAL	oMEF		AS FabMEFFile
	    //
	    i := AScan(SELF:aMods, {|x|x[1]==cModName})
	    //
	    IF (i > 0)
		    // Get start of Module
		    PosData 	:= aMods[i, 2]
		    // Build a MEF Object
		    oMEF := FabMEFFile{ SELF:oMS , PosData }
		    //
 	    ENDIF
 	    //
     RETURN oMEF
*/

    ACCESS ModuleListList AS System.Collections.Generic.List<FabMEFFile>
    	LOCAL oList AS System.Collections.Generic.List<FabMEFFile>
    	LOCAL aList AS xARRAY
    	oList := System.Collections.Generic.List<FabMEFFile>{}
    	aList := SELF:ModuleList
    	FOR LOCAL n := 1 AS DWORD UPTO ALen(aList)
    		oList:Add((FabMEFFile)aList[n])
    	NEXT
    RETURN oList

    ACCESS	ModuleList AS xARRAY 	
	    LOCAL 	nCpt	AS	DWORD
	    LOCAL 	nMax	AS	DWORD
	    LOCAL 	aTemp	AS	xARRAY
	    LOCAL	PosData	AS  LONG
	    LOCAL	oMEF	AS FabMEFFile
	    //
	    nMax := ALen( SELF:aMods )
	    aTemp := xARRAY{}
	    //
	    FOR nCpt := 1 TO nMax
		    // Get start of Module
		    PosData 	:= (INT)aMods[ nCpt, 2 ]
		    // Build a MEF Object
		    oMEF := FabMEFFile{ SELF:oMS, PosData }
		    //
		    AAdd( aTemp, oMEF )
	    NEXT
	    //
    RETURN aTemp


/*    ACCESS	ModuleNameList AS ARRAY 	
	    LOCAL aTemp	AS	ARRAY
	    //
	    aTemp := {}
	    AEval( SELF:aMods, { |aData| AAdd( aTemp, aData[ 1 ] ) } )
	    //
    RETURN aTemp
*/
    ACCESS Name	AS STRING  
    RETURN SELF:cName

    PROTECT METHOD Scan() AS VOID 	
	    LOCAL aTemp		AS xARRAY
	    LOCAL PosData   AS LONG
	    LOCAL PosStart  AS LONG
	    LOCAL br        AS BinaryReader
	    // Move RecHeader to the Top of File
	    SELF:oMS:Position := 0
	    SELF:aMods := xARRAY{}
	    //
	    PosStart := 0
	    PosData := (INT)SELF:oMS:Position + FabRecheader.Size
	    //(int)_sizeof(AEFRecHeader)
	    SELF:FillRecHeader()
	    //
	    aTemp := ArrayCreate( 3 )
	    //
	    WHILE( SELF:oRecHeader:uiType != FabVODefinitions.REC_END )
		    //
		    DO CASE
			    CASE SELF:oRecHeader:uiType == 0x0107 //AEF_REC_APPOPTIONS
//			    	? SELF:oRecHeader:ulLength
//			    	? "-----------"
					IF SELF:oRecHeader:ulLength == 76
				    	SELF:oMS:Position := PosData
				    	br := BinaryReader{SELF:oMS}
				    	br:ReadInt32()
				    	br:ReadInt32()
				    	br:ReadInt32()
				    	/*? "typeinfer" , */br:ReadInt32()
				    	br:ReadInt32()
				    	br:ReadInt32()
				    	br:ReadInt32()
				    	/*? */SELF:lOptionIntegerDivisions := br:ReadInt32() == 1
				    	/*? "range" , */br:ReadInt32()
				    	/*? "overflow" , */SELF:lOptionOverflow := br:ReadInt32() == 1
					ENDIF
			    	
			    CASE SELF:oRecHeader:uiType == FabVODefinitions.APPNAME
				    // Application Name
				    SELF:oMS:Position := PosData
				    SELF:cName := FabTools.ReadPSZString( SELF:oMS, (LONG)SELF:oRecHeader:ulLength )
			    CASE SELF:oRecHeader:uiType = FabVODefinitions.SPATH
    			    SELF:oMS:Position := PosData
				    AAdd(SELF:aLibs, FabTools.ReadPSZString( SELF:oMS, (LONG)SELF:oRecHeader:ulLength ))
			    CASE SELF:oRecHeader:uiType = FabVODefinitions.UDC
    			    SELF:oMS:Position := PosData
				    AAdd(SELF:aUDCs, xARRAY{FabTools.ReadPSZString( SELF:oMS, (LONG)SELF:oRecHeader:ulLength ), PosStart})
			    CASE SELF:oRecHeader:uiType = FabVODefinitions.APPBODY
    			    SELF:oMS:Position := PosData+8
				    br := BinaryReader{ SELF:oMS }
				    LOCAL b AS BYTE
				    b := br:ReadByte()
				    IF( b == 1)
					    SELF:cType := "D"
	                ELSE
        			    SELF:oMS:Position := PosData+4
        			    b := br:ReadByte()
	                    IF( b == 1) .or. b == 255
					        SELF:cType := "L"
					    ENDIF
	                ENDIF
/*	                SELF:oMS:Position := PosData+36
	                ? "console:"
	                FOR LOCAL n := 1 AS INT UPTO 12
	                	? br:ReadByte()
	                NEXT*/
	                SELF:oMS:Position := PosData+44
	                b := br:ReadByte()
	                IF( b == 1) .or. b == 255
	                	SELF:cType := "C"
	                ENDIF
	                
			    CASE SELF:oRecHeader:uiType = FabVODefinitions.LIBFLG
    			    SELF:oMS:Position := PosData
				    br := BinaryReader{ SELF:oMS }
				    IF( br:ReadByte() == 1)
					    SELF:cType := "L"
				    ENDIF
			    CASE SELF:oRecHeader:uiType = FabVODefinitions.DLLFLG
    			    SELF:oMS:Position := PosData
				    br := BinaryReader{ SELF:oMS }
				    IF( br:ReadByte() == 1)
					    SELF:cType := "D"
				    ENDIF
			    CASE SELF:oRecHeader:uiType = FabVODefinitions.EXENAME
    			    SELF:oMS:Position := PosData
				    SELF:cExeName := FabTools.ReadPSZString( SELF:oMS, (LONG)SELF:oRecHeader:ulLength )
			    CASE SELF:oRecHeader:uiType = FabVODefinitions.APPDESC
    			    SELF:oMS:Position := PosData
				    SELF:cDescription := FabTools.ReadPSZString( SELF:oMS, (LONG)SELF:oRecHeader:ulLength )
			    CASE SELF:oRecHeader:uiType == FabVODefinitions.APPBODY
    			    SELF:oMS:Position := PosData + 37
				    br := BinaryReader{ SELF:oMS }
				    //
				    //SELF:dwLastBuild := Bin2DW( CHR( br:ReadByte() ) + CHR( br:ReadByte() ) + CHR( br:ReadByte() ) + CHR( br:ReadByte() ) )
				    //SELF:dwCreateTime := Bin2DW( CHR( br:ReadByte() ) + CHR(br:ReadByte() ) + CHR( br:ReadByte() ) + CHR( br:ReadByte() ) )
				    SELF:dwLastBuild := br:ReadUInt32()
				    SELF:dwCreateTime := br:ReadUInt32()
			    CASE SELF:oRecHeader:uiType == FabVODefinitions.EXTMODNAME
				    // We are viewing an external Module
				    aTemp[ 3 ] := TRUE
			    CASE SELF:oRecHeader:uiType == FabVODefinitions.MODNAME
				    // Add the module name, and store the pointer where it starts
				    IF !Empty( aTemp[ 1 ] )
					    AAdd( SELF:aMods, aTemp )
 				    ENDIF
				    // One More module
				    SELF:iMods := SELF:iMods + 1
				    //
				    aTemp := ArrayCreate( 3 )
				    aTemp[ 1 ] := ""
				    aTemp[ 2 ] := 0
				    aTemp[ 3 ] := FALSE
				    // Add the module name, and store the pointer where it starts
    			    SELF:oMS:Position := PosData
				    aTemp[ 1 ] := FabTools.ReadPSZString( SELF:oMS, (LONG)SELF:oRecHeader:ulLength )
				    aTemp[ 2 ] := PosStart
		    ENDCASE
		    // Move to the next Record
		    PosStart 	:= PosData + (LONG)SELF:oRecHeader:ulLength
		    SELF:oMS:Position := PosStart
		    SELF:FillRecHeader()
		    //  The Data will be there
		    PosData		:= PosStart
		    PosData		:= PosData + FabRecHeader.Size
		    //(long)_sizeof(AEFRecHeader)
		    //
	    ENDDO
	    // Add the module name, and store the pointer where it starts
	    IF !Empty( aTemp[ 1 ] )
		    AAdd( SELF:aMods, aTemp )
	    ENDIF
    RETURN 

[Obsolete];
    METHOD	SortByName( ) AS VOID  
/*	    // Sort Entities by Names
	    SELF:aMods := ASort( SELF:aMods, , , { |x,y| x[1] < y[ 1 ] } )
	    //*/
    RETURN

    ACCESS Success AS LOGIC 	
    RETURN SELF:lSuccess


/*    ACCESS UDCNameList AS ARRAY  
    RETURN SELF:aUDCs*/

    PROTECT METHOD FillRecHeader() AS VOID
        LOCAL br AS BinaryReader
        //
	    br := BinaryReader{ SELF:oMS }
	    //
	    SELF:oRecHeader:uiType := br:ReadUInt16()
	    SELF:oRecHeader:ulLength := br:ReadUInt32()
	    //
    RETURN
    
END CLASS

END NAMESPACE
