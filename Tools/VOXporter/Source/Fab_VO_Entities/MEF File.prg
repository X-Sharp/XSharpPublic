//#include "GlobalDefines.vh"
#using System.IO
#using System.Text
#using FabToolsNS

DEFINE BINARY_WED := 10
DEFINE BINARY_MED := 16
DEFINE BINARY_FED := 14
DEFINE BINARY_DED := 11
DEFINE BINARY_FLD := 12
DEFINE BINARY_IND := 13
DEFINE BINARY_ORD := 30

CLASS Designer
	PROPERTY Name AS STRING AUTO
	PROPERTY Bytes AS BYTE[] AUTO
	PROPERTY Type AS INT AUTO
	CONSTRUCTOR()
		SELF:Bytes := BYTE[]{0}
	RETURN
	CONSTRUCTOR(_nType AS INT)
		SELF:Type := _nType
		SELF:Bytes := BYTE[]{0}
	RETURN
	CONSTRUCTOR(_cName AS STRING, _nType AS INT)
		SELF:Name := _cName
		SELF:Type := _nType
		SELF:Bytes := BYTE[]{0}
	RETURN
	CONSTRUCTOR(_cName AS STRING, _nType AS INT, _aBytes AS BYTE[])
		SELF:Name := _cName
		SELF:Type := _nType
		SELF:Bytes := _aBytes
	RETURN
	METHOD AppendBytes(aBytes AS BYTE[]) AS VOID
		LOCAL aTemp AS BYTE[]
		aTemp := BYTE[]{SELF:Bytes:Length + aBytes:Length}
		LOCAL nIndex := 1 AS INT
		FOR LOCAL n := 1 AS INT UPTO SELF:Bytes:Length
			aTemp[nIndex] := SELF:Bytes[n]
			nIndex ++
		NEXT
		FOR LOCAL n := 1 AS INT UPTO aBytes:Length
			aTemp[nIndex] := aBytes[n]
			nIndex ++
		NEXT
		SELF:Bytes := aTemp      
	PROPERTY MustExport AS LOGIC
		GET
			SWITCH SELF:Type
			CASE BINARY_WED		// Window
			CASE BINARY_MED		// Menu
			CASE BINARY_FED		// FieldSpec
			CASE BINARY_DED		// DBServer
				RETURN TRUE
			END SWITCH
			RETURN FALSE
		END GET
	END PROPERTY
	PROPERTY IsDedHelper AS LOGIC
		GET
			SWITCH SELF:Type
			CASE BINARY_FLD
			CASE BINARY_IND
			CASE BINARY_ORD
				RETURN TRUE
			END SWITCH
			RETURN FALSE
		END GET
	END PROPERTY
	PROPERTY Extension AS STRING 
		GET
			SWITCH SELF:Type
			CASE BINARY_WED
				RETURN ".xsfrm"
			CASE BINARY_MED
				RETURN ".xsmnu"
			CASE BINARY_FED
				RETURN ".xsfs"
			CASE BINARY_DED
				RETURN ".xsdbs"
			CASE BINARY_FLD
				RETURN ".vnfld"
			CASE BINARY_IND
				RETURN ".vnind"
			CASE BINARY_ORD
				RETURN ".vnord"
			END SWITCH
			RETURN ".bin"
		END GET
	END PROPERTY  
	PROPERTY FileName AS STRING GET Name+Extension
END CLASS

BEGIN NAMESPACE Fab_VO_Entities

    CLASS FabMEFFile	INHERIT	FabModuleBase
        // Open the AEF file through a FileStream
        PROTECT oFS             AS FileStream
        // We will copy the file into memory
        PROTECT oMS             AS MemoryStream
	    // Pointer to first RecHeader
	    PROTECT	oRecHeader		AS FabRecHeader
	    // Init was done by a FileName
	    PROTECT	lFile			AS LOGIC
	    // Init / Last operation successful
	    PROTECT	lSuccess		AS LOGIC
	    // Qty of Entities
	    PROTECT	iEnt			AS DWORD
	    // Array with Entity Name, Prototype and Start
	    PROTECT	aEnt			AS xARRAY
	    // Version of VO who create the file
	    PROTECT	cCreator		AS STRING
	    // Is a Valid MEF File ?
	    PROTECT	lIsMef			AS	LOGIC
	    // Thanks to Paul Piko for these
	    PROTECT dwLastBuild		AS	DWORD
	    PROTECT dwCreateTime	AS	DWORD
	    // Fully Qualified Name of an external Module
	    PROTECT cExtName		AS	STRING
	    // Pointer to the source of the external Module
	    PROTECT ptrExtData		AS	LONG
	    // Size of Data
	    PROTECT	dwExtSize		AS	LONG
	    // Start of MEF 
	    PROTECT PosBase         AS  LONG
	    //
	    PROTECT	oPrg			AS	FabPRGFile

EXPORT aDesigners := System.Collections.Generic.List<Designer>{} AS System.Collections.Generic.List<Designer>

    DESTRUCTOR()	
	    SELF:Close()
    RETURN 

    METHOD Close() AS VOID  
	    //
	    IF SELF:lFile
	        SELF:oMS:Close()
	        SELF:oFS:Close()
	    ENDIF
	    //
	    SELF:lSuccess := FALSE
	    //
    RETURN

/*    VIRTUAL ACCESS CreateDate AS DATE  
	    LOCAL dDate	AS	DATE
	    //
	    dDate := SToD( "19700101" )
	    ddate := ddate + Integer( SELF:dwCreateTime / 86400 )
    RETURN dDate


    VIRTUAL ACCESS CreateTime AS STRING  
	    LOCAL cTime	AS	STRING
	    //
	    cTime := FabTools.TString( SELF:dwCreateTime % 86400 )
    RETURN cTime*/


    METHOD EntityFind( cEntity AS STRING, nType AS DWORD ) AS OBJECT  
	    LOCAL nCpt		AS	DWORD
	    LOCAL oEnt		AS	FabMEFEntity
	    LOCAL lFound	AS	LOGIC
	    //
	    FOR nCpt := 1 TO SELF:EntityCount
		    oEnt := (FabMEFEntity)SELF:aEnt[ nCpt ]
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

    ACCESS EntityListList AS System.Collections.Generic.List<FabMEFEntity>
    	LOCAL oList AS System.Collections.Generic.List<FabMEFEntity>
    	LOCAL aList AS xARRAY
    	oList := System.Collections.Generic.List<FabMEFEntity>{}
    	aList := SELF:EntityList
    	FOR LOCAL n := 1 AS DWORD UPTO ALen(aList)
    		oList:Add((FabMEFEntity)aList[n])
    	NEXT  
    RETURN oList

    VIRTUAL ACCESS	EntityList AS xARRAY  
    RETURN SELF:aEnt


    METHOD ExportModule( cFileName AS STRING ) AS LOGIC  
    /*
	    LOCAL	dw			AS DWORD
	    LOCAL	RecHeader	IS AefRecHeader
	    LOCAL	hfExport	AS PTR
	    LOCAL	pbData		AS BYTE PTR
	    LOCAL	lSuccess	AS	LOGIC
	    //
	    IF(!Empty(cFileName))
		    hfExport := CreateFile( String2Psz(cFileName), DWORD(_CAST, _OR(GENERIC_READ, GENERIC_WRITE)), 0, NULL_PTR, CREATE_ALWAYS, 0, NULL_PTR)
		    // Creation OK ?
		    IF(hfExport != INVALID_HANDLE_VALUE)
			    // Add Header
			    RecHeader:uiType 	:= AEF_REC_HEADER
			    RecHeader:ulLength  := MEFFILE_VERSION_LEN+MEFFILE_TEXT_LEN
			    // Write Header
			    WriteFile(hfExport, @RecHeader, _sizeof(AEFRECHEADER), @dw, NULL_PTR)
			    WriteFile(hfExport, PTR(_CAST,_CHR(0x80)+_Chr(0x02)), 2, @dw, NULL_PTR)
			    // And Text
			    WriteFile(hfExport, PTR(_CAST, MEFFILE_TEXT), MEFFILE_TEXT_LEN, @dw, NULL_PTR)
			    // Get start of Module
			    oRecHeader 	:= SELF:lpbBase
			    // Get it as a Pointer to BYTEs
			    pbData 		:= oRecHeader
			    pbData		+= _sizeof(AEFRECHEADER)
			    WHILE(TRUE)
				    // Add write data
				    WriteFile(hfExport, oRecHeader, _sizeof(AEFRECHEADER), @dw, NULL_PTR)
				    WriteFile(hfExport, pbData, oRecHeader:ulLength, @dw, NULL_PTR)
				    oRecHeader 	:= pbData + oRecHeader:ulLength
				    pbData 		:= oRecHeader
				    pbData		+= _sizeof(AEFRECHEADER)
				    // Until End of Module or End of AEF
				    IF(oRecHeader:uiType == AEF_REC_MODNAME .or. oRecHeader:uiType == AEF_REC_END)
					    EXIT
				    ENDIF
			    ENDDO
			    // Add End Marker
			    RecHeader:uiType 	:= AEF_REC_END
			    RecHeader:ulLength  := 0
			    WriteFile(hfExport, @RecHeader, _sizeof(AEFRECHEADER), @dw, NULL_PTR)
			    // Close File
			    CloseHandle(hfExport)
			    lSuccess := TRUE
		    ENDIF
	    ENDIF
    RETURN lSuccess
    */
    // TODO Write Export
    RETURN FALSE


    ACCESS ExternalFile AS STRING  
    RETURN SELF:cExtName


    ACCESS	ExternalSource AS STRING  
	    LOCAL cData	AS	STRING
	    LOCAL data  AS  BYTE[]
	    LOCAL ToASCII AS Encoding
	    //
	    IF ( SELF:ptrExtData != 0 )
		    //
		    data := BYTE[]{SELF:dwExtSize}
		    SELF:oMS:Position := SELF:ptrExtData
		    SELF:oMS:Read( data, 0, SELF:dwExtSize )
//		    ToASCII := Encoding.ASCII
		    ToASCII := ascenc
		    cData := ToASCII:GetString(data)
	    ENDIF
	    //
    RETURN cData


    ACCESS FullPath AS STRING  
    RETURN SELF:cFilePath


    CONSTRUCTOR( cFile AS STRING ) 
	    SUPER()
	    LOCAL wVersion	AS WORD
	    LOCAL br        AS BinaryReader
    //	LOCAL oError	AS	FabError
	    //
	    SELF:oRecHeader := FabRecHeader{}
        // Open the file
        SELF:oFS := FileStream{ cFile, FileMode.Open, FileAccess.Read }
        // Create a MemoryStream to hold the whole file
        SELF:oMS := MemoryStream{ }
        SELF:oMS:SetLength((INT)SELF:oFS:Length)
        // Read file to memory
        SELF:oFS:Read( SELF:oMS:GetBuffer(), 0, (INT)SELF:oFS:Length )
		// Start of FabMEFFile record
		SELF:FillRecHeader()
	    // Check if we have a FabMEFFile
	    IF	( SELF:oRecHeader:uiType != FabVODefinitions.REC_HEADER ) //.or. ;
		    //( SELF: oRecHeader:ulLength != MEFFILE_VERSION_LEN+ MEFFILE_TEXT_LEN )
		    //
		    SELF:cError := "Invalid MEF File STRUCTURE"
		    SELF:dwError := 2
/*			oError := FabError{}
		    oError:Description := "Invalid MEF File structure"
		    oError:CanSubstitute := TRUE
		    oError:SubstituteType := OBJECT
		    oError:Raise() */
		    //
		    SELF:Close()
		    //
		    RETURN
	    ENDIF
	    // Get Version Info
	    SELF:oMS:Position := 0
	    SELF:oMS:Position += FabRecHeader.Size
	    //
	    br := BinaryReader{ SELF:oMS }
	    wVersion := br:ReadUInt16()
	    // Unknown Version type !!
	    IF(  _AND(wVersion, 0x0080) == 0 )
		    SELF:Close()
		    RETURN
	    ENDIF
	    // Back to Original Pos
	    SELF:oMS:Position := 0
	    SELF:oMS:Position += FabRecHeader.Size  
	    //
	    wVersion := (WORD)_AND( (DWORD)wVersion, _NOT(0x0080) )
	    //
	    IF(wVersion = 256)
		    //
		    SELF:cCreator := "CA-Visual Objects 1.0"
	    ELSEIF(wVersion = 512)
		    //
		    SELF:cCreator := "CA-Visual Objects 2.0"
	    ELSE
		    SELF:Close()
		    RETURN
	    ENDIF
	    //
	    SELF:lFile := TRUE
		SELF:cFilePath := cFile
	    //
	    SELF:aEnt   := xARRAY{}
	    //
	    SELF:lSuccess := TRUE
	    SELF:lIsMef	:= TRUE
	    // Fill Arrays with infos
	    SELF:Scan()
/*	    IF SELF:IsExternal
		    SELF:oPRG := FabPRGFile{ SELF:ExternalFile, SELF:ExternalSource }
		    SELF:aEnt := SELF:oPRG:EntityList
	    ENDIF*/
    RETURN 

    CONSTRUCTOR( oMemory AS MemoryStream, PosData AS LONG ) 
    //	LOCAL oError	AS	FabError
	    //
	    SUPER()
	    SELF:oRecHeader := FabRecHeader{}
	    // Start of FabMEFFile record
	    SELF:oMS := oMemory
	    SELF:PosBase := PosData
	    SELF:lFile := FALSE
	    //
	    SELF:aEnt   := xARRAY{}
	    //
	    SELF:lSuccess := TRUE
	    SELF:lIsMef	:= TRUE
	    // Fill Arrays with infos
	    SELF:Scan()
/*	    IF SELF:IsExternal
		    SELF:oPRG := FabPRGFile{ SELF:ExternalFile, SELF:ExternalSource }
		    SELF:aEnt := SELF:oPRG:EntityList
	    ENDIF*/
    RETURN 

    ACCESS IsExternal AS LOGIC 	
    RETURN !Empty( SELF:ExternalFile )


    ACCESS	IsMef AS LOGIC  
    RETURN	SELF:lIsMef


    ACCESS	IsPrg AS LOGIC 	
    RETURN	FALSE


/*    ACCESS LastBuildDate AS DATE 	
	    LOCAL dDate	AS	DATE
	    //
	    dDate := SToD( "19700101" ) 
	    ddate := ddate + Integer( SELF:dwlastbuild / 86400 )
    RETURN dDate


    ACCESS LastBuildTime AS STRING 	
	    LOCAL cTime	AS	STRING
	    //
	    cTime := TString( SELF:dwlastbuild % 86400 )
    RETURN cTime*/


    PROTECT METHOD Scan() AS VOID 	
	    LOCAL lFirst	AS	LOGIC
	    LOCAL oInfoTemp AS  FabEntInfo
	    LOCAL PosData   AS LONG
	    LOCAL PosStart  AS LONG
	    LOCAL br        AS BinaryReader
        LOCAL aBytes AS BYTE[]

	    // Move RecHeader to the Top of File
	    SELF:oMS:Position := SELF:PosBase
	    SELF:aEnt := xARRAY{}
	    //
	    PosStart := 0
	    PosData := (INT)SELF:oMS:Position + FabRecHeader.Size
	    //(int)_sizeof(AEFRecHeader)
	    SELF:FillRecHeader()
	    //
	    oInfoTemp := FabEntInfo{ SELF:oMS }
	    lFirst := FALSE
	    LOCAL nCurrentType := 0 AS INT
	    LOCAL cCurrentName := "" AS STRING
	    LOCAL oCurrentDesigner AS Designer
	    WHILE( SELF:oRecHeader:uiType != FabVODefinitions.REC_END )
		    //
		    ?  SELF:oRecHeader:uiType:ToString("X") , SELF:oRecHeader:ulLength
		    DO CASE
			    CASE SELF:oRecHeader:uiType == 0x44 // AEF_REC_ENTTYPE
			        SELF:oMS:Position := PosData+0
				    br := BinaryReader{ SELF:oMS }
				    nCurrentType := br:ReadInt16()
				    oInfoTemp:Type := nCurrentType
				    ? nCurrentType
				    IF nCurrentType == BINARY_MED .OR. nCurrentType == BINARY_WED .OR. ;
				    	 nCurrentType == BINARY_DED .OR. nCurrentType == BINARY_FED .OR. ;
				    	 nCurrentType == BINARY_FLD .OR. nCurrentType == BINARY_IND .OR. nCurrentType == BINARY_ORD
				    	oCurrentDesigner := Designer{cCurrentName , nCurrentType}
				    	SELF:aDesigners:Add(oCurrentDesigner)
				    END IF
			    CASE SELF:oRecHeader:uiType == 0x42 // AEF_REC_ENTCUSTOM
				    IF oCurrentDesigner != NULL
			        	SELF:oMS:Position := PosData+0
				    	br := BinaryReader{ SELF:oMS }
					    aBytes := br:ReadBytes((LONG)SELF:oRecHeader:ulLength)
					    oCurrentDesigner:Bytes := aBytes
				    ENDIF
			    CASE SELF:oRecHeader:uiType == 0x0123 // AEF_REC_ENTDISPNAME
			        SELF:oMS:Position := PosData
				    cCurrentName := FabTools.ReadPSZString( SELF:oMS, (LONG)SELF:oRecHeader:ulLength )
				    IF oCurrentDesigner != NULL
					    oCurrentDesigner:Name := cCurrentName
				    END IF
				    ? cCurrentName

			    CASE SELF:oRecHeader:uiType == 0x085 // AEF_REC_DT_STATIC
				    IF nCurrentType == 16 .AND. oCurrentDesigner != NULL
				        SELF:oMS:Position := PosData
				    	br := BinaryReader{ SELF:oMS }
					    aBytes := br:ReadBytes((LONG)SELF:oRecHeader:ulLength)
//					    oCurrentDesigner:AppendBytes(aBytes)
				    END IF



			    CASE SELF:oRecHeader:uiType == FabVODefinitions.MODBODY
			        SELF:oMS:Position := PosData+17
				    br := BinaryReader{ SELF:oMS }
				    //
				    //SELF:dwLastBuild := Bin2DW( CHR( br:ReadByte() ) + CHR( br:ReadByte() ) + CHR( br:ReadByte() ) + CHR( br:ReadByte() ) )
				    //SELF:dwCreateTime := Bin2DW( CHR( br:ReadByte() ) + CHR( br:ReadByte() ) + CHR( br:ReadByte() ) + CHR( br:ReadByte() ) )
				    SELF:dwLastBuild := br:ReadUInt32()
				    SELF:dwCreateTime := br:ReadUInt32()
			    CASE SELF:oRecHeader:uiType == FabVODefinitions.MODNAME
				    // Do we already have found a module name ?
				    IF !lFirst
					    lFirst := TRUE
    			        SELF:oMS:Position := PosData
				    	SELF:cName := FabTools.ReadPSZString( SELF:oMS, (LONG)SELF:oRecHeader:ulLength )
				    ELSE
					    // Yes, so we are on another one !! Stop
					    EXIT
				    ENDIF
			    CASE SELF:oRecHeader:uiType == FabVODefinitions.ENTBODY
    // Last Entity Informations
			        SELF:oMS:Position := PosData+29
				    br := BinaryReader{ SELF:oMS }    
				    //aTemp[ MEF_ENT_CREATETIME ] := Bin2DW( CHR( br:ReadByte() ) + CHR( br:ReadByte() ) + CHR( br:ReadByte() ) + CHR( br:ReadByte() ) )
				    //aTemp[ MEF_ENT_LASTBUILD ] := Bin2DW( CHR( br:ReadByte() ) + CHR( br:ReadByte() ) + CHR( br:ReadByte() ) + CHR( br:ReadByte() ) )
				    oInfoTemp:CreateTime := br:ReadUInt32()
				    oInfoTemp:LastBuild := br:ReadUInt32()
			    CASE SELF:oRecHeader:uiType == FabVODefinitions.ENTNAME
    // First Entity Information
				    // Only add Entities with source
//				    IF !Empty( oInfoTemp:Proto )
				    IF oInfoTemp:MustExport
					    AAdd( SELF:aEnt, FabMEFEntity{ oInfoTemp } )
					    //
					    oInfoTemp := FabEntInfo{ SELF:oMS }
				    ENDIF
				    // One more entity
				    SELF:iEnt := SELF:iEnt + 1
				    // Add the module name, and store the pointer where it starts
			        SELF:oMS:Position := PosData
				    oInfoTemp:Name := FabTools.ReadPSZString( SELF:oMS, (LONG)SELF:oRecHeader:ulLength )
				    ? "---------------"
				    ? oInfoTemp:Name
				    cCurrentName := oInfoTemp:Name
				    oCurrentDesigner := NULL
			    CASE SELF:oRecHeader:uiType == FabVODefinitions.ENTSOURCE
				    // Source code
				    oInfoTemp:Pos :=  PosData
				    oInfoTemp:Size := (LONG)SELF:oRecHeader:ulLength 
				    
				    IF nCurrentType == 16
				    	br := BinaryReader{ SELF:oMS }
					    aBytes := br:ReadBytes((LONG)SELF:oRecHeader:ulLength)
					    oCurrentDesigner:Bytes := aBytes
				    END IF
				    
			    CASE SELF:oRecHeader:uiType == FabVODefinitions.ENTPROTO
				    // Prototype
			        SELF:oMS:Position := PosData
				    oInfoTemp:Proto := FabTools.ReadPSZString( SELF:oMS, (LONG)SELF:oRecHeader:ulLength )
			    CASE SELF:oRecHeader:uiType == FabVODefinitions.EXTMODNAME
				    // We are viewing an external Module
			        SELF:oMS:Position := PosData
				    // FileName ( fully qualified )
				    SELF:cExtName := FabTools.ReadPSZString( SELF:oMS, (LONG)SELF:oRecHeader:ulLength )
				    //
			    CASE SELF:oRecHeader:uiType == FabVODefinitions.EXTMOD
				    // We are viewing an external Module
				    // We now have the Source of the External Module
				    SELF:ptrExtData := PosData
				    SELF:dwExtSize := (LONG)oRecHeader:ulLength
		    ENDCASE
		    // Move to the next Record
		    PosStart 	:= PosData + (LONG)SELF:oRecHeader:ulLength
		    SELF:oMS:Position := PosStart
		    SELF:FillRecHeader()
		    // The data will be there
		    PosData		:= PosStart
		    PosData		:= PosData + FabRecHeader.Size
		    //(long)_sizeof(AEFRecHeader)
		    //
	    ENDDO
	    //
//	    IF !Empty(  oInfoTemp:Proto  )
	    IF oInfoTemp:MustExport
		    AAdd( SELF:aEnt, FabMEFEntity{ oInfoTemp } )
	    ENDIF
	    //
	    SELF:lSuccess := ( SELF:iEnt == ALen( SELF:aEnt ) )
    RETURN

    PUBLIC METHOD	SortByName( ) AS VOID  
        LOCAL lOk   AS LOGIC
        LOCAL nCpt  AS LONG
        LOCAL nMax  AS LONG
        LOCAL oEnt1	AS	FabMEFEntity
        LOCAL oEnt2	AS	FabMEFEntity
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
                oEnt1 := (FabMEFEntity)SELF:aEnt[ (DWORD)nCpt ]
                oEnt2 := (FabMEFEntity)SELF:aEnt[ (DWORD)nCpt + 1 ]
                IF ( oEnt1:Name > oEnt2:Name )
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
