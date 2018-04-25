// FabTools.prg
#using System
#using System.IO
#using System.Text

BEGIN NAMESPACE FabToolsNS

   CLASS FabTools

      CONSTRUCTOR()
         RETURN
         
        STATIC METHOD GetToken( sString := "" AS STRING, nToken := 1 AS DWORD, cSep := " " AS STRING  ) AS STRING
        //g String,String Manipulation
        //l Extract a Token.
        //d Retrieve a Token in a delimited string, using a separator. \line
        //d The separator can be a string with more than one char. If so, the full string is used as a separator.
        //x <cString> is the Source\line
        //x <nToken> is the number of the token to retrieve. ( Default is 1 )\line
        //x <cSep> is a String with the separator. ( Default is " " )\line
        //e	FabGetToken( "Hello Visual Object World !", 2, " " )	--> "Visual"
        /*
        Retrieve a Token in a delimited string, using a separator.
            FabGetToken( "Hello Visual Object World !", 2, " " )
            --> "Visual"
        	
            The separator is a string( One or More )

        */
            LOCAL wPos		AS	DWORD
            LOCAL sVal		AS	STRING
            LOCAL nCount	AS	DWORD
            //
            sVal := ""
            nCount := 1
            //
            //
            IF ( SLen( sString ) > 0 )
                WHILE	.T.
	                wPos := At2( cSep, sString )
	                //
	                IF ( wPos > 0 )
		                // Get Token
		                sVal := SubStr3( sString, 1, wPos - 1 )
		                // Extract String
		                sString := SubStr2( sString, wPos + SLen( cSep ) )
		                //
		                WHILE ( SubStr( sString, 1, SLen( cSep ) ) == cSep )
			                // Move Next
			                sString := SubStr2( sString, SLen( cSep )+1 )
		                ENDDO
		                // Search for next Token
		                nCount ++
		                IF ( nCount > nToken )
			                EXIT
		                ENDIF
	                ELSE
		                IF ( nCount >= nToken )
			                sVal := sString
		                ELSE
			                sVal := ""
		                ENDIF
		                // Bye
		                EXIT
	                ENDIF
                ENDDO
            ENDIF
        RETURN sVal

        STATIC METHOD GMTUnixTimeToLocalUnixTime( dwUnixTime AS DWORD ) AS DWORD
        //g Date & Time
        //l Convert a GMT UnixTime to a Local UnixTime
        //p Convert a GMT UnixTime to a Local UnixTime
        //a <dwUnixTime> is a DWord with the UnixTime to convert
        //d The UnixTime is a DWord with the number of seconds elapsed since Midnight 1 Jan 1970.
        //d  It's usually exprimed in UTC time.\line
        //e // Convert UnixTime
        //e dwTime := FabGMTUnixTimeToLocalUnixTime( dwTime )
        //e // Retrieve VO Date
        //e ddate := ConDate( 1970,01,01)
        //e ddate := ddate + Integer( dwTime / 86400 )
        //e // And VO Time
        //e	cTime := FabTString( dwTime % 86400 )
        //r A Dword with the corresponding Local Time.
        LOCAL Origin AS DateTime
        LOCAL Work   AS DateTime
        LOCAL Unix   AS DateTime
        LOCAL Span   AS TimeSpan
        // 
        Unix := DateTime{1970, 1, 1, 0, 0, 0, 0, DateTimeKind.Utc}
        origin := DateTime{1970, 1, 1, 0, 0, 0, 0, DateTimeKind.Utc}
        origin:AddSeconds(dwUnixTime)
        Work := origin:ToLocalTime()
        Span := Work - Unix
        //(DateTime.UtcNow - new DateTime(1970,1,1,0,0,0)).TotalSeconds
        RETURN (DWORD)Span:TotalSeconds
        
        /*
	        LOCAL PTRTZ 	IS 	_WINTIME_ZONE_INFORMATION
	        LOCAL GMTBias 	AS 	LONG
	        LOCAL StdBias	AS	LONG
	        LOCAL DlBias	AS	LONG
	        LOCAL TotalBias	AS	LONG
	        LOCAL dDate		AS	DATE
	        LOCAL nTime		AS	DWORD
	        LOCAL dStart	AS	DATE
	        LOCAL cStart	AS	STRING
	        LOCAL nStart	AS	DWORD
	        LOCAL dEnd		AS	DATE
	        LOCAL cEnd		AS	STRING
	        LOCAL nEnd		AS	DWORD
 	        // Retrieve Info about the current TimeZone
	        GetTimeZoneInformation( @ptrTZ )
	        // No StandardDate Month !!
	        IF ( ptrTZ:StandardDate:wMonth == 0 )
		        RETURN dwUnixTime
	        ENDIF
	        // Build GMT Info
	        ddate := ConDate( 1970,01,01)
	        ddate := ddate + Integer( dwUnixTime / 86400 )
	        nTime := ( dwUnixTime % 86400 )
	        // Search Start date / time of Daylight Saving
	        cStart := FabSystemTimeToVOTime( @ptrTZ:DayLightNDate )
	        nStart := Secs( cStart )
	        //Absolute Format
	        IF ( ptrTZ:DayLightNDate:wYear != 0 )
		        dStart := FabSystemTimeToVODate( @ptrTZ:DayLightNDate )
	        ELSE
		        // Day-in-Month format
		        ptrTZ:DayLightNDate:wDay := Max( ptrTZ:DayLightNDate:wDay, 1 )
		        dStart := FabGetFirstDOW( DOW_SUNDAY, ConDate( Year( dDate ), ptrTZ:DayLightNDate:wMonth, 1 ) )
		        dStart := dStart + ( 7 * ( ptrTZ:DayLightNDate:wDay - 1 ) )
		        IF ( dStart > FabGetLastDOW( DOW_SUNDAY, ConDate( Year( dDate ), ptrTZ:DayLightNDate:wMonth, 1 ) ) )
			        dStart := FabGetLastDOW( DOW_SUNDAY, ConDate( Year( dDate ), ptrTZ:DayLightNDate:wMonth, 1 ) )
		        ENDIF
	        ENDIF
	        // Search End Date / time
	        cEnd := FabSystemTimeToVOTime( @ptrTZ:StandardDate )
	        nEnd := Secs( cEnd )
	        // Absolute Format
	        IF ( ptrTZ:StandardDate:wYear != 0 )
		        dEnd := FabSystemTimeToVODate( @ptrTZ:StandardDate )
	        ELSE
		        // Day-in-Month Format
		        ptrTZ:StandardDate:wDay := Max( ptrTZ:StandardDate:wDay, 1 )
		        dEnd := FabGetFirstDOW( DOW_SUNDAY, ConDate( Year( dDate ), ptrTZ:StandardDate:wMonth, 1 ) )
		        dEnd := dEnd + ( 7 * ( ptrTZ:StandardDate:wDay - 1 ) )
		        IF ( dEnd > FabGetLastDOW( DOW_SUNDAY, ConDate( Year( dDate ), ptrTZ:StandardDate:wMonth, 1 ) ) )
			        dEnd := FabGetLastDOW( DOW_SUNDAY, ConDate( Year( dDate ), ptrTZ:StandardDate:wMonth, 1 ) )
		        ENDIF
	        ENDIF
	        // Initialize Bias
	        GMTBias := ptrTZ:Bias
	        StdBias := PTRTZ:StandardBias
	        DlBias := PTRTZ:DaylightBias
	        //
	        TotalBias := GMTBias
	        //
	        IF ( dDate == dStart )
		        IF ( nTime >= nStart )
			        TotalBias := TotalBias + DlBias
		        ENDIF
	        ELSEIF ( dDate == dEnd )
		        IF ( nTime <= nEnd )
			        TotalBias := TotalBias + DlBias
		        ENDIF
	        ELSEIF ( dDate > dStart ) .and. ( dDate < dEnd )
		        TotalBias := TotalBias + DlBias
	        ELSE
		        TotalBias := TotalBias + StdBias
	        ENDIF
	        // Apply Bias
	        IF ( long(dwUnixTime) >= TotalBias)
		        dwUnixTime := dword( long(dwUnixTime) - ( TotalBias * 60 ) )
	        ELSE
		        dwUnixTime := 0
	        ENDIF
	        //
        RETURN dwUnixTime	
        */

/*        STATIC METHOD TString( dwSeconds AS DWORD ) AS STRING
        //g Date & Time
        //l Convert a specified number of seconds to a 24h time string.
        //p Convert a specified number of seconds to a 24h time string.
        //a <dwSeconds>	The number of seconds to convert.
        //r The corresponding Time
	        LOCAL cTime	AS	STRING
	        LOCAL cSep	AS	STRING
	        LOCAL nSec, nMin, nHour AS DWORD
	        //
	        cSep := Chr( GetTimeSep() )
	        nSec := ( dwSeconds % 60 )
	        nHour := Integer( dwSeconds / 3600 )
	        nMin := Integer( dwSeconds / 60 ) -( nHour * 60 )
	        cTime := StrZero( nHour, 2 ) + cSep + StrZero( nMin, 2 ) + cSep + StrZero( nSec, 2 )
	        //
        RETURN cTime
*/        
        STATIC METHOD ReadPSZString( oBr AS BinaryReader ) AS STRING
            LOCAL oSB    AS StringBuilder
            LOCAL i      AS INT
            LOCAL b      AS System.Char
            LOCAL Result AS STRING
            //
            oSB := StringBuilder{ "" }
            //
            FOR i := 0 TO ( oBr:BaseStream:Length - 1 )
                b := oBr:ReadChar()
                IF ( b == 0 )
                    EXIT
                ENDIF
                oSB:Append( b )
            NEXT
            Result := oSB:ToString()
        RETURN Result
        
        STATIC METHOD ReadPSZString( oBr AS BinaryReader, BlocLength AS LONG ) AS STRING
            LOCAL oSB    AS StringBuilder
            LOCAL i      AS INT
            LOCAL Result AS STRING
            LOCAL Data   AS System.Char[]
            //
            Data := oBr:ReadChars( BlocLength )
            //
            i := System.Array.IndexOf(Data, (Char)0)
            oSB := StringBuilder{ "" }
            oSB:Append( Data,0, i )
            //
            Result := oSB:ToString()
        RETURN Result

        STATIC METHOD ReadPSZString( oS AS Stream ) AS STRING
            LOCAL oSB    AS StringBuilder
            LOCAL obr     AS BinaryReader
            LOCAL i      AS INT
            LOCAL b      AS System.Char
            LOCAL Result AS STRING
            //
            oSB := StringBuilder{ "" }
            obr := BinaryReader{ oS, ascenc }
            //
            FOR i := 0 TO ( oBr:BaseStream:Length - 1 )
                b := oBr:ReadChar()
                IF ( b == 0 )
                    EXIT
                ENDIF
                oSB:Append( b )
            NEXT
            Result := oSB:ToString()
        RETURN Result
        
        STATIC METHOD ReadPSZString( oS AS Stream, BlocLength AS LONG ) AS STRING
            LOCAL oSB    AS StringBuilder
            LOCAL oBr     AS BinaryReader
            LOCAL i      AS INT
            LOCAL Result AS STRING
            LOCAL Data   AS System.Char[]
            //
            oBr := BinaryReader{ oS, ascenc }
			//	    
            Data := oBr:ReadChars( BlocLength )
            //
            i := System.Array.IndexOf(Data, (Char)0)
            IF i == -1 // in a few cases strings were not null terminated
            	i := data:Length
            END IF
            oSB := StringBuilder{ "" }
            oSB:Append( Data,0, i )
            //
            Result := oSB:ToString()
        RETURN Result

/*        STATIC METHOD ExtractFileDir( FileName AS STRING) AS STRING   
        //g Files,Files Related Classes/Functions
        //p Extract FileDir info from FullPath String
        //l Extract FileDir info from FullPath String
        //r the FileDir ( never ended by a \ char except for root )
        //e "C:\TEST\TESTFILE.TST"			->	"C:\TEST"
        //e "C:\TESTFILE.TST"				->	"C:\"
        //e "\TEST\TESTFILE.TST"			->	"\TEST"
        //e "TEST\TESTFILE.TST"				->	"TEST"
        //e "\\SERVER\TEST\TESTFILE.TST"	->	"\\SERVER\TEST"
	        LOCAL wPos		AS	DWORD
	        LOCAL cResult	AS	STRING
	        //
	        wPos := SLen( FileName )
	        // Starting at end of String, search for a Path or Drive separator
	        WHILE ( wPos > 0 ) .and. !InStr( SubStr3( FileName, wPos, 1 ), "\:" )
		        wPos --
	        ENDDO
	        // Don't forget to suppress separator
	        IF ( wPos > 1 ) .AND. SubStr3( FileName, wPos, 1 ) == "\" .and. !InStr( SubStr3( FileName, wPos-1, 1 ), "\:" )
		        wPos--
	        ENDIF
	        //
	        cResult := SubStr3( FileName, 1, wPos )
        RETURN cResult
*/
/*        STATIC METHOD ExtractFileDrive( FileName AS STRING ) AS STRING  
        //g Files,Files Related Classes/Functions
        //p Extract FileDrive info from FullPath String
        //l Extract FileDrive info from FullPath String
        //d This functions also support FullPath information in UNC format. If so, the string
        //d  returned will hold the full UNC path.
        //r the FileDrive
        //e "C:\TEST\TESTFILE.TST"			->	"C:"
        //e "C:\TESTFILE.TST"				->	"C:"
        //e "\TEST\TESTFILE.TST"			->	""
        //e "TEST\TESTFILE.TST"				->	""
        //e "\\SERVER\TEST\TESTFILE.TST"	->	"\\SERVER\TEST"
	        LOCAL wPos		AS	DWORD
	        LOCAL wPos2		AS	DWORD
	        LOCAL cResult	AS	STRING
	        // Standard filepath string
	        IF ( SLen( FileName ) >= 3 ) .AND. ( SubStr3( FileName, 2, 1 ) == ":" )
		        cResult := Upper( SubStr3( FileName, 1, 2 ) )
	        ELSEIF ( SLen( FileName ) >= 2 ) .AND. ( SubStr3( FileName, 1, 2 ) == "\\" )
		        // UNC naming
		        wPos2 := 0
		        wPos := 3
		        WHILE ( wPos < SLen( FileName ) ) .and. ( wPos2 < 2 )
			        IF SubStr3( FileName, wPos, 1 ) == "\"
				        wPos2 ++
			        ENDIF
			        IF ( wPos2 < 2 )
				        wPos ++
			        ENDIF
		        ENDDO
		        IF ( SubStr3( FileName, wPos, 1 ) == "\" )
			        wPos --
		        ENDIF
		        cResult := SubStr3( FileName, 1, wPos )
	        ENDIF
        RETURN cResult
*/
        STATIC METHOD ExtractFileExt( FileName AS STRING) AS STRING
        //g Files,Files Related Classes/Functions
        //p Extract File Extension info from FullPath String
        //l Extract File Extension info from FullPath String
        //r the File Extension ( always start with a . char )
        //e "C:\TEST\TESTFILE.TST"			->	".TST"
        //e "C:\TEST\TESTFILE."				->	"."
        //e "C:\TEST\TESTFILE"				->	""
	        LOCAL wPos		AS	DWORD
	        LOCAL cResult	AS	STRING
	        //
	        wPos := SLen( FileName )
	        // Starting at end of String, search for a Path, Drive or extension separator
	        WHILE ( wPos > 0 ) .and. !InStr( SubStr3( FileName, wPos, 1 ), "\:." )
		        wPos --
	        ENDDO
	        //
	        IF ( wPos > 0 ) .AND. SubStr3( FileName, wPos, 1 ) == "."
		        cResult := SubStr2( FileName, wPos )
	        ENDIF
        RETURN cResult

        STATIC METHOD ExtractFileInfo( cPath AS STRING ) AS STRING
        //g Files,Files Related Classes/Functions
        //p Get File Info from a FullPath String
        //d Get File Info from a FullPath String
        //a <cPath> is a string with the FullPath information
        //r The File Name and extension stored in <cPath>
	        LOCAL cFile, cExt	AS STRING
	        //
	        cFile := FabTools.ExtractFileName( cPath )
	        cExt := FabTools.ExtractFileExt( cPath )
	        //
        RETURN ( cFile + cExt )

        STATIC METHOD ExtractFileName( FileName AS STRING) AS STRING
        //g Files,Files Related Classes/Functions
        //p Extract FileName info from FullPath String
        //l Extract FileName info from FullPath String
        //r the FileName ( including the extension )
        //e "C:\TEST\TESTFILE.TST"			->	"TESTFILE"
        //e "C:\TEST\TESTFILE."				->	"TESTFILE"
        //e "C:\TEST\TESTFILE"				->	"TESTFILE"
        //e "\\SERVER\TEST\TESTFILE.TST"	->	"TESTFILE"
	        LOCAL wPos		AS	DWORD
	        LOCAL cResult	AS	STRING
	        //
	        wPos := SLen( FileName )
	        // Starting at end of String, search for a Path or Drive separator
	        WHILE ( wPos > 0 ) .and. !InStr( SubStr3( FileName, wPos, 1 ), "\:" )
		        wPos --
	        ENDDO
	        // Extract File Name and File Extension
	        cResult := SubStr2( FileName, wPos + 1 )
	        // Now Remove the Extension
	        wPos := RAt( ".", cResult )
	        IF ( wPos > 0 )
		        cResult := SubStr( cResult, 1, wPos - 1 )
	        ENDIF
        RETURN cResult

        STATIC METHOD FabExtractFilePath( FileName AS STRING) AS STRING
        //g Files,Files Related Classes/Functions
        //p Extract FilePath info from FullPath String
        //l Extract FilePath info from FullPath String
        //r the FilePath ( always ended by a \ char )
        //e "C:\TEST\TESTFILE.TST"			->	"C:\TEST\"
        //e "C:\TESTFILE.TST"				->	"C:\"
        //e "\TEST\TESTFILE.TST"			->	"\TEST\"
        //e "TEST\TESTFILE.TST"				->	"TEST\"
        //e "\\SERVER\TEST\TESTFILE.TST"	->	"\\SERVER\TEST\"
	        LOCAL wPos		AS	DWORD
	        LOCAL cResult	AS	STRING
	        //
	        wPos := SLen( FileName )
	        // Starting at end of String, search for a Path or Drive separator
	        WHILE ( wPos > 0 ) .and. !InStr( SubStr3( FileName, wPos, 1 ), "\:" )
		        wPos --
	        ENDDO
	        //
	        cResult := SubStr3( FileName, 1, wPos )
        RETURN cResult
               
   END CLASS
   
END NAMESPACE // Fab_VO_Entities
   
