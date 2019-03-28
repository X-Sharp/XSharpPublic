CLASS Console
	PROTECT hConOut AS PTR
	PROTECT hConIn AS PTR
	PROTECT hConErr AS PTR
	PROTECT lNewConsole AS LOGIC

CONSTRUCTOR( lNew ) 

	IF ! IsLogic( lNew )
		lNew := FALSE
	ENDIF

	SELF:lNewConsole := lNew

	hConOut := GetStdHandle( STD_OUTPUT_HANDLE )
	hConErr := GetStdHandle( STD_ERROR_HANDLE )

	IF ( hConOut == NULL )
		IF lNew
			AllocConsole( )
			hConOut := GetStdHandle( STD_OUTPUT_HANDLE )
		ENDIF
	ENDIF

	hConIn := GetStdHandle( STD_INPUT_HANDLE )

	RETURN 

METHOD Clear( ) 
	LOCAL coordScreen := 0 AS DWORD
	LOCAL cCharsWritten	 AS DWORD
	LOCAL csbi IS _winCONSOLE_SCREEN_BUFFER_INFO
	LOCAL dwConSize AS DWORD

	/* get the number of character cells in the current buffer */
	GetConsoleScreenBufferInfo( hConOut, @csbi )
	dwConSize := (DWORD) (csbi:dwSize:X * csbi:dwSize:Y)

	/* fill the entire screen with blanks */
	FillConsoleOutputCharacter( hConOut, 32, dwConSize, coordScreen, @cCharsWritten )

	/* get the current text attribute */
	GetConsoleScreenBufferInfo( hConOut, @csbi )

	/* now set the buffer's attributes accordingly */
	FillConsoleOutputAttribute( hConOut, csbi:wAttributes, dwConSize, coordScreen, @cCharsWritten )

	/* put the cursor at (0, 0) */
	SetConsoleCursorPosition( hConOut, coordScreen )

	RETURN SELF

ACCESS CursorPos 
	LOCAL sbi IS _winCONSOLE_SCREEN_BUFFER_INFO

	GetConsoleScreenBufferInfo( hConOut, @sbi )

	RETURN ConsoleCoord{ sbi:dwCursorPosition:X, sbi:dwCursorPosition:Y }

ASSIGN CursorPos( oNewPos ) 
	LOCAL dwPos AS DWORD
	
	dwPos := _OR( ( DWORD( _CAST, oNewPos:Y ) << 16 ), DWORD( _CAST, oNewPos:X ) )
	SetConsoleCursorPosition( hConOut, dwPos )

	RETURN 

ACCESS CursorSize 
	LOCAL cci IS _winCONSOLE_CURSOR_INFO

	GetConsoleCursorInfo( hConOut, @cci )

	RETURN cci:dwSize

ASSIGN CursorSize( nNewSize ) 
	LOCAL cci IS _winCONSOLE_CURSOR_INFO

	GetConsoleCursorInfo( hConOut, @cci )
	cci:dwSize := nNewSize
	SetConsoleCursorInfo( hConOut, @cci )

	RETURN 

ACCESS CursorVisible 
	LOCAL cci IS _winCONSOLE_CURSOR_INFO

	GetConsoleCursorInfo( hConOut, @cci )

	RETURN cci:bVisible

ASSIGN CursorVisible( lNewVal ) 
	LOCAL cci IS _winCONSOLE_CURSOR_INFO

	GetConsoleCursorInfo( hConOut, @cci )
	cci:bVisible := lNewVal
	SetConsoleCursorInfo( hConOut, @cci )

	RETURN 

METHOD Destroy( ) 
	FreeConsole( )

	RETURN SELF

METHOD Read( ) 
	LOCAL sBuf AS STRING
	LOCAL dwRead AS DWORD
	LOCAL DIM abTemp[256] AS BYTE

	IF SELF:hConIn != F_ERROR
		dwRead := FRead3( SELF:hConIn,  @abTemp[1], 256 )
		IF dwRead > 2
			sBuf := Mem2String( @abTemp[1], dwRead - 2 )
		ENDIF
	ENDIF

	RETURN sBuf

ACCESS Size 
	LOCAL sbi IS _winCONSOLE_SCREEN_BUFFER_INFO

	GetConsoleScreenBufferInfo( hConOut, @sbi )

	RETURN ConsoleCoord{ sbi:dwSize:X, sbi:dwSize:Y }

ASSIGN Size( oNewSize ) 
	LOCAL sbi 				IS _winCONSOLE_SCREEN_BUFFER_INFO
	LOCAL coordX 			AS DWORD
	LOCAL coordY 			AS DWORD
	LOCAL srWindowRect 	IS _winSMALL_RECT
	LOCAL dwCoordScreen AS DWORD

	GetConsoleScreenBufferInfo( hConOut, @sbi )

	// get the largest size we can size the console window to
	dwCoordScreen := GetLargestConsoleWindowSize( hConOut )

	// define the new console window size and scroll position
	coordX := _AND( dwCoordScreen, 0x0000FFFF )
	coordY := ( dwCoordScreen >> 16 )
	srWindowRect:Right  := Min( oNewSize:X, coordX ) - 1
	srWindowRect:Bottom := Min( oNewSize:Y, coordY ) - 1
	srWindowRect:Left := srWindowRect:Top :=  0

	// define the new console buffer size
	dwCoordScreen  :=  MakeDWord(oNewSize:Y, oNewSize:X)  

	// if the current buffer is larger than what we want, resize the
	// console window first, then the buffer
	IF ( sbi:dwSize:X * sbi:dwSize:Y >  oNewSize:X * oNewSize:Y )
		SetConsoleWindowInfo( hConOut, TRUE, @srWindowRect )
		SetConsoleScreenBufferSize( hConOut, dwCoordScreen )   
		
	ELSEIF ( sbi:dwSize:X * sbi:dwSize:Y <  oNewSize:X * oNewSize:Y )
		// if the current buffer is smaller than what we want, resize the
		// buffer first, then the console window

		SetConsoleScreenBufferSize( hConOut, dwCoordScreen )
		SetConsoleWindowInfo( hConOut, TRUE, @srWindowRect )
	ENDIF

	RETURN 

ACCESS TextAttribute 
	LOCAL sbi IS _winCONSOLE_SCREEN_BUFFER_INFO

	GetConsoleScreenBufferInfo( hConOut, @sbi )

	RETURN sbi:wAttributes

ASSIGN TextAttribute( wNewAttr ) 
	SetConsoleTextAttribute( hConOut, wNewAttr )
	RETURN 

ACCESS Title 
	LOCAL DIM buf[256] AS BYTE

	IF ( GetConsoleTitle( @buf[1], 255 ) > 0 )
		RETURN Psz2String( @buf[1] )
	ENDIF

	RETURN NULL_STRING

ASSIGN Title( sNewTitle ) 

	SetConsoleTitle( String2Psz( sNewTitle ) )

	RETURN 

METHOD @@Wait 
	LOCAL sInput 	IS _WININPUT_RECORD          
	LOCAL sKey		AS _winKEY_EVENT_RECORD
	LOCAL dwEvents	AS DWORD
	LOCAL lReady	AS LOGIC                 
	LOCAL dwOldMode AS DWORD   
	LOCAL cResult	AS STRING
	GetConsoleMode(SELF:hConIn, @dwOldMode)
	SetConsoleMode(SELF:hConIn, ENABLE_MOUSE_INPUT)
	
	DO WHILE .NOT. lReady
		IF (! ReadConsoleInput(SELF:hConIn, @sInput, 1, @dwEvents))
			EXIT
		ENDIF  
		
		DO CASE
		CASE sInput:EventType == KEY_EVENT   
			sKey := (_winKEY_EVENT_RECORD PTR) @sInput:@@Event                 
			// Only respond to Keyup event for Specific Key
			IF (sKey:uChar:AsciiChar != 0 .AND. ! sKey:bKeyDown)
				#IFDEF __DEBUG__
					_DebOut32(String2Psz("Console  Key Event "+NTrim(sKey:uChar:AsciiChar)))
				#ENDIF         
				lReady := TRUE  
				cResult := Chr(sKey:uChar:AsciiChar)
			ENDIF
		CASE sInput:EventType == _MOUSE_EVENT
			IF sInput:@@Event:MouseEvent:dwButtonState != 0
				#IFDEF __DEBUG__
					_DebOut32(String2Psz("Console Mouse Button Event"))
				#ENDIF
				lReady := TRUE
				cResult := Chr(128)
			ENDIF
		CASE sInput:EventType == WINDOW_BUFFER_SIZE_EVENT		
			#IFDEF __DEBUG__
				_DebOut32(String2Psz("Console  Window Buffer Event"))
			#ENDIF
		CASE sInput:EventType == FOCUS_EVENT		
			#IFDEF __DEBUG__
				_DebOut32(String2Psz("Console Focus Event"))
			#ENDIF
		CASE sInput:EventType == MENU_EVENT
			#IFDEF __DEBUG__
				_DebOut32(String2Psz("Console MENU Event"))
			#ENDIF
		OTHERWISE
			// What else
			#IFDEF __DEBUG__
				_DebOut32(String2Psz("Console Event (Unknown) "+NTrim(sInput:EventType)))
			#ENDIF
		ENDCASE
	ENDDO
	SetConsoleMode(SELF:hConIn, dwOldMode)
	SELF:Write(cResult)
	RETURN cResult	
	

METHOD Write( sMsg ) 
	LOCAL lRet AS LOGIC
	LOCAL n AS DWORD

	IF ( hConOut != F_ERROR )
		n := SLen( sMsg )
		lRet := ( FWrite( SELF:hConOut, sMsg, n ) == n )
	ENDIF

	#IFDEF __DEBUG__
//		IF ! lRet
//			SELF:WriteError( "Erite Error: " + __GetLastErrorMsg( GetLastError( ) ) )
//		ENDIF
	#ENDIF

	RETURN lRet

METHOD WriteError( sMsg ) 
	LOCAL lRet AS LOGIC
	LOCAL n AS DWORD

	IF ( SELF:hConErr != F_ERROR )
		n := SLen( sMsg )
		lRet := ( FWrite( SELF:hConErr, sMsg, n ) == n )
	ENDIF

	RETURN lRet

METHOD WriteLine( sMsg ) 
	RETURN SELF:Write( sMsg + _CHR( 13 ) + _CHR( 10 ) )



END CLASS

