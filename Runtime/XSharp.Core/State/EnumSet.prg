//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp
USING XSharp.RDD.Enums
USING System.Collections.Generic
BEGIN NAMESPACE XSharp
    /// <summary>Values that match the Visual Objects SET_* defines </summary>
    /// <remarks>Global settings are stored in the RuntimeState and are thread specific.
    /// The numeric value of these sets indicate the key of the setting in the settings dictionary on the runtimestate.
    /// </remarks>
    /// <seealso cref='RuntimeState'>RuntimeState</seealso>
    /// <seealso cref='RuntimeState.GetValue``1(XSharp.Set)'>RuntimeState.GetValue</seealso>
    /// <seealso cref='RuntimeState.SetValue``1(XSharp.Set,``0)'>RuntimeState.SetValue</seealso>
    ENUM Set
        /// <summary>Specifies rules that Visual FoxPro uses when comparing two strings of different lengths.</summary>
        MEMBER Exact       := 1			// LOGIC
        /// <summary>Specifies if the number of decimal places used in the display of numeric data is fixed.</summary>
        MEMBER Fixed	   := 2			// LOGIC
        /// <summary>Specifies the number of decimal places displayed in numeric expressions.</summary>
        MEMBER Decimals    := 3			// INT
        /// <summary>Specifies the format for the display of Date and DateTime expressions.</summary>
        MEMBER DateFormat  := 4			// STRING
        /// <summary>Change the setting that determines how dates without century digits are interpreted.</summary>
        MEMBER Epoch       := 5			// INT
        /// <summary>Specifies a path for file searches.</summary>
        MEMBER Path        := 6			// STRING
        /// <summary>Specifies the default drive and directory.</summary>
        MEMBER @@Default   := 7			// STRING
        /// <summary>Specifies whether Visual FoxPro opens table files for exclusive or shared use on a network.</summary>
        MEMBER Exclusive   := 8			// LOGIC
        /// <summary>Determines where the record pointer is positioned after FIND or SEEK unsuccessfully searches for a record.</summary>
        MEMBER Softseek    := 9			// LOGIC
        /// <summary>Specifies whether records with duplicate index key values are maintained in an index file.</summary>
        MEMBER Unique      := 10		// LOGIC
        /// <summary>Specifies whether Visual FoxPro processes records marked for deletion and whether they are available for use with other commands.</summary>
        MEMBER Deleted     := 11		// LOGIC
        /// <summary>--- not used ---</summary>
        MEMBER Cancel      := 12		// LOGIC
        /// <summary>--- not used ---</summary>
        MEMBER @@Debug     := 13
        /// <summary>--- not used ---</summary>
        MEMBER Typeahead   := 14		// INT
        /// <summary>Sets the colors used on the console output</summary>
        MEMBER Color       := 15		// STRING
        /// <summary>Determines whether the insertion point is displayed when Visual FoxPro waits for input.</summary>
        MEMBER Cursor      := 16		// INT
        /// <summary>Enables or disables output to the main Visual FoxPro window or to the active user-defined window from within programs.</summary>
        MEMBER Console     := 17		// LOGIC
        /// <summary>Directs screen or printer output created with ?, ??, DISPLAY, or LIST to a text file.</summary>
        MEMBER Alternate   := 18		// LOGIC
        /// <summary>Output filename for the Alternate file.</summary>
        MEMBER AltFile     := 19		// STRING
        /// <summary>Directs output from @ ... SAY to the screen, a printer, or a file.</summary>
        MEMBER Device      := 20		// STRING
        // 21 and 22 missing
        /// <summary>Enables or disables output to the printer or routes output to a file, port, or network printer. </summary>
        MEMBER Printer     := 23		// LOGIC
        /// <summary>Output filename for printer output.</summary>
        MEMBER PrintFile   := 24		// STRING
        /// <summary>Sets the left printer margin and affects all output directed to the printer.</summary>
        MEMBER Margin      := 25		// INT
        /// <summary>Turns the computer bell on or off and sets the bell attributes.</summary>
        MEMBER Bell        := 26		// LOGIC
        /// <summary>Specifies whether the user can exit a text box by typing past the last character in the text box.</summary>
        MEMBER Confirm     := 27		// LOGIC
        /// <summary>Determines whether pressing the ESC key interrupts program and command execution.</summary>
        MEMBER Escape      := 28		// LOGIC
        /// <summary>--- not used ---</summary>
        MEMBER Insert      := 29		// LOGIC
        /// <summary>--- not used ---</summary>
        MEMBER Exit        := 30		// LOGIC
        /// <summary>--- not used ---</summary>
        MEMBER Intensity   := 31		// LOGIC
        /// <summary>--- not used ---</summary>
        MEMBER Scoreboard  := 32		// LOGIC
        /// <summary></summary>
        MEMBER Delimiters  := 33		// STRING
        /// <summary>--- not used ---</summary>
        MEMBER DelimChars  := 34		// STRING
        /// <summary>--- not used ---</summary>
        MEMBER Wrap        := 35		// LOGIC
        /// <summary>--- not used ---</summary>
        MEMBER Message     := 36		// INT
        /// <summary>--- not used ---</summary>
        MEMBER Mcenter     := 37		// LOGIC
        /// <summary>--- not used ---</summary>
        MEMBER ScrollBreak := 38		// LOGIC
        /// <summary>--- not used ---</summary>
        MEMBER Errorlog    := 39		// LOGIC
        /// <summary></summary>
        MEMBER Neterr      	:= 40	// LOGIC
        /// <summary>The setting that determines the number of digits that will be shown to the left of the decimal point when a number is displayed.</summary>
        MEMBER Digits      	:= 41	// INT
        /// <summary>A string representing the morning extension for time strings in 12-hour format.</summary>
        MEMBER AmExt		:= 42	// STRING
        /// <summary>A string representing the evening extension for time strings in 12-hour format.</summary>
        MEMBER PmExt	    := 43	// STRING
        /// <summary>The setting that determines whether database files are created using ANSI or OEM format and whether certain text file operations convert between the two character sets.</summary>
        MEMBER Ansi      	:= 44	// LOGIC
        /// <summary></summary>
        MEMBER Yield     	:= 45	// LOGIC
        /// <summary>The number of times certain lock operations are retried</summary>
        MEMBER Locktries   	:= 46	// INT
        /// <summary>The setting that determines whether time strings are in 12-hour or 24-hour format.</summary>
        MEMBER AmPm		    := 47	// LOGIC
        /// <summary>Determines whether the century portion of date expressions is displaid.</summary>
        MEMBER Century	    := 48	// LOGIC
        /// <summary>The setting that fixes the number of digits used to display numeric output.</summary>
        MEMBER DigitFixed  	:= 49	// LOGIC
        /// <summary>Determines the decimal point character used in the display of numeric and currency expressions.</summary>
        MEMBER DecimalSep  	:= 50	// DWORD
        /// <summary>Specifies the character that separates each group of three digits to the left of the decimal point when displaying a formatted numeric or currency value. </summary>
        MEMBER ThousandSep 	:= 51	// DWORD
        /// <summary>The current separation character used in time strings.</summary>
        MEMBER Timesep     	:= 52	// DWORD
        /// <summary>The setting that determines whether assignments are made to fields or to memory variables.</summary>
        MEMBER Fieldstore  	:= 53   // Logic
        /// <summary>The setting that controls if numbers are displaid in scientific notation.</summary>
        MEMBER Science     	:= 54	// LOGIC
        /// <summary></summary>
        MEMBER Cpu			:= 55	// INT
        /// <summary>The setting that determines the point at which 2 floating point numbers would be considered equal even though they are different.</summary>
        MEMBER Floatdelta	:= 56	// System.Double
        /// <summary></summary>
        MEMBER Math			:= 57	// INT
        /// <summary></summary>
        MEMBER International:= 58	// STRING
        /// <summary>Specifies the format for the display of Date and DateTime expressions.</summary>
        MEMBER DateCountry  := 59	// INT
        /// <summary></summary>
        MEMBER DefaultDir   := 60   // STRING location of error log file

        // X# helper state
        /// <summary>Determines how Microsoft Visual FoxPro interprets dates that specify only 2 digit years.</summary>
        MEMBER EpochCent     := 70		// Numeric
        /// <summary>Determines how Microsoft Visual FoxPro interprets dates that specify only 2 digit years.</summary>
        MEMBER EpochYear     := 71		// Numeric
        /// <summary>The current dateformat in .Net format.</summary>
        MEMBER DateFormatNet := 72		// String
        /// <summary>The empty representation of the current date format.</summary>
        MEMBER DateFormatEmpty := 73    // String
        /// <summary>SysObject value</summary>
        MEMBER SysObject := 74
        // 75 unused
        /// <summary>The last method called with a late bound send operator that was not found in the object that was used.</summary>
        MEMBER NoMethod		:= 76	// STRING
        // 77 unused
        /// <summary>The current path setting as an array of strings.</summary>
        MEMBER Patharray    := 78	// String[]
        /// <summary>Determines the name of the current Nation DLL.</summary>
        MEMBER NatDLL		:= 79   // string
        /// <summary>Determines the collation array read from the current Nation DLL</summary>
        MEMBER CollationTable := 80  // byte[]
        /// <summary>--- not used ---</summary>
        MEMBER ErrorLevel   := 81  // DWORD
        /// <summary>The codeblock set for the current error handler.</summary>
        MEMBER ErrorBlock   := 82  // Codeblock
        // <summary>The last error that occurred for a RDD operation.</summary>
        //MEMBER LastRddError := 84   // Exception object
        /// <summary>The last script error that occurred.</summary>
        MEMBER LastScriptError := 85   // Exception object
        /// <summary>The last file found with File()</summary>
        MEMBER LastFound    := 86   // Last file found with File()
        // <summary>The last File error code</summary>
        //MEMBER FileError    := 87   // Last File error code
        // <summary>The last File exception</summary>
        //MEMBER FileException:= 88   // Last File exception

        /// <summary>Determines the name of the current Delim RDD.</summary>
        MEMBER DelimRDD         := 89
        /// <summary>Determines the current field delimiter for Delim RDDs</summary>
        MEMBER FieldDelimiter   := 90
        /// <summary>Determines the current record delimiter for Delim RDDs</summary>
        MEMBER RecordDelimiter  := 91
        /// <summary>Name of the error log file from the default RT error handler. Defaults to VOERROR.LOG</summary>
        MEMBER ErrorLogFile     := 92
        // 92 - 97 unused
        /// <summary></summary>
        MEMBER Dict        := 98	// LOGIC
        /// <summary></summary>
        MEMBER Intl        := 99	// CollationMode

        // Vulcan RDDInfo Settings
        /// <summary></summary>
        MEMBER RddInfo		:= 100      // no value
        /// <summary>Specifies how X# allocates disk space for the storage of memo fields.
        /// This is 64 for the FoxPro dialect and 32 for other dialects.</summary>
        MEMBER MemoBlockSize:= 101		// INT
        /// <summary>Determines the current Default RDD. This is DBFVFP for the FoxPro dialect and
        /// DBFNTX for the other dialects.</summary>
        MEMBER DefaultRdd	:= 102		// STRING
        /// <summary>Determines the current default Memofile extension.</summary>
        MEMBER MemoExt	    := 103		// STRING
        /// <summary>Determines if production CDX files should be automatically opened. Defaults to TRUE</summary>
        MEMBER AutoOpen     := 104		// LOGIC
        /// <summary>Determines if the first index in the production CDX should be selected. Defaults to 0 for the FoxPro dialect and 1 for the other dialects.</summary>
        MEMBER AutoOrder    := 105		// 0 or 1
        /// <summary>Should High Performance NTX Locking be used for DBFNTX files.</summary>
        MEMBER HpLocking    := 106      // LOGIC
        /// <summary>Should the New Index Locking Scheme be used for DBFNTX files.</summary>
        MEMBER NewIndexLock := 107      // LOGIC
        /// <summary></summary>
        MEMBER Autoshare    := 108		// 0 or 1
        /// <summary></summary>
        MEMBER StrictRead   := 109		// LOGIC
        /// <summary></summary>
        MEMBER BlobCircref	:= 110		// LOGIC
        /// <summary>Enables or disables Query Optimization.</summary>
        MEMBER Optimize     := 111		// LOGIC
        /// <summary>Should the FoxPro locking scheme be used for DBFCDX and DBFVFP files.</summary>
        MEMBER FoxLock      := 112		// LOGIC

        /// <summary>Determines the maximum number of RDD related settings.</summary>
        MEMBER RddInfoMax   := 119      // no value

        /// <summary>Determines the current Windows CodePage number</summary>
        MEMBER WinCodepage	:= 120		// Numeric
        /// <summary>Determines the current DOS/OEM CodePage number</summary>
        MEMBER DosCodepage	:= 121		// Numeric
        /// <summary>Determines the current Collation mode (Windows, Clipper, Ordinal, Unicode, XPP)</summary>
        MEMBER CollationMode:= 122		// CollationMode

        // 123 and 124 reserved
        // FoxPro settings 125 - 159, last used 145
        // Some settings are IDE specific, such as AutoSave, BrowseIME, Clock etc.
        // Others are windows regional settings, such as first day of week and currency symbol
        // these are not implemented
        /// <summary>Specifies whether to pad a shorter string with spaces when making a SQL string comparison or binary expression with zero (0) bytes when making a binary expression comparison in SQL commands using the equal sign operator (=).</summary>
        MEMBER Asserts          := 125 // Logic
        /// <summary>Specifies whether attempts to update or insert values in a field with automatically incrementing values generate errors.</summary>
        MEMBER AutoIncError     := 126 // Logic
        //MEMBER AutoSave
        //MEMBER BrowseIME
        //MEMBER Carry
        //MEMBER Clock
        //MEMBER ClockRowCol
        //MEMBER ColorScheme
        //MEMBER ColorSet
        /// <summary>Specifies a collation sequence for character fields in subsequent indexing and sorting operations. Defaults to MACHINE</summary>
        MEMBER CollateFox       := 127
        /// <summary>Controls compatibility with Microsoft FoxBASE+ and other FoxPro languages.</summary>
        MEMBER Compatible       := 128
        // MEMBER Coverage
        // MEMBER CoverageFile
        // MEMBER CpCompile
        // MEMBER CpDialog
        // MEMBER Currency
        // MEMBER CurrencySymbol
        /// <summary>Sets an open database as the current database or sets no current database.</summary>
        MEMBER Database         := 129 // string
        /// <summary></summary>
        MEMBER DataSession      := 130 // Numeric
        /// <summary>Directs debugging output to a file.</summary>
        MEMBER DebugOut         := 131 // string - filename
        /// <summary>Specifies the default drive and directory.</summary>
        MEMBER Directory        := Set.Default         // FoxPro alias
        // MEMBER Dohistory
        // MEMBER Echo
        // MEMBER EngineBehavior
        // MEMBER EventList
        // MEMBER EventTracking
        // MEMBER Fdow
        // MEMBER Fields            => not in Set() but Db.. function
        // MEMBER FieldsList        => not in Set() but Db.. function
        // MEMBER Format
        /// <summary>FoxPro: Should CDX(),DBF() and similar functions return full paths</summary>
        MEMBER FullPath         := 132      // Logic
        // MEMBER Function
        // MEMBER FWeek
        MEMBER Headings         := 144
        // MEMBER Help
        // MEMBER Hours
        // MEMBER Intensity
        // MEMBER Key
        // MEMBER KeyComp
        // MEMBER Libary
        /// <summary>Enables or disables automatic file locking in certain commands.</summary>
        MEMBER Lock             := 133
        // MEMBER LogErrors
        // MEMBER MacKey
        // MEMBER MarkOf
        // MEMBER MarkTo
        /// <summary>Specifies the displayed width of memo fields and character expressions.Default is 50</summary>
        MEMBER MemoWidth        := 134
        /// <summary>Determines whether you can lock multiple records using LOCK( ) or RLOCK( ). Default is OFF</summary>
        MEMBER MultiLocks       := 135
        /// <summary>Determines where the record pointer is positioned after FIND or SEEK unsuccessfully searches for a record.</summary>
        MEMBER Near             := Set.Softseek
        // MEMBER NoCpTrans        // not needed: Unicode
        // MEMBER Notify
        /// <summary>Determines how null values are supported by the ALTER TABLE, CREATE TABLE and INSERT - SQL commands.</summary>
        MEMBER Null             := Set.NullValue
        // MEMBER NullDisplay
        // MEMBER Odometer
        // MEMBER OleObject
        // MEMBER Palette
        // MEMBER PdSetup
        /// <summary>Determines the decimal point character used in the display of numeric and currency expressions.</summary>
        MEMBER Point            := Set.DecimalSep
        // MEMBER Procedure
        // MEMBER ReadBorder
        /// <summary>Determines whether to and how frequently to update local memory buffers with changes from other users on the network.</summary>
        MEMBER Refresh          := 136   // refresh time for local cache of shared data
        //MEMBER ReportBehavior
        /// <summary>Specifies how many times and for how long Visual FoxPro attempts to lock a file or record after an unsuccessful locking attempt.</summary>
        MEMBER Reprocess        := 137
        //MEMBER Resource
        /// <summary>Determines whether Visual FoxPro displays a dialog box before overwriting an existing file.</summary>
        MEMBER Safety           := 138  // Logic
        // MEMBER Seconds
        /// <summary>Specifies the character that separates each group of three digits to the left of the decimal point when displaying a formatted numeric or currency value. </summary>
        MEMBER Separator        := Set.ThousandSep
        // MEMBER Skip          // SelectiveRelation
        // MEMBER SkipOf
        //
        /// <summary>FoxPro: Add Space between ? and ?? field expressions</summary>
        MEMBER Space            := 139    // Logic
        /// <summary>Specifies if data in a SQL - SELECT statement is based on buffered data or data written to disk.</summary>
        MEMBER SqlBuffering     := 140    // Logic
        // MEMBER StatusBar
        // MEMBER Status
        /// <summary>Specifies whether to pad a shorter string with spaces when making a SQL string comparison or binary expression with zero (0) bytes when making a binary expression comparison in SQL commands using the equal sign operator (=).</summary>
        MEMBER SqlAnsi          := 141
        // MEMBER Step
        // MEMBER StrictDate
        // MEMBER SysFormats
        // MEMBER SysMenu
        // MEMBER TablePrompt
        // MEMBER TableValidate
        // MEMBER Talk
        //
        /// <summary>FoxPro: Is Textmerge enabled.</summary>
        MEMBER TextMerge        := 142      // Logic
        MEMBER VarCharMapping   := 143    // Logic
        MEMBER TextMergeDelimiters := 146 // string[]
        // MEMBER Topic
        // MEMBER TopicID
        // MEMBER TrBetween
        // MEMBER UdfParams
        /// <summary>Specifies how character data expressions are mapped to query result sets.</summary>
        MEMBER WithStack        := 145

        // Xbase++ defines
        MEMBER CharSet          := 160
        MEMBER HandleEvent      := 161
        MEMBER DevTimeOut       := 162
        MEMBER Accelerators     := 163
        MEMBER Colormode        := 164
        // Optimize already defined
        MEMBER Rushmore         := 165
        MEMBER SmartFilter      := 166
        MEMBER NullValue        := 167
        MEMBER Collation        := 168  // XPP Collation Number
        MEMBER Lexical          := 169  // Not implemented




        // 180 - 197 Harbour extensions, Most have  No defaults below yet
        // Originally these started at 100
        MEMBER Language       :=  180               // STRING
        MEMBER IdleRepeat     :=  181               // Numeric Ignored for now
        MEMBER FileCase       :=  182			    // Numeric Ignored for now
        MEMBER DirCase        :=  183               // Numeric Ignored for now
        MEMBER DirSeparator   :=  184               // String
        MEMBER Eof            :=  185               // Logic: Is Chr(26) written to end of text files
        MEMBER HardCommit     :=  186               // Logic: Forces Hard Commit in RDD system (whatever that me be..)
        MEMBER ForceOpt       :=  187               // LOGIC: Force Optimization
        MEMBER DbfLockscheme  :=  188               //
        MEMBER Defextensions  :=  189               // Logic: Force Extensions for RDD and other output files. Not used yet
        MEMBER Eol            :=  190               // ENd of Line characters
        MEMBER Trimfilename   :=  191	            // Logic: Should filenames be trimmed in the IO system
        MEMBER Hboutlog       :=  192               // STRING LogfileName
        MEMBER Hboutloginfo   :=  193               // String Info written to error log files
        MEMBER Codepage       :=  WinCodepage		// Remapped
        MEMBER Oscodepage     :=  DosCodepage	    // Remapped
        MEMBER Timeformat     :=  196
        MEMBER Dbcodepage     :=  197				// Map to Vulcan setting ?

        // Start of user values
        MEMBER User           := 200
        // Advantage extensions
        MEMBER Axslocking           := User+1
        MEMBER Rightschecking       := User+2
        MEMBER Connection_handle    := User+3
        MEMBER Exactkeypos          := User+4
        MEMBER Sql_query            := User+5
        MEMBER Sql_table_passwords  := User+6
        MEMBER Collation_name       := User+7
        MEMBER Sql_timeout          := User+8
        MEMBER Sql_parameters       := User+9

    END ENUM
END NAMESPACE
#region Defines
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EXACT       := Set.Exact
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FIXED       := Set.Fixed
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DECIMALS    := Set.Decimals
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DATEFORMAT  := Set.DateFormat
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EPOCH       := Set.Epoch
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_PATH        := Set.Path
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEFAULT     := Set.Default

/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EXCLUSIVE   := Set.Exclusive
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SOFTSEEK    := Set.Softseek
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_UNIQUE      := Set.Unique
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DELETED     := Set.Deleted

/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_Cancel      := Set.Cancel
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEBUG       := Set.Debug
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_TYPEAHEAD   := Set.Typeahead

/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_COLOR       := Set.Color
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CURSOR      := Set.Cursor
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CONSOLE     := Set.Console
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_ALTERNATE   := Set.Alternate
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_ALTFILE     := Set.AltFile
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEVICE      := Set.Device
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_PRINTER     := Set.Printer
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_PRINTFILE   := Set.PrintFile
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MARGIN      := Set.Margin

/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_BELL        := Set.Bell
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CONFIRM     := Set.Confirm
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_ESCAPE      := Set.Escape
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_INSERT      := Set.Insert
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EXIT        := Set.Exit
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_INTENSITY   := Set.Intensity
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SCOREBOARD  := Set.Scoreboard
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DELIMITERS  := Set.Delimiters
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DELIMCHARS  := Set.DelimChars

/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_WRAP        := Set.Wrap
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MESSAGE     := Set.Message
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MCENTER     := Set.Mcenter
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SCROLLBREAK := Set.ScrollBreak

// 48 and 49 unused
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DIGITS      	:= Set.Digits
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_NETERR      	:= Set.Neterr
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_ANSI      		:= Set.Ansi
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_YIELD     		:= Set.Yield
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_LOCKTRIES   	:= Set.Locktries
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AMEXT			:= Set.AmExt
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AMPM			:= Set.AmPm
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_PMEXT	    	:= Set.PmExt
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CENTURY	    	:= Set.Century
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DIGITFIXED  	:= Set.DigitFixed
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DECIMALSEP  	:= Set.DecimalSep
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_THOUSANDSEP 	:= Set.ThousandSep
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_TIMESEP     	:= Set.Timesep
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FIELDSTORE  	:= Set.Fieldstore
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SCIENCE     	:= Set.Science
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CPU				:= Set.Cpu
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FLOATDELTA		:= Set.Floatdelta
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MATH			:= Set.Math
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_INTERNATIONAL	:= Set.International
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DATECOUNTRY		:= Set.DateCountry
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DICT			:= Set.Dict
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_INTL			:= Set.Intl

/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEFAULTDIR      := Set.DefaultDir



/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DELIMRDD         := Set.DelimRDD
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FIELDDELIMITER   := Set.FieldDelimiter
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_RECORDDELIMITER  := Set.RecordDelimiter


/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_BLOB_CIRCULAR_ARRAY_REF := Set.BlobCircref
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_USER := Set.User


/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_COLLATIONMODE	:= Set.CollationMode

// Vulcan RDDInfo Settings
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_RDDINFO				:= Set.RddInfo
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MEMOBLOCKSIZE		:= Set.MemoBlockSize
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEFAULTRDD			:= Set.DefaultRdd
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MEMOEXT	    		:= Set.MemoExt
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AUTOOPEN    		:= Set.AutoOpen
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AUTOORDER   		:= Set.AutoOrder
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_HPLOCKING   		:= Set.HpLocking
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_HPLOCK      		:= Set.HpLocking
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_NEWINDEXLOCK		:= Set.NewIndexLock
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AUTOSHARE   		:= Set.Autoshare
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_STRICTREAD  		:= Set.StrictRead
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_BLOBCIRCREF			:= Set.BlobCircref
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_OPTIMIZE    		:= Set.Optimize
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FOXLOCK     		:= Set.FoxLock
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_WINCODEPAGE			:= Set.WinCodepage
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DOSCODEPAGE			:= Set.DosCodepage

// Harbour extensions
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_LANGUAGE       :=  Set.Language
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_IDLEREPEAT     :=  Set.IdleRepeat
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FILECASE       :=  Set.FileCase
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DIRCASE        :=  Set.DirCase
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DIRSEPARATOR   :=  Set.DirSeparator
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EOF            :=  Set.Eof
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_HARDCOMMIT     :=  Set.HardCommit
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FORCEOPT       :=  Set.ForceOpt
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DBFLOCKSCHEME  :=  Set.DbfLockscheme
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEFEXTENSIONS  :=  Set.Defextensions
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EOL            :=  Set.Eol
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_TRIMFILENAME   :=  Set.Trimfilename
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_HBOUTLOG       :=  Set.Hboutlog
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_HBOUTLOGINFO   :=  Set.Hboutloginfo
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CODEPAGE       :=  Set.Codepage
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_OSCODEPAGE     :=  Set.Oscodepage
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_TIMEFORMAT     :=  Set.Timeformat
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DBCODEPAGE     :=  Set.Dbcodepage

// Advantage additions
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AXSLOCKING           := Set.Axslocking
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_RIGHTSCHECKING       := Set.Rightschecking
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CONNECTION_HANDLE    := Set.Connection_handle
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EXACTKEYPOS          := Set.Exactkeypos
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SQL_QUERY            := Set.Sql_query
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SQL_TABLE_PASSWORDS  := Set.Sql_table_passwords
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_COLLATION_NAME       := Set.Collation_name
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SQL_PARAMETERS       :=  Set.Sql_parameters
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SQL_TIMEOUT          :=  Set.Sql_timeout


// Xbase++ additions
DEFINE  _SET_CHARSET     := Set.CharSet
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_HANDLEEVENT := Set.HandleEvent
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_DEVTIMEOUT  := Set.DevTimeOut
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_ACCELERATORS := Set.Accelerators
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_COLORMODE    := Set.Colormode
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_RUSHMORE    := Set.Rushmore
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_SMARTFILTER := Set.SmartFilter
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_NULLVALUE   := Set.NullValue

/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_COLLATION   := Set.Collation
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_LEXICAL    := Set.Lexical


// FoxPro defines
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_ASSERTS         := Set.Asserts
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AUTOINCERROR    := Set.AutoIncError
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_COLLATEFOX := Set.CollateFox
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DATABASE        := Set.Database
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DATASESSION     := Set.DataSession
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEBUGOUT        := Set.DebugOut
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FULLPATH        := Set.FullPath
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_LOCK            := Set.Lock
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MEMOWIDTH       := Set.MemoWidth
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MULTILOCKS      := Set.MultiLocks
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_NULL             := Set.Null
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_POINT            := Set.Point
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_REFRESH          := Set.Refresh
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_REPROCESS        := Set.Reprocess
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SAFETY           := Set.Safety
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SEPARATOR        := Set.Separator
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SPACE           := Set.Space
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SQLANSI         := Set.SqlAnsi
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SQLBUFFERING     := Set.SqlBuffering
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_TEXTMERGE        := Set.TextMerge
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_VARCHARMAPPING    := Set.VarCharMapping


#endregion


DEFINE _MAX_PATH := 260
DEFINE MAX_PATH := 260

INTERNAL FUNCTION RuntimeStateDefaultValue(nSet AS XSharp.Set) AS OBJECT
    SWITCH nSet
        CASE Set.Exact
        CASE Set.Fixed
        CASE Set.Softseek
        CASE Set.Unique
        CASE Set.Deleted
        CASE Set.Cancel
        CASE Set.Debug
        CASE Set.Alternate
        CASE Set.Printer
        CASE Set.Confirm
        CASE Set.Escape
        CASE Set.Insert
        CASE Set.Exit
        CASE Set.Intensity
        CASE Set.Scoreboard
        CASE Set.Wrap
        CASE Set.Mcenter
        CASE Set.ScrollBreak
        CASE Set.Errorlog
        CASE Set.Yield
        CASE Set.Neterr
        CASE Set.AmPm
        CASE Set.Century
        CASE Set.DigitFixed
        CASE Set.Fieldstore
        CASE Set.Science
        CASE Set.Dict
        CASE Set.HpLocking
        CASE Set.NewIndexLock
        CASE Set.StrictRead
        CASE Set.BlobCircref
        CASE Set.FoxLock
        CASE Set.HandleEvent
        CASE Set.Rushmore
        CASE Set.SmartFilter
        CASE Set.NullValue
        CASE Set.Lexical
        // FoxPro
        CASE Set.Asserts
        CASE Set.Lock
        CASE Set.MultiLocks
        CASE Set.SqlAnsi
        CASE Set.SqlBuffering
        CASE Set.VarCharMapping
		    RETURN FALSE

        CASE Set.Ansi
        CASE Set.Bell
        CASE Set.Console
        CASE Set.Exclusive
        CASE Set.Optimize
        CASE Set.AutoOpen
        CASE Set.Defextensions
        CASE Set.ForceOpt
        CASE Set.Trimfilename
        // FoxPro
        CASE Set.AutoIncError
        CASE Set.FullPath
        CASE Set.Safety
        CASE Set.Space
        CASE Set.TextMerge
        CASE Set.HardCommit
        	RETURN TRUE

        CASE Set.TextMergeDelimiters
            return <string>{"<<",">>"}

       CASE Set.DirCase
       CASE Set.FileCase
//            //#define HB_SET_CASE_MIXED  0
//            //#define HB_SET_CASE_LOWER  1
//            //#define HB_SET_CASE_UPPER  2
//            SWITCH System.PlatformID
//            CASE PlatformID.Unix
//            CASE PlatformID.MacOSX
//            CASE PlatformID.Win32NT
//            CASE PlatformID.Win32S
//            CASE PlatformID.Win32Windows
//            CASE PlatformID.WinCE
//            CASE PlatformID.Xbox
//            END SWITCH
            RETURN 0
        CASE Set.Typeahead
        CASE Set.Cursor
        CASE Set.Margin
        CASE Set.Message
        CASE Set.Cpu
        CASE Set.Math
        CASE Set.CharSet
        CASE Set.DevTimeOut
        CASE Set.Colormode
        CASE Set.Collation
        CASE Set.IdleRepeat
            RETURN 0L

        CASE Set.Compatible
            RETURN "OFF"

        CASE Set.AutoOrder
            RETURN 1L

        CASE Set.DateCountry
            RETURN DateCountry.American

        CASE Set.Autoshare
            RETURN AutoShareMode.Auto

        CASE Set.Color
           RETURN "W/N,N/W,N/N,N/N,N/W"
        CASE Set.DateFormat
            RETURN "MM/DD/YYYY"

        CASE Set.DefaultRdd
            RETURN "DBFNTX"

        CASE Set.Eol
            RETURN e"\r\n"

        CASE Set.Hboutlog
            RETURN "hb_out.log"

        CASE Set.Hboutloginfo
        CASE Set.Path
        CASE Set.AltFile
        CASE Set.Device
        CASE Set.PrintFile
        CASE Set.DelimChars
        CASE Set.AmExt
        CASE Set.PmExt
        CASE Set.International
        CASE Set.NatDLL
        CASE Set.NoMethod
        CASE Set.DateFormatNet
        CASE Set.DateFormatEmpty
        CASE Set.LastFound
        CASE Set.MemoExt
        CASE Set.Language
        CASE Set.Timeformat
        // FoxPro
        CASE Set.Database
        CASE Set.DebugOut
            RETURN String.Empty
        CASE Set.Default
        CASE Set.DefaultDir
            RETURN ""

        CASE Set.DirSeparator
            RETURN System.IO.Path.DirectorySeparatorChar:ToString()

        CASE Set.Floatdelta
            RETURN 0.0000000000001


        CASE Set.Patharray     // String[]
            RETURN (STRING[]) NULL
        CASE Set.CollationTable   // byte[]
            RETURN (BYTE[] ) NULL

        CASE Set.DecimalSep
        CASE Set.ThousandSep
        CASE Set.Timesep
        CASE Set.EpochYear
        //CASE Set.FileError
        CASE Set.ErrorLevel     // DWORD
             RETURN 0U
        CASE Set.Decimals
             RETURN 2U

        CASE Set.Digits
        CASE Set.Locktries
             RETURN 10U

        CASE Set.MemoBlockSize
            RETURN (WORD) 32

        CASE Set.Epoch
        CASE Set.EpochCent
             RETURN 1900U

        CASE Set.ErrorBlock     // Codeblock
            RETURN NULL

        CASE Set.LastScriptError    // Exception object
        CASE Set.SysObject
        CASE Set.RddInfo
            RETURN NULL
        CASE Set.WithStack
            RETURN Stack<OBJECT>{}
        CASE Set.Intl
        CASE Set.CollationMode
            RETURN CollationMode.Windows

        CASE Set.ErrorLogFile
            RETURN "ERROR.LOG"


        CASE Set.DosCodepage
            RETURN 437L     // US American

        CASE Set.WinCodepage
            RETURN 1252L    // Latin 1 / Western Europe

        CASE Set.FieldDelimiter
            RETURN ","

        CASE Set.Delimiters
            RETURN e"\""

        CASE Set.RecordDelimiter
            RETURN CRLF

        CASE Set.DelimRDD
            RETURN "DELIM"

        // 180 - 197 Harbour extensions, no value yet
//        MEMBER FILECASE       :=  182
//        MEMBER DIRCASE        :=  183
//        MEMBER DBFLOCKSCHEME  :=  188
//        MEMBER DBCODEPAGE     :=  197				// Map to Vulcan setting ?


        // Advantage
        CASE Set.Axslocking
        CASE Set.Rightschecking
        CASE Set.Exactkeypos
            RETURN TRUE

        CASE Set.Sql_query
        CASE Set.Collation_name
            RETURN String.Empty
        CASE Set.Connection_handle
        CASE Set.Sql_table_passwords
        CASE Set.Sql_parameters
            RETURN NULL
        CASE Set.Sql_timeout
            RETURN 0

        // FoxPro
        CASE Set.CollateFox
            RETURN "MACHINE"
        CASE Set.DataSession
            RETURN 1
        CASE Set.MemoWidth
            RETURN 50
        CASE Set.Refresh
            RETURN 5.0
        CASE Set.Reprocess
            RETURN 0


END SWITCH



RETURN NULL


