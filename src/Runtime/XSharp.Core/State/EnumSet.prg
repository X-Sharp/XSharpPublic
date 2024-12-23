//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp
USING XSharp.RDD.Enums
USING System.Collections.Generic
BEGIN NAMESPACE XSharp
/// <include file="XSharp.CoreDefines.xml" path="members/Set/*" />
/// <seealso cref='RuntimeState'>RuntimeState</seealso>
/// <seealso cref='RuntimeState.GetValue``1(XSharp.Set)'>RuntimeState.GetValue</seealso>
/// <seealso cref='RuntimeState.SetValue``1(XSharp.Set,``0)'>RuntimeState.SetValue</seealso>

ENUM Set
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Exact/*" />
    MEMBER Exact       := 1			// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Fixed/*" />
    MEMBER Fixed	   := 2			// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Decimals/*" />
    MEMBER Decimals    := 3			// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DateFormat/*" />
    MEMBER DateFormat  := 4			// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Epoch/*" />
    MEMBER Epoch       := 5			// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Path/*" />
    MEMBER Path        := 6			// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Default/*" />
    MEMBER @@Default   := 7			// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Exclusive/*" />
    MEMBER Exclusive   := 8			// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.SoftSeek/*" />
    MEMBER Softseek    := 9			// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Unique/*" />
    MEMBER Unique      := 10		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Deleted/*" />
    MEMBER Deleted     := 11		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    MEMBER Cancel      := 12		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    MEMBER @@Debug     := 13
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    MEMBER Typeahead   := 14		// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Color/*" />
    MEMBER Color       := 15		// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Cursor/*" />
    MEMBER Cursor      := 16		// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Console/*" />
    MEMBER Console     := 17		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Alternate/*" />
    MEMBER Alternate   := 18		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.AltFile/*" />
    MEMBER AltFile     := 19		// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Device/*" />
    MEMBER Device      := 20		// STRING
    // 21 and 22 missing
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Printer/*" />
    MEMBER Printer     := 23		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.PrintFile/*" />
    MEMBER PrintFile   := 24		// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Margin/*" />
    MEMBER Margin      := 25		// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Bell/*" />
    MEMBER Bell        := 26		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Confirm/*" />
    MEMBER Confirm     := 27		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Escape/*" />
    MEMBER Escape      := 28		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    MEMBER Insert      := 29		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    MEMBER Exit        := 30		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    MEMBER Intensity   := 31		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    MEMBER Scoreboard  := 32		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    MEMBER Delimiters  := 33		// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    MEMBER DelimChars  := 34		// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    MEMBER Wrap        := 35		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    MEMBER Message     := 36		// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    MEMBER Mcenter     := 37		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    MEMBER ScrollBreak := 38		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    MEMBER Errorlog    := 39		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Neterr/*" />
    MEMBER Neterr      	:= 40	// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Digits/*" />
    MEMBER Digits      	:= 41	// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.AmExt/*" />
    MEMBER AmExt		:= 42	// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.PmExt/*" />
    MEMBER PmExt	    := 43	// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Ansi/*" />
    MEMBER Ansi      	:= 44	// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Yield/*" />
    MEMBER Yield     	:= 45	// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Locktries/*" />
    MEMBER Locktries   	:= 46	// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.AmPm/*" />
    MEMBER AmPm		    := 47	// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Century/*" />
    MEMBER Century	    := 48	// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DigitFixed/*" />
    MEMBER DigitFixed  	:= 49	// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DecimalSep/*" />
    MEMBER DecimalSep  	:= 50	// DWORD
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.ThousandSep/*" />
    MEMBER ThousandSep 	:= 51	// DWORD
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Timesep/*" />
    MEMBER Timesep     	:= 52	// DWORD
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Fieldstore/*" />
    MEMBER Fieldstore  	:= 53   // Logic
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Science/*" />
    MEMBER Science     	:= 54	// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    MEMBER Cpu			:= 55	// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Floatdelta/*" />
    MEMBER Floatdelta	:= 56	// System.Double
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    MEMBER Math			:= 57	// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.International/*" />
    MEMBER International:= 58	// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DateCountry/*" />
    MEMBER DateCountry  := 59	// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DefaultDir/*" />
    MEMBER DefaultDir   := 60   // STRING location of error log file

    // X# helper state
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.EpochCent/*" />
    MEMBER EpochCent     := 70		// Numeric
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.EpochYear/*" />
    MEMBER EpochYear     := 71		// Numeric
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DateFormatNet/*" />
    MEMBER DateFormatNet := 72		// String
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DateFormatEmpty/*" />
    MEMBER DateFormatEmpty := 73    // String
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.SysObject/*" />
    MEMBER SysObject := 74
    // 75 unused
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NoMethod/*" />
    MEMBER NoMethod		:= 76	// STRING
    // 77 unused
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Patharray/*" />
    MEMBER Patharray    := 78	// String[]
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NatDLL/*" />
    MEMBER NatDLL		:= 79   // string
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.CollationTable/*" />
    MEMBER CollationTable := 80  // byte[]
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    MEMBER ErrorLevel   := 81  // DWORD
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.ErrorBlock/*" />
    MEMBER ErrorBlock   := 82  // Codeblock
    // <summary>The last error that occurred for a RDD operation.</summary>
    //MEMBER LastRddError := 84   // Exception object
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.LastScriptError/*" />
    MEMBER LastScriptError := 85   // Exception object
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.LastFound/*" />
    MEMBER LastFound    := 86   // Last file found with File()
    // <summary>The last File error code</summary>
    //MEMBER FileError    := 87   // Last File error code
    // <summary>The last File exception</summary>
    //MEMBER FileException:= 88   // Last File exception

    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DelimRDD/*" />
    MEMBER DelimRDD         := 89
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.FieldDelimiter/*" />
    MEMBER FieldDelimiter   := 90
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.RecordDelimiter/*" />
    MEMBER RecordDelimiter  := 91
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.ErrorLogFile/*" />
    MEMBER ErrorLogFile     := 92
    // 92 - 97 unused
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.ErrorBlock/*" />
    MEMBER Dict        := 98	// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Intl/*" />
    MEMBER Intl        := 99	// CollationMode

    // Vulcan RDDInfo Settings
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.RddInfo/*" />
    MEMBER RddInfo		:= 100      // no value
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.MemoBlockSize/*" />
    MEMBER MemoBlockSize:= 101		// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DefaultRdd/*" />
    MEMBER DefaultRdd	:= 102		// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.MemoExt/*" />
    MEMBER MemoExt	    := 103		// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.AutoOpen/*" />
    MEMBER AutoOpen     := 104		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.AutoOrder/*" />
    MEMBER AutoOrder    := 105		// 0 or 1
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.HpLocking/*" />
    MEMBER HpLocking    := 106      // LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NewIndexLock/*" />
    MEMBER NewIndexLock := 107      // LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Autoshare/*" />
    MEMBER Autoshare    := 108		// 0 or 1
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.StrictRead/*" />
    MEMBER StrictRead   := 109		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.BlobCircref/*" />
    MEMBER BlobCircref	:= 110		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Optimize/*" />
    MEMBER Optimize     := 111		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.FoxLock/*" />
    MEMBER FoxLock      := 112		// LOGIC

    /// <include file="XSharp.CoreDefines.xml" path="members/Set.RddInfoMax/*" />
    MEMBER RddInfoMax   := 119      // no value

    /// <include file="XSharp.CoreDefines.xml" path="members/Set.WinCodepage/*" />
    MEMBER WinCodepage	:= 120		// Numeric
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DosCodepage/*" />
    MEMBER DosCodepage	:= 121		// Numeric
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.CollationMode/*" />
    MEMBER CollationMode:= 122		// CollationMode

    // 123 and 124 reserved
    // FoxPro settings 125 - 159, last used 145
    // Some settings are IDE specific, such as AutoSave, BrowseIME, Clock etc.
    // Others are windows regional settings, such as first day of week and currency symbol
    // these are not implemented
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Asserts/*" />
    MEMBER Asserts          := 125 // Logic
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.AutoIncError/*" />
    MEMBER AutoIncError     := 126 // Logic
    //MEMBER AutoSave
    //MEMBER BrowseIME
    //MEMBER Carry
    //MEMBER Clock
    //MEMBER ClockRowCol
    //MEMBER ColorScheme
    //MEMBER ColorSet
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.CollateFox/*" />
    MEMBER CollateFox       := 127
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Compatible/*" />
    MEMBER Compatible       := 128
    // MEMBER Coverage
    // MEMBER CoverageFile
    // MEMBER CpCompile
    // MEMBER CpDialog
    // MEMBER Currency
    // MEMBER CurrencySymbol
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Database/*" />
    MEMBER Database         := 129 // string
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DataSession/*" />
    MEMBER DataSession      := 130 // Numeric
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DebugOut/*" />
    MEMBER DebugOut         := 131 // string - filename
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Directory/*" />
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
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.FullPath/*" />
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
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Lock/*" />
    MEMBER Lock             := 133
    // MEMBER LogErrors
    // MEMBER MacKey
    // MEMBER MarkOf
    // MEMBER MarkTo
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.MemoWidth/*" />
    MEMBER MemoWidth        := 134
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.MultiLocks/*" />
    MEMBER MultiLocks       := 135
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Near/*" />
    MEMBER Near             := Set.Softseek
    // MEMBER NoCpTrans        // not needed: Unicode
    // MEMBER Notify
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Null/*" />
    MEMBER Null             := Set.NullValue
    // MEMBER NullDisplay
    // MEMBER Odometer
    // MEMBER OleObject
    // MEMBER Palette
    // MEMBER PdSetup
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Point/*" />
    MEMBER Point            := Set.DecimalSep
    // MEMBER Procedure
    // MEMBER ReadBorder
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Refresh/*" />
    MEMBER Refresh          := 136   // refresh time for local cache of shared data
    //MEMBER ReportBehavior
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Reprocess/*" />
    MEMBER Reprocess        := 137
    //MEMBER Resource
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Safety/*" />
    MEMBER Safety           := 138  // Logic
    // MEMBER Seconds
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Separator/*" />
    MEMBER Separator        := Set.ThousandSep
    // MEMBER Skip          // SelectiveRelation
    // MEMBER SkipOf
    //
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Space/*" />
    MEMBER Space            := 139    // Logic
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.SqlBuffering/*" />
    MEMBER SqlBuffering     := 140    // Logic
    // MEMBER StatusBar
    // MEMBER Status
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.SqlAnsi/*" />
    MEMBER SqlAnsi          := 141
    // MEMBER Step
    // MEMBER StrictDate
    // MEMBER SysFormats
    // MEMBER SysMenu
    // MEMBER TablePrompt
    // MEMBER TableValidate
    // MEMBER Talk
    //
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.TextMerge/*" />
    MEMBER TextMerge        := 142      // Logic
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.VarCharMapping/*" />
    MEMBER VarCharMapping   := 143    // Logic
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.TextMergeDelimiters/*" />
    MEMBER TextMergeDelimiters := 146 // string[]
    // MEMBER Topic
    // MEMBER TopicID
    // MEMBER TrBetween
    // MEMBER UdfParams
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.WithStack/*" />
    MEMBER WithStack        := 145

    // Xbase++ defines
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.CharSet/*" />
    MEMBER CharSet          := 160
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.HandleEvent/*" />
    MEMBER HandleEvent      := 161
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DevTimeOut/*" />
    MEMBER DevTimeOut       := 162
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Accelerators/*" />
    MEMBER Accelerators     := 163
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Colormode/*" />
    MEMBER Colormode        := 164
    // Optimize already defined
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Rushmore/*" />
    MEMBER Rushmore         := 165
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.SmartFilter/*" />
    MEMBER SmartFilter      := 166
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NullValue/*" />
    MEMBER NullValue        := 167
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Collation/*" />
    MEMBER Collation        := 168  // XPP Collation Number
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Lexical/*" />
    MEMBER Lexical          := 169  // Not implemented




    // 180 - 197 Harbour extensions, Most have  No defaults below yet
    // Originally these started at 100
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Language/*" />
    MEMBER Language       :=  180               // STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.IdleRepeat/*" />
    MEMBER IdleRepeat     :=  181               // Numeric Ignored for now
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.FileCase/*" />
    MEMBER FileCase       :=  182			    // Numeric Ignored for now
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DirCase/*" />
    MEMBER DirCase        :=  183               // Numeric Ignored for now
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DirSeparator/*" />
    MEMBER DirSeparator   :=  184               // String
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Eof/*" />
    MEMBER Eof            :=  185               // Logic: Is Chr(26) written to end of text files
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.HardCommit/*" />
    MEMBER HardCommit     :=  186               // Logic: Forces Hard Commit in RDD system (whatever that me be..)
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.ForceOpt/*" />
    MEMBER ForceOpt       :=  187               // LOGIC: Force Optimization
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DbfLockscheme/*" />
    MEMBER DbfLockscheme  :=  188               //
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Defextensions/*" />
    MEMBER Defextensions  :=  189               // Logic: Force Extensions for RDD and other output files. Not used yet
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Eol/*" />
    MEMBER Eol            :=  190               // ENd of Line characters
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Trimfilename/*" />
    MEMBER Trimfilename   :=  191	            // Logic: Should filenames be trimmed in the IO system
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Hboutlog/*" />
    MEMBER Hboutlog       :=  192               // STRING LogfileName
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Hboutloginfo/*" />
    MEMBER Hboutloginfo   :=  193               // String Info written to error log files
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Codepage/*" />
    MEMBER Codepage       :=  WinCodepage		// Remapped
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Oscodepage/*" />
    MEMBER Oscodepage     :=  DosCodepage	    // Remapped
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Timeformat/*" />
    MEMBER Timeformat     :=  196
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Dbcodepage/*" />
    MEMBER Dbcodepage     :=  197				// Map to Vulcan setting ?

    // Start of user values
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.User/*" />
    MEMBER User           := 200
    // Advantage extensions
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Axslocking/*" />
    MEMBER Axslocking           := User+1
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Rightschecking/*" />
    MEMBER Rightschecking       := User+2
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Connection_handle/*" />
    MEMBER Connection_handle    := User+3
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Exactkeypos/*" />
    MEMBER Exactkeypos          := User+4
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Sql_query/*" />
    MEMBER Sql_query            := User+5
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Sql_table_passwords/*" />
    MEMBER Sql_table_passwords  := User+6
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Collation_name/*" />
    MEMBER Collation_name       := User+7
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Sql_timeout/*" />
    MEMBER Sql_timeout          := User+8
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Sql_parameters/*" />
    MEMBER Sql_parameters       := User+9

END ENUM
END NAMESPACE
#region Defines
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Exact/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EXACT       := Set.Exact
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Fixed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FIXED       := Set.Fixed
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Decimals/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DECIMALS    := Set.Decimals
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DateFormat/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DATEFORMAT  := Set.DateFormat
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Epoch/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EPOCH       := Set.Epoch
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Path/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_PATH        := Set.Path
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Default/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEFAULT     := Set.Default
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Exclusive/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EXCLUSIVE   := Set.Exclusive
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Softseek/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SOFTSEEK    := Set.Softseek
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Unique/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_UNIQUE      := Set.Unique
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Deleted/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DELETED     := Set.Deleted

/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_Cancel      := Set.Cancel
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEBUG       := Set.Debug
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_TYPEAHEAD   := Set.Typeahead
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Color/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_COLOR       := Set.Color
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Cursor/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CURSOR      := Set.Cursor
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Console/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CONSOLE     := Set.Console
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Alternate/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_ALTERNATE   := Set.Alternate
/// <include file="XSharp.CoreDefines.xml" path="members/Set.AltFile/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_ALTFILE     := Set.AltFile
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Device/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEVICE      := Set.Device
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Printer/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_PRINTER     := Set.Printer
/// <include file="XSharp.CoreDefines.xml" path="members/Set.PrintFile/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_PRINTFILE   := Set.PrintFile
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Margin/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MARGIN      := Set.Margin
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Bell/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_BELL        := Set.Bell
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Confirm/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CONFIRM     := Set.Confirm
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Escape/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_ESCAPE      := Set.Escape
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_INSERT      := Set.Insert
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EXIT        := Set.Exit
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_INTENSITY   := Set.Intensity
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SCOREBOARD  := Set.Scoreboard
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DELIMITERS  := Set.Delimiters
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DELIMCHARS  := Set.DelimChars
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_WRAP        := Set.Wrap
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MESSAGE     := Set.Message
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MCENTER     := Set.Mcenter
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SCROLLBREAK := Set.ScrollBreak
// 48 and 49 unused
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Digits/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DIGITS      	:= Set.Digits
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Neterr/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_NETERR      	:= Set.Neterr
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Ansi/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_ANSI      		:= Set.Ansi
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Yield/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_YIELD     		:= Set.Yield
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Locktries/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_LOCKTRIES   	:= Set.Locktries
/// <include file="XSharp.CoreDefines.xml" path="members/Set.AmExt/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AMEXT			:= Set.AmExt
/// <include file="XSharp.CoreDefines.xml" path="members/Set.AmPm/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AMPM			:= Set.AmPm
/// <include file="XSharp.CoreDefines.xml" path="members/Set.PmExt/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_PMEXT	    	:= Set.PmExt
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Century/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CENTURY	    	:= Set.Century
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DigitFixed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DIGITFIXED  	:= Set.DigitFixed
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DecimalSep/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DECIMALSEP  	:= Set.DecimalSep
/// <include file="XSharp.CoreDefines.xml" path="members/Set.ThousandSep/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_THOUSANDSEP 	:= Set.ThousandSep
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Timesep/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_TIMESEP     	:= Set.Timesep
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Fieldstore/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FIELDSTORE  	:= Set.Fieldstore
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Science/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SCIENCE     	:= Set.Science
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CPU				:= Set.Cpu
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Floatdelta/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FLOATDELTA		:= Set.Floatdelta
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MATH			:= Set.Math
/// <include file="XSharp.CoreDefines.xml" path="members/Set.International/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_INTERNATIONAL	:= Set.International
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DateCountry/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DATECOUNTRY		:= Set.DateCountry
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Dict/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DICT			:= Set.Dict
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Intl/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_INTL			:= Set.Intl
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DefaultDir/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEFAULTDIR      := Set.DefaultDir
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DelimRDD/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DELIMRDD         := Set.DelimRDD
/// <include file="XSharp.CoreDefines.xml" path="members/Set.FieldDelimiter/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FIELDDELIMITER   := Set.FieldDelimiter
/// <include file="XSharp.CoreDefines.xml" path="members/Set.RecordDelimiter/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_RECORDDELIMITER  := Set.RecordDelimiter
/// <include file="XSharp.CoreDefines.xml" path="members/Set.BlobCircref/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_BLOB_CIRCULAR_ARRAY_REF := Set.BlobCircref
/// <include file="XSharp.CoreDefines.xml" path="members/Set.User/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_USER := Set.User


/// <include file="XSharp.CoreDefines.xml" path="members/Set.CollationMode/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_COLLATIONMODE	:= Set.CollationMode

// Vulcan RDDInfo Settings
/// <include file="XSharp.CoreDefines.xml" path="members/Set.RddInfo/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_RDDINFO				:= Set.RddInfo
/// <include file="XSharp.CoreDefines.xml" path="members/Set.MemoBlockSize/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MEMOBLOCKSIZE		:= Set.MemoBlockSize
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DefaultRdd/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEFAULTRDD			:= Set.DefaultRdd
/// <include file="XSharp.CoreDefines.xml" path="members/Set.MemoExt/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MEMOEXT	    		:= Set.MemoExt
/// <include file="XSharp.CoreDefines.xml" path="members/Set.AutoOpen/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AUTOOPEN    		:= Set.AutoOpen
/// <include file="XSharp.CoreDefines.xml" path="members/Set.AutoOrder/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AUTOORDER   		:= Set.AutoOrder
/// <include file="XSharp.CoreDefines.xml" path="members/Set.HpLocking/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_HPLOCKING   		:= Set.HpLocking
/// <include file="XSharp.CoreDefines.xml" path="members/Set.HpLocking/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_HPLOCK      		:= Set.HpLocking
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NewIndexLock/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_NEWINDEXLOCK		:= Set.NewIndexLock
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Autoshare/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AUTOSHARE   		:= Set.Autoshare
/// <include file="XSharp.CoreDefines.xml" path="members/Set.StrictRead/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_STRICTREAD  		:= Set.StrictRead
/// <include file="XSharp.CoreDefines.xml" path="members/Set.BlobCircref/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_BLOBCIRCREF			:= Set.BlobCircref
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Optimize/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_OPTIMIZE    		:= Set.Optimize
/// <include file="XSharp.CoreDefines.xml" path="members/Set.FoxLock/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FOXLOCK     		:= Set.FoxLock
/// <include file="XSharp.CoreDefines.xml" path="members/Set.WinCodepage/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_WINCODEPAGE			:= Set.WinCodepage
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DosCodepage/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DOSCODEPAGE			:= Set.DosCodepage

// Harbour extensions
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Language/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_LANGUAGE       :=  Set.Language
/// <include file="XSharp.CoreDefines.xml" path="members/Set.IdleRepeat/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_IDLEREPEAT     :=  Set.IdleRepeat
/// <include file="XSharp.CoreDefines.xml" path="members/Set.FileCase/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FILECASE       :=  Set.FileCase
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DirCase/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DIRCASE        :=  Set.DirCase
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DirSeparator/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DIRSEPARATOR   :=  Set.DirSeparator
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Eof/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EOF            :=  Set.Eof
/// <include file="XSharp.CoreDefines.xml" path="members/Set.HardCommit/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_HARDCOMMIT     :=  Set.HardCommit
/// <include file="XSharp.CoreDefines.xml" path="members/Set.ForceOpt/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FORCEOPT       :=  Set.ForceOpt
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DbfLockscheme/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DBFLOCKSCHEME  :=  Set.DbfLockscheme
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Defextensions/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEFEXTENSIONS  :=  Set.Defextensions
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Eol/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EOL            :=  Set.Eol
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Trimfilename/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_TRIMFILENAME   :=  Set.Trimfilename
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Hboutlog/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_HBOUTLOG       :=  Set.Hboutlog
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Hboutloginfo/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_HBOUTLOGINFO   :=  Set.Hboutloginfo
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Codepage/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CODEPAGE       :=  Set.Codepage
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Oscodepage/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_OSCODEPAGE     :=  Set.Oscodepage
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Timeformat/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_TIMEFORMAT     :=  Set.Timeformat
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Dbcodepage/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DBCODEPAGE     :=  Set.Dbcodepage

// Advantage additions
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Axslocking/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AXSLOCKING           := Set.Axslocking
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Rightschecking/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_RIGHTSCHECKING       := Set.Rightschecking
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Connection_handle/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CONNECTION_HANDLE    := Set.Connection_handle
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Exactkeypos/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EXACTKEYPOS          := Set.Exactkeypos
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Sql_query/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SQL_QUERY            := Set.Sql_query
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Sql_table_passwords/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SQL_TABLE_PASSWORDS  := Set.Sql_table_passwords
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Collation_name/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_COLLATION_NAME       := Set.Collation_name
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Sql_parameters/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SQL_PARAMETERS       :=  Set.Sql_parameters
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Sql_timeout/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SQL_TIMEOUT          :=  Set.Sql_timeout


// Xbase++ additions
DEFINE  _SET_CHARSET     := Set.CharSet
/// <include file="XSharp.CoreDefines.xml" path="members/Set.HandleEvent/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_HANDLEEVENT := Set.HandleEvent
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DevTimeOut/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_DEVTIMEOUT  := Set.DevTimeOut
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Accelerators/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_ACCELERATORS := Set.Accelerators
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Colormode/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_COLORMODE    := Set.Colormode
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Rushmore/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_RUSHMORE    := Set.Rushmore
/// <include file="XSharp.CoreDefines.xml" path="members/Set.SmartFilter/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_SMARTFILTER := Set.SmartFilter
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NullValue/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_NULLVALUE   := Set.NullValue

/// <include file="XSharp.CoreDefines.xml" path="members/Set.Collation/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_COLLATION   := Set.Collation
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Lexical/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE  _SET_LEXICAL    := Set.Lexical


// FoxPro defines
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Asserts/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_ASSERTS         := Set.Asserts
/// <include file="XSharp.CoreDefines.xml" path="members/Set.AutoIncError/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AUTOINCERROR    := Set.AutoIncError
/// <include file="XSharp.CoreDefines.xml" path="members/Set.CollateFox/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_COLLATEFOX := Set.CollateFox
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Database/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DATABASE        := Set.Database
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DataSession/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DATASESSION     := Set.DataSession
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DebugOut/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEBUGOUT        := Set.DebugOut
/// <include file="XSharp.CoreDefines.xml" path="members/Set.FullPath/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FULLPATH        := Set.FullPath
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Lock/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_LOCK            := Set.Lock
/// <include file="XSharp.CoreDefines.xml" path="members/Set.MemoWidth/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MEMOWIDTH       := Set.MemoWidth
/// <include file="XSharp.CoreDefines.xml" path="members/Set.MultiLocks/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MULTILOCKS      := Set.MultiLocks
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Null/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_NULL             := Set.Null
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Point/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_POINT            := Set.Point
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Refresh/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_REFRESH          := Set.Refresh
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Reprocess/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_REPROCESS        := Set.Reprocess
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Safety/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SAFETY           := Set.Safety
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Separator/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SEPARATOR        := Set.Separator
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Space/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SPACE           := Set.Space
/// <include file="XSharp.CoreDefines.xml" path="members/Set.SqlAnsi/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SQLANSI         := Set.SqlAnsi
/// <include file="XSharp.CoreDefines.xml" path="members/Set.SqlBuffering/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SQLBUFFERING     := Set.SqlBuffering
/// <include file="XSharp.CoreDefines.xml" path="members/Set.TextMerge/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_TEXTMERGE        := Set.TextMerge
/// <include file="XSharp.CoreDefines.xml" path="members/Set.VarCharMapping/*" />
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


