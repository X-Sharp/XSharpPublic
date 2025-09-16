//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using XSharp
using XSharp.RDD.Enums
using System.Collections.Generic
begin namespace XSharp
/// <include file="XSharp.CoreDefines.xml" path="members/Set/*" />
/// <seealso cref='RuntimeState'>RuntimeState</seealso>
/// <seealso cref='RuntimeState.GetValue``1(XSharp.Set)'>RuntimeState.GetValue</seealso>
/// <seealso cref='RuntimeState.SetValue``1(XSharp.Set,``0)'>RuntimeState.SetValue</seealso>

enum Set
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Exact/*" />
    member Exact       := 1			// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Fixed/*" />
    member Fixed	   := 2			// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Decimals/*" />
    member Decimals    := 3			// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DateFormat/*" />
    member DateFormat  := 4			// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Epoch/*" />
    member Epoch       := 5			// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Path/*" />
    member Path        := 6			// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Default/*" />
    member @@Default   := 7			// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Exclusive/*" />
    member Exclusive   := 8			// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.SoftSeek/*" />
    member Softseek    := 9			// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Unique/*" />
    member Unique      := 10		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Deleted/*" />
    member Deleted     := 11		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    member Cancel      := 12		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    member @@Debug     := 13
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    member Typeahead   := 14		// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Color/*" />
    member Color       := 15		// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Cursor/*" />
    member Cursor      := 16		// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Console/*" />
    member Console     := 17		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Alternate/*" />
    member Alternate   := 18		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.AltFile/*" />
    member AltFile     := 19		// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Device/*" />
    member Device      := 20		// STRING
    // 21 and 22 missing
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Printer/*" />
    member Printer     := 23		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.PrintFile/*" />
    member PrintFile   := 24		// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Margin/*" />
    member Margin      := 25		// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Bell/*" />
    member Bell        := 26		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Confirm/*" />
    member Confirm     := 27		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Escape/*" />
    member Escape      := 28		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    member Insert      := 29		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    member @@Exit        := 30		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    member Intensity   := 31		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    member Scoreboard  := 32		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    member Delimiters  := 33		// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    member DelimChars  := 34		// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    member Wrap        := 35		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    member Message     := 36		// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    member Mcenter     := 37		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    member ScrollBreak := 38		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    member Errorlog    := 39		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Neterr/*" />
    member Neterr      	:= 40	// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Digits/*" />
    member Digits      	:= 41	// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.AmExt/*" />
    member AmExt		:= 42	// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.PmExt/*" />
    member PmExt	    := 43	// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Ansi/*" />
    member Ansi      	:= 44	// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Yield/*" />
    member @@Yield     	:= 45	// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Locktries/*" />
    member Locktries   	:= 46	// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.AmPm/*" />
    member AmPm		    := 47	// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Century/*" />
    member Century	    := 48	// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DigitFixed/*" />
    member DigitFixed  	:= 49	// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DecimalSep/*" />
    member DecimalSep  	:= 50	// DWORD
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.ThousandSep/*" />
    member ThousandSep 	:= 51	// DWORD
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Timesep/*" />
    member Timesep     	:= 52	// DWORD
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Fieldstore/*" />
    member Fieldstore  	:= 53   // Logic
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Science/*" />
    member Science     	:= 54	// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    member Cpu			:= 55	// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Floatdelta/*" />
    member Floatdelta	:= 56	// System.Double
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    member Math			:= 57	// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.International/*" />
    member International:= 58	// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DateCountry/*" />
    member DateCountry  := 59	// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DefaultDir/*" />
    member DefaultDir   := 60   // STRING location of error log file

    // X# helper state
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.EpochCent/*" />
    member EpochCent     := 70		// Numeric
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.EpochYear/*" />
    member EpochYear     := 71		// Numeric
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DateFormatNet/*" />
    member DateFormatNet := 72		// String
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DateFormatEmpty/*" />
    member DateFormatEmpty := 73    // String
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.SysObject/*" />
    member SysObject := 74
    // 75 unused
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NoMethod/*" />
    member NoMethod		:= 76	// STRING
    // 77 unused
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Patharray/*" />
    member Patharray    := 78	// String[]
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NatDLL/*" />
    member NatDLL		:= 79   // string
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.CollationTable/*" />
    member CollationTable := 80  // byte[]
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
    member ErrorLevel   := 81  // DWORD
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.ErrorBlock/*" />
    member ErrorBlock   := 82  // Codeblock
    // <summary>The last error that occurred for a RDD operation.</summary>
    //MEMBER LastRddError := 84   // Exception object
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.LastScriptError/*" />
    member LastScriptError := 85   // Exception object
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.LastFound/*" />
    member LastFound    := 86   // Last file found with File()
    // <summary>The last File error code</summary>
    //MEMBER FileError    := 87   // Last File error code
    // <summary>The last File exception</summary>
    //MEMBER FileException:= 88   // Last File exception

    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DelimRDD/*" />
    member DelimRDD         := 89
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.FieldDelimiter/*" />
    member FieldDelimiter   := 90
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.RecordDelimiter/*" />
    member RecordDelimiter  := 91
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.ErrorLogFile/*" />
    member ErrorLogFile     := 92
    // 92 - 97 unused
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.ErrorBlock/*" />
    member Dict        := 98	// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Intl/*" />
    member Intl        := 99	// CollationMode

    // Vulcan RDDInfo Settings
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.RddInfo/*" />
    member RddInfo		:= 100      // no value
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.MemoBlockSize/*" />
    member MemoBlockSize:= 101		// INT
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DefaultRdd/*" />
    member DefaultRdd	:= 102		// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.MemoExt/*" />
    member MemoExt	    := 103		// STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.AutoOpen/*" />
    member AutoOpen     := 104		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.AutoOrder/*" />
    member AutoOrder    := 105		// 0 or 1
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.HpLocking/*" />
    member HpLocking    := 106      // LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NewIndexLock/*" />
    member NewIndexLock := 107      // LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Autoshare/*" />
    member Autoshare    := 108		// 0 or 1
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.StrictRead/*" />
    member StrictRead   := 109		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.BlobCircref/*" />
    member BlobCircref	:= 110		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Optimize/*" />
    member Optimize     := 111		// LOGIC
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.FoxLock/*" />
    member FoxLock      := 112		// LOGIC

    /// <include file="XSharp.CoreDefines.xml" path="members/Set.RddInfoMax/*" />
    member RddInfoMax   := 119      // no value

    /// <include file="XSharp.CoreDefines.xml" path="members/Set.WinCodepage/*" />
    member WinCodepage	:= 120		// Numeric
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DosCodepage/*" />
    member DosCodepage	:= 121		// Numeric
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.CollationMode/*" />
    member CollationMode:= 122		// CollationMode

    // 123 and 124 reserved
    // FoxPro settings 125 - 159, last used 145
    // Some settings are IDE specific, such as AutoSave, BrowseIME, Clock etc.
    // Others are windows regional settings, such as first day of week and currency symbol
    // these are not implemented
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Asserts/*" />
    member Asserts          := 125 // Logic
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.AutoIncError/*" />
    member AutoIncError     := 126 // Logic
    //MEMBER AutoSave
    //MEMBER BrowseIME
    //MEMBER Carry
    //MEMBER Clock
    //MEMBER ClockRowCol
    //MEMBER ColorScheme
    //MEMBER ColorSet
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.CollateFox/*" />
    member CollateFox       := 127
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Compatible/*" />
    member Compatible       := 128
    // MEMBER Coverage
    // MEMBER CoverageFile
    // MEMBER CpCompile
    // MEMBER CpDialog
    // MEMBER Currency
    // MEMBER CurrencySymbol
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Database/*" />
    member Database         := 129 // string
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DataSession/*" />
    member DataSession      := 130 // Numeric
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DebugOut/*" />
    member DebugOut         := 131 // string - filename
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Directory/*" />
    member Directory        := Set.Default         // FoxPro alias
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
    member FullPath         := 132      // Logic
    // MEMBER Function
    // MEMBER FWeek
    member Headings         := 144
    // MEMBER Help
    // MEMBER Intensity
    // MEMBER Key
    // MEMBER KeyComp
    // MEMBER Libary
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Lock/*" />
    member Lock             := 133
    // MEMBER LogErrors
    // MEMBER MacKey
    // MEMBER MarkOf
    // MEMBER MarkTo
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.MemoWidth/*" />
    member MemoWidth        := 134
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.MultiLocks/*" />
    member MultiLocks       := 135
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Near/*" />
    member Near             := Set.Softseek
    // MEMBER NoCpTrans        // not needed: Unicode
    // MEMBER Notify
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Null/*" />
    member @@Null             := Set.NullValue
    // MEMBER NullDisplay
    // MEMBER Odometer
    // MEMBER OleObject
    // MEMBER Palette
    // MEMBER PdSetup
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Point/*" />
    member Point            := Set.DecimalSep
    // MEMBER Procedure
    // MEMBER ReadBorder
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Refresh/*" />
    member Refresh          := 136   // refresh time for local cache of shared data
    //MEMBER ReportBehavior
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Reprocess/*" />
    member Reprocess        := 137
    //MEMBER Resource
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Safety/*" />
    member Safety           := 138  // Logic
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Separator/*" />
    member Separator        := Set.ThousandSep
    // MEMBER Skip          // SelectiveRelation
    // MEMBER SkipOf
    //
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Space/*" />
    member Space            := 139    // Logic
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.SqlBuffering/*" />
    member SqlBuffering     := 140    // Logic
    // MEMBER StatusBar
    // MEMBER Status
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.SqlAnsi/*" />
    member SqlAnsi          := 141
    // MEMBER Step
    // MEMBER StrictDate
    // MEMBER SysFormats
    // MEMBER SysMenu
    // MEMBER TablePrompt
    // MEMBER TableValidate
    // MEMBER Talk
    //
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.TextMerge/*" />
    member TextMerge        := 142      // Logic
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.VarCharMapping/*" />
    member VarCharMapping   := 143    // Logic
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.TextMergeDelimiters/*" />
    member TextMergeDelimiters := 146 // string[]
    // MEMBER Topic
    // MEMBER TopicID
    // MEMBER TrBetween
    // MEMBER UdfParams
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.WithStack/*" />
    member WithStack        := 145

    // Xbase++ defines
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.CharSet/*" />
    member CharSet          := 160
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.HandleEvent/*" />
    member HandleEvent      := 161
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DevTimeOut/*" />
    member DevTimeOut       := 162
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Accelerators/*" />
    member Accelerators     := 163
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Colormode/*" />
    member Colormode        := 164
    // Optimize already defined
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Rushmore/*" />
    member Rushmore         := 165
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.SmartFilter/*" />
    member SmartFilter      := 166
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.NullValue/*" />
    member NullValue        := 167
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Collation/*" />
    member Collation        := 168  // XPP Collation Number
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Lexical/*" />
    member Lexical          := 169  // Not implemented




    // 180 - 197 Harbour extensions, Most have  No defaults below yet
    // Originally these started at 100
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Language/*" />
    member Language       :=  180               // STRING
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.IdleRepeat/*" />
    member IdleRepeat     :=  181               // Numeric Ignored for now
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.FileCase/*" />
    member FileCase       :=  182			    // Numeric Ignored for now
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DirCase/*" />
    member DirCase        :=  183               // Numeric Ignored for now
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DirSeparator/*" />
    member DirSeparator   :=  184               // String
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Eof/*" />
    member Eof            :=  185               // Logic: Is Chr(26) written to end of text files
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.HardCommit/*" />
    member HardCommit     :=  186               // Logic: Forces Hard Commit in RDD system (whatever that me be..)
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.ForceOpt/*" />
    member ForceOpt       :=  187               // LOGIC: Force Optimization
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.DbfLockscheme/*" />
    member DbfLockscheme  :=  188               //
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Defextensions/*" />
    member Defextensions  :=  189               // Logic: Force Extensions for RDD and other output files. Not used yet
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Eol/*" />
    member Eol            :=  190               // ENd of Line characters
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Trimfilename/*" />
    member Trimfilename   :=  191	            // Logic: Should filenames be trimmed in the IO system
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Hboutlog/*" />
    member Hboutlog       :=  192               // STRING LogfileName
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Hboutloginfo/*" />
    member Hboutloginfo   :=  193               // String Info written to error log files
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Codepage/*" />
    member Codepage       :=  WinCodepage		// Remapped
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Oscodepage/*" />
    member Oscodepage     :=  DosCodepage	    // Remapped
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Timeformat/*" />
    member Timeformat     :=  196
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Dbcodepage/*" />
    member Dbcodepage     :=  197				// Map to Vulcan setting ?

    // Start of user values
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.User/*" />
    member User           := 200
    // Advantage extensions
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Axslocking/*" />
    member Axslocking           := User+1
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Rightschecking/*" />
    member Rightschecking       := User+2
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Connection_handle/*" />
    member Connection_handle    := User+3
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Exactkeypos/*" />
    member Exactkeypos          := User+4
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Sql_query/*" />
    member Sql_query            := User+5
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Sql_table_passwords/*" />
    member Sql_table_passwords  := User+6
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Collation_name/*" />
    member Collation_name       := User+7
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Sql_timeout/*" />
    member Sql_timeout          := User+8
    /// <include file="XSharp.CoreDefines.xml" path="members/Set.Sql_parameters/*" />
    member Sql_parameters       := User+9
	/// <summary>Hours format setting (12 vs 24 hours)</summary>
	/// <include file="XSharp.CoreDefines.xml" path="members/Set.Hours/*" />
	member Hours				:= User+10
	/// <summary>Show seconds in time display</summary>
	/// <include file="XSharp.CoreDefines.xml" path="members/Set.Seconds/*" />
	member Seconds				:= User+11
end enum
end namespace
#region Defines
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Exact/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_EXACT       := Set.Exact
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Fixed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_FIXED       := Set.Fixed
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Decimals/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DECIMALS    := Set.Decimals
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DateFormat/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DATEFORMAT  := Set.DateFormat
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Epoch/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_EPOCH       := Set.Epoch
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Path/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_PATH        := Set.Path
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Default/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DEFAULT     := Set.Default
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Exclusive/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_EXCLUSIVE   := Set.Exclusive
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Softseek/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_SOFTSEEK    := Set.Softseek
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Unique/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_UNIQUE      := Set.Unique
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Deleted/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DELETED     := Set.Deleted

/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_Cancel      := Set.Cancel
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DEBUG       := Set.Debug
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_TYPEAHEAD   := Set.Typeahead
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Color/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_COLOR       := Set.Color
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Cursor/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_CURSOR      := Set.Cursor
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Console/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_CONSOLE     := Set.Console
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Alternate/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_ALTERNATE   := Set.Alternate
/// <include file="XSharp.CoreDefines.xml" path="members/Set.AltFile/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_ALTFILE     := Set.AltFile
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Device/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DEVICE      := Set.Device
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Printer/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_PRINTER     := Set.Printer
/// <include file="XSharp.CoreDefines.xml" path="members/Set.PrintFile/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_PRINTFILE   := Set.PrintFile
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Margin/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_MARGIN      := Set.Margin
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Bell/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_BELL        := Set.Bell
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Confirm/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_CONFIRM     := Set.Confirm
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Escape/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_ESCAPE      := Set.Escape
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_INSERT      := Set.Insert
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_EXIT        := Set.Exit
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_INTENSITY   := Set.Intensity
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_SCOREBOARD  := Set.Scoreboard
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DELIMITERS  := Set.Delimiters
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DELIMCHARS  := Set.DelimChars
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_WRAP        := Set.Wrap
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_MESSAGE     := Set.Message
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_MCENTER     := Set.Mcenter
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_SCROLLBREAK := Set.ScrollBreak
// 48 and 49 unused
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Digits/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DIGITS      	:= Set.Digits
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Neterr/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_NETERR      	:= Set.Neterr
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Ansi/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_ANSI      		:= Set.Ansi
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Yield/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_YIELD     		:= Set.Yield
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Locktries/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_LOCKTRIES   	:= Set.Locktries
/// <include file="XSharp.CoreDefines.xml" path="members/Set.AmExt/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_AMEXT			:= Set.AmExt
/// <include file="XSharp.CoreDefines.xml" path="members/Set.AmPm/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_AMPM			:= Set.AmPm
/// <include file="XSharp.CoreDefines.xml" path="members/Set.PmExt/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_PMEXT	    	:= Set.PmExt
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Century/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_CENTURY	    	:= Set.Century
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DigitFixed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DIGITFIXED  	:= Set.DigitFixed
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DecimalSep/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DECIMALSEP  	:= Set.DecimalSep
/// <include file="XSharp.CoreDefines.xml" path="members/Set.ThousandSep/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_THOUSANDSEP 	:= Set.ThousandSep
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Timesep/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_TIMESEP     	:= Set.Timesep
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Fieldstore/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_FIELDSTORE  	:= Set.Fieldstore
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Science/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_SCIENCE     	:= Set.Science
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_CPU				:= Set.Cpu
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Floatdelta/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_FLOATDELTA		:= Set.Floatdelta
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NotUsed/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_MATH			:= Set.Math
/// <include file="XSharp.CoreDefines.xml" path="members/Set.International/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_INTERNATIONAL	:= Set.International
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DateCountry/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DATECOUNTRY		:= Set.DateCountry
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Dict/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DICT			:= Set.Dict
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Intl/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_INTL			:= Set.Intl
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DefaultDir/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DEFAULTDIR      := Set.DefaultDir
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DelimRDD/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DELIMRDD         := Set.DelimRDD
/// <include file="XSharp.CoreDefines.xml" path="members/Set.FieldDelimiter/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_FIELDDELIMITER   := Set.FieldDelimiter
/// <include file="XSharp.CoreDefines.xml" path="members/Set.RecordDelimiter/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_RECORDDELIMITER  := Set.RecordDelimiter
/// <include file="XSharp.CoreDefines.xml" path="members/Set.BlobCircref/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_BLOB_CIRCULAR_ARRAY_REF := Set.BlobCircref
/// <include file="XSharp.CoreDefines.xml" path="members/Set.User/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_USER := Set.User


/// <include file="XSharp.CoreDefines.xml" path="members/Set.CollationMode/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_COLLATIONMODE	:= Set.CollationMode

// Vulcan RDDInfo Settings
/// <include file="XSharp.CoreDefines.xml" path="members/Set.RddInfo/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_RDDINFO				:= Set.RddInfo
/// <include file="XSharp.CoreDefines.xml" path="members/Set.MemoBlockSize/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_MEMOBLOCKSIZE		:= Set.MemoBlockSize
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DefaultRdd/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DEFAULTRDD			:= Set.DefaultRdd
/// <include file="XSharp.CoreDefines.xml" path="members/Set.MemoExt/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_MEMOEXT	    		:= Set.MemoExt
/// <include file="XSharp.CoreDefines.xml" path="members/Set.AutoOpen/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_AUTOOPEN    		:= Set.AutoOpen
/// <include file="XSharp.CoreDefines.xml" path="members/Set.AutoOrder/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_AUTOORDER   		:= Set.AutoOrder
/// <include file="XSharp.CoreDefines.xml" path="members/Set.HpLocking/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_HPLOCKING   		:= Set.HpLocking
/// <include file="XSharp.CoreDefines.xml" path="members/Set.HpLocking/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_HPLOCK      		:= Set.HpLocking
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NewIndexLock/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_NEWINDEXLOCK		:= Set.NewIndexLock
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Autoshare/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_AUTOSHARE   		:= Set.Autoshare
/// <include file="XSharp.CoreDefines.xml" path="members/Set.StrictRead/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_STRICTREAD  		:= Set.StrictRead
/// <include file="XSharp.CoreDefines.xml" path="members/Set.BlobCircref/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_BLOBCIRCREF			:= Set.BlobCircref
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Optimize/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_OPTIMIZE    		:= Set.Optimize
/// <include file="XSharp.CoreDefines.xml" path="members/Set.FoxLock/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_FOXLOCK     		:= Set.FoxLock
/// <include file="XSharp.CoreDefines.xml" path="members/Set.WinCodepage/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_WINCODEPAGE			:= Set.WinCodepage
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DosCodepage/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DOSCODEPAGE			:= Set.DosCodepage

// Harbour extensions
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Language/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_LANGUAGE       :=  Set.Language
/// <include file="XSharp.CoreDefines.xml" path="members/Set.IdleRepeat/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_IDLEREPEAT     :=  Set.IdleRepeat
/// <include file="XSharp.CoreDefines.xml" path="members/Set.FileCase/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_FILECASE       :=  Set.FileCase
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DirCase/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DIRCASE        :=  Set.DirCase
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DirSeparator/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DIRSEPARATOR   :=  Set.DirSeparator
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Eof/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_EOF            :=  Set.Eof
/// <include file="XSharp.CoreDefines.xml" path="members/Set.HardCommit/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_HARDCOMMIT     :=  Set.HardCommit
/// <include file="XSharp.CoreDefines.xml" path="members/Set.ForceOpt/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_FORCEOPT       :=  Set.ForceOpt
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DbfLockscheme/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DBFLOCKSCHEME  :=  Set.DbfLockscheme
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Defextensions/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DEFEXTENSIONS  :=  Set.Defextensions
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Eol/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_EOL            :=  Set.Eol
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Trimfilename/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_TRIMFILENAME   :=  Set.Trimfilename
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Hboutlog/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_HBOUTLOG       :=  Set.Hboutlog
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Hboutloginfo/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_HBOUTLOGINFO   :=  Set.Hboutloginfo
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Codepage/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_CODEPAGE       :=  Set.Codepage
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Oscodepage/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_OSCODEPAGE     :=  Set.Oscodepage
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Timeformat/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_TIMEFORMAT     :=  Set.Timeformat
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Dbcodepage/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DBCODEPAGE     :=  Set.Dbcodepage

// Advantage additions
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Axslocking/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_AXSLOCKING           := Set.Axslocking
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Rightschecking/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_RIGHTSCHECKING       := Set.Rightschecking
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Connection_handle/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_CONNECTION_HANDLE    := Set.Connection_handle
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Exactkeypos/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_EXACTKEYPOS          := Set.Exactkeypos
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Sql_query/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_SQL_QUERY            := Set.Sql_query
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Sql_table_passwords/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_SQL_TABLE_PASSWORDS  := Set.Sql_table_passwords
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Collation_name/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_COLLATION_NAME       := Set.Collation_name
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Sql_parameters/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_SQL_PARAMETERS       :=  Set.Sql_parameters
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Sql_timeout/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_SQL_TIMEOUT          :=  Set.Sql_timeout


// Xbase++ additions
define  _SET_CHARSET     := Set.CharSet
/// <include file="XSharp.CoreDefines.xml" path="members/Set.HandleEvent/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define  _SET_HANDLEEVENT := Set.HandleEvent
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DevTimeOut/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define  _SET_DEVTIMEOUT  := Set.DevTimeOut
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Accelerators/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define  _SET_ACCELERATORS := Set.Accelerators
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Colormode/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define  _SET_COLORMODE    := Set.Colormode
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Rushmore/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define  _SET_RUSHMORE    := Set.Rushmore
/// <include file="XSharp.CoreDefines.xml" path="members/Set.SmartFilter/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define  _SET_SMARTFILTER := Set.SmartFilter
/// <include file="XSharp.CoreDefines.xml" path="members/Set.NullValue/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define  _SET_NULLVALUE   := Set.NullValue

/// <include file="XSharp.CoreDefines.xml" path="members/Set.Collation/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define  _SET_COLLATION   := Set.Collation
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Lexical/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define  _SET_LEXICAL    := Set.Lexical


// FoxPro defines
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Asserts/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_ASSERTS         := Set.Asserts
/// <include file="XSharp.CoreDefines.xml" path="members/Set.AutoIncError/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_AUTOINCERROR    := Set.AutoIncError
/// <include file="XSharp.CoreDefines.xml" path="members/Set.CollateFox/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_COLLATEFOX := Set.CollateFox
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Database/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DATABASE        := Set.Database
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DataSession/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DATASESSION     := Set.DataSession
/// <include file="XSharp.CoreDefines.xml" path="members/Set.DebugOut/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_DEBUGOUT        := Set.DebugOut
/// <include file="XSharp.CoreDefines.xml" path="members/Set.FullPath/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_FULLPATH        := Set.FullPath
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Lock/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_LOCK            := Set.Lock
/// <include file="XSharp.CoreDefines.xml" path="members/Set.MemoWidth/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_MEMOWIDTH       := Set.MemoWidth
/// <include file="XSharp.CoreDefines.xml" path="members/Set.MultiLocks/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_MULTILOCKS      := Set.MultiLocks
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Null/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_NULL             := Set.Null
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Point/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_POINT            := Set.Point
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Refresh/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_REFRESH          := Set.Refresh
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Reprocess/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_REPROCESS        := Set.Reprocess
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Safety/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_SAFETY           := Set.Safety
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Separator/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_SEPARATOR        := Set.Separator
/// <include file="XSharp.CoreDefines.xml" path="members/Set.Space/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_SPACE           := Set.Space
/// <include file="XSharp.CoreDefines.xml" path="members/Set.SqlAnsi/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_SQLANSI         := Set.SqlAnsi
/// <include file="XSharp.CoreDefines.xml" path="members/Set.SqlBuffering/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_SQLBUFFERING     := Set.SqlBuffering
/// <include file="XSharp.CoreDefines.xml" path="members/Set.TextMerge/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_TEXTMERGE        := Set.TextMerge
/// <include file="XSharp.CoreDefines.xml" path="members/Set.VarCharMapping/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_VARCHARMAPPING    := Set.VarCharMapping

/// <include file="XSharp.CoreDefines.xml" path="members/Set.Hours/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_HOURS := Set.Hours

/// <include file="XSharp.CoreDefines.xml" path="members/Set.Seconds/*" />
/// <include file="CoreComments.xml" path="Comments/Set/*" />
define _SET_SECONDS := Set.Seconds
#endregion


define _MAX_PATH := 260
define MAX_PATH := 260

internal function RuntimeStateDefaultValue(nSet as XSharp.Set) as object
	switch nSet
	case Set.Exact
	case Set.Fixed
	case Set.Softseek
	case Set.Unique
	case Set.Deleted
	case Set.Cancel
	case Set.Debug
	case Set.Alternate
	case Set.Printer
	case Set.Confirm
	case Set.Escape
	case Set.Insert
	case Set.Exit
	case Set.Intensity
	case Set.Scoreboard
	case Set.Wrap
	case Set.Mcenter
	case Set.ScrollBreak
	case Set.Errorlog
	case Set.Yield
	case Set.Neterr
	case Set.AmPm
	case Set.Century
	case Set.DigitFixed
	case Set.Fieldstore
	case Set.Science
	case Set.Dict
	case Set.HpLocking
	case Set.NewIndexLock
	case Set.StrictRead
	case Set.BlobCircref
	case Set.FoxLock
	case Set.HandleEvent
	case Set.Rushmore
	case Set.SmartFilter
	case Set.NullValue
	case Set.Lexical
		// FoxPro
	case Set.Asserts
	case Set.Lock
	case Set.MultiLocks
	case Set.SqlAnsi
	case Set.SqlBuffering
	case Set.VarCharMapping
		return false

	case Set.Ansi
	case Set.Bell
	case Set.Console
	case Set.Exclusive
	case Set.Optimize
	case Set.AutoOpen
	case Set.Defextensions
	case Set.ForceOpt
	case Set.Trimfilename
		// FoxPro
	case Set.AutoIncError
	case Set.FullPath
	case Set.Safety
	case Set.Space
	case Set.TextMerge
	case Set.HardCommit
		return true

	case Set.TextMergeDelimiters
		return <string>{"<<",">>"}

	case Set.DirCase
	case Set.FileCase
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
		return 0
	case Set.Typeahead
	case Set.Cursor
	case Set.Margin
	case Set.Message
	case Set.Cpu
	case Set.Math
	case Set.CharSet
	case Set.DevTimeOut
	case Set.Colormode
	case Set.Collation
	case Set.IdleRepeat
		return 0L

	case Set.Compatible
		return "OFF"

	case Set.AutoOrder
		return 1L

	case Set.DateCountry
		return DateCountry.American

	case Set.Autoshare
		return AutoShareMode.Auto

	case Set.Color
		return "W/N,N/W,N/N,N/N,N/W"
	case Set.DateFormat
		return "MM/DD/YYYY"

	case Set.DefaultRdd
		return "DBFNTX"

	case Set.Eol
		return e"\r\n"

	case Set.Hboutlog
		return "hb_out.log"

	case Set.Hboutloginfo
	case Set.Path
	case Set.AltFile
	case Set.Device
	case Set.PrintFile
	case Set.DelimChars
	case Set.AmExt
	case Set.PmExt
	case Set.International
	case Set.NatDLL
	case Set.NoMethod
	case Set.DateFormatNet
	case Set.DateFormatEmpty
	case Set.LastFound
	case Set.MemoExt
	case Set.Language
	case Set.Timeformat
		// FoxPro
	case Set.Database
	case Set.DebugOut
		return String.Empty
	case Set.Default
	case Set.DefaultDir
		return ""

	case Set.DirSeparator
		return System.IO.Path.DirectorySeparatorChar:ToString()

	case Set.Floatdelta
		return 0.0000000000001

	case Set.Patharray     // String[]
		return (string[]) null
	case Set.CollationTable   // byte[]
		return (byte[] ) null

	case Set.DecimalSep
	case Set.ThousandSep
	case Set.Timesep
	case Set.EpochYear
		//CASE Set.FileError
	case Set.ErrorLevel     // DWORD
		return 0U
	case Set.Decimals
		return 2U

	case Set.Digits
	case Set.Locktries
		return 10U

	case Set.MemoBlockSize
		return (word) 32

	case Set.Epoch
	case Set.EpochCent
		return 1900U

	case Set.ErrorBlock     // Codeblock
		return null

	case Set.LastScriptError    // Exception object
	case Set.SysObject
	case Set.RddInfo
		return null
	case Set.WithStack
		return Stack<object>{}
	case Set.Intl
	case Set.CollationMode
		return CollationMode.Windows

	case Set.ErrorLogFile
		return "ERROR.LOG"

	case Set.DosCodepage
		return 437L     // US American

	case Set.WinCodepage
		return 1252L    // Latin 1 / Western Europe

	case Set.FieldDelimiter
		return ","

	case Set.Delimiters
		return e"\""

	case Set.RecordDelimiter
		return CRLF

	case Set.DelimRDD
		return "DELIM"

		// 180 - 197 Harbour extensions, no value yet
		//        MEMBER FILECASE       :=  182
		//        MEMBER DIRCASE        :=  183
		//        MEMBER DBFLOCKSCHEME  :=  188
		//        MEMBER DBCODEPAGE     :=  197				// Map to Vulcan setting ?
		// Advantage
	case Set.Axslocking
	case Set.Rightschecking
	case Set.Exactkeypos
		return true

	case Set.Sql_query
	case Set.Collation_name
		return String.Empty
	case Set.Connection_handle
	case Set.Sql_table_passwords
	case Set.Sql_parameters
		return null
	case Set.Sql_timeout
		return 0

		// FoxPro
	case Set.CollateFox
		return "MACHINE"
	case Set.DataSession
		return 1
	case Set.MemoWidth
		return 50
	case Set.Refresh
		return 5.0
	case Set.Reprocess
		return 0
	case Set.Hours
		return 12L
	case Set.Seconds
		return true
	end switch

return null
