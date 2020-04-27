//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp
USING XSharp.RDD.Enums
BEGIN NAMESPACE XSharp
    /// <summary>Values that match the Visual Objects SET_* defines </summary>
    /// <remarks>Global settings are stored in the RuntimeState and are thread specific.
    /// The numeric value of these sets indicate the key of the setting in the settings dictionary on the runtimestate.
    /// </remarks>
    /// <seealso cref='T:XSharp.RuntimeState'>RuntimeState</seealso>
    /// <seealso cref='M:XSharp.RuntimeState.GetValue``1(XSharp.Set)'>RuntimeState.GetValue</seealso>
    /// <seealso cref='M:XSharp.RuntimeState.SetValue``1(XSharp.Set,``0)'>RuntimeState.SetValue</seealso>
    ENUM Set
        MEMBER Exact       := 1			// LOGIC
        MEMBER Fixed	   := 2			// LOGIC
        MEMBER Decimals    := 3			// INT
        MEMBER DateFormat  := 4			// STRING
        MEMBER Epoch       := 5			// INT
        MEMBER Path        := 6			// STRING
        MEMBER Default	   := 7			// STRING
        MEMBER Exclusive   := 8			// LOGIC
        MEMBER Softseek    := 9			// LOGIC
        MEMBER Unique      := 10		// LOGIC
        MEMBER Deleted     := 11		// LOGIC
        MEMBER Cancel      := 12		// LOGIC
        MEMBER @@Debug     := 13	
        MEMBER Typeahead   := 14		// INT
        MEMBER Color       := 15		// STRING
        MEMBER Cursor      := 16		// INT
        MEMBER Console     := 17		// LOGIC
        MEMBER Alternate   := 18		// LOGIC
        MEMBER AltFile     := 19		// STRING
        MEMBER Device      := 20		// STRING
        MEMBER Extra       := 21		// LOGIC
        MEMBER ExtraFile   := 22		// STRING
        MEMBER Printer     := 23		// LOGIC
        MEMBER PrintFile   := 24		// STRING
        MEMBER Margin      := 25		// INT
        MEMBER Bell        := 26		// LOGIC
        MEMBER Confirm     := 27		// LOGIC
        MEMBER Escape      := 28		// LOGIC
        MEMBER Insert      := 29		// LOGIC
        MEMBER Exit        := 30		// LOGIC
        MEMBER Intensity   := 31		// LOGIC
        MEMBER Scoreboard  := 32		// LOGIC
        MEMBER Delimiters  := 33		// STRING
        MEMBER DelimChars  := 34		// STRING   // note: used for @ Say .. GET in Clipper
        MEMBER Wrap        := 35		// LOGIC
        MEMBER Message     := 36		// INT
        MEMBER Mcenter     := 37		// LOGIC
        MEMBER ScrollBreak := 38		// LOGIC
        MEMBER Errrorlog   := 39		// LOGIC

        MEMBER Neterr      	:= 40	// LOGIC
        MEMBER Digits      	:= 41	// INT   
        MEMBER AmExt		:= 42	// STRING
        MEMBER PmExt	    := 43	// STRING
        MEMBER Ansi      	:= 44	// LOGIC 
        MEMBER Yield     	:= 45	// LOGIC 
        MEMBER Locktries   	:= 46	// INT   
        MEMBER AmPm		    := 47	// LOGIC
        MEMBER Century	    := 48	// LOGIC
        MEMBER DigitFixed  	:= 49	// LOGIC
        MEMBER DecimalSep  	:= 50	// DWORD
        MEMBER ThousandSep 	:= 51	// DWORD
        MEMBER Timesep     	:= 52	// DWORD
        MEMBER Fieldstore  	:= 53   // Logic
        MEMBER Science     	:= 54	// LOGIC
        MEMBER Cpu			:= 55	// INT
        MEMBER Floatdelta	:= 56	// System.Double
        MEMBER Math			:= 57	// INT
        MEMBER International:= 58	// STRING
        MEMBER DateCountry  := 59	// INT
        MEMBER DefaultDir   := 60  // STRING

        // 61 - 69 unused

        // X# helper state
        MEMBER EpochCent     := 70		// Numeric
        MEMBER EpochYear     := 71		// Numeric
        MEMBER DateFormatNet := 72		// String
        MEMBER DateFormatEmpty := 73    // String
        // 74 - 75 unused
        MEMBER NoMethod		:= 76	// STRING
        // 77 unused 
        MEMBER Patharray    := 78	// String[]
        MEMBER NatDLL		:= 79   // string
        MEMBER CollationTable := 80  // byte[]
        MEMBER ErrorLevel   := 81  // DWORD
        MEMBER ErrorBlock   := 82  // Codeblock
        MEMBER LastRddError := 84   // Exception object
        // 85 unused
        MEMBER LastFound    := 86   // Last file found with File()
        MEMBER FileError    := 87   // Last File error code
        MEMBER FileException:= 88   // Last File exception

        MEMBER DelimRDD         := 89
        MEMBER FieldDelimiter   := 90
        MEMBER RecordDelimiter  := 91

        // 92 - 97 unused
        MEMBER Dict        := 98	// LOGIC
        MEMBER Intl        := 99	// CollationMode

        // Vulcan RDDInfo Settings
        MEMBER RddInfo		:= 100      // no value
        MEMBER MemoBlockSize:= 101		// INT
        MEMBER DefaultRdd	:= 102		// STRING
        MEMBER MemoExt	    := 103		// STRING
        MEMBER AutoOpen     := 104		// LOGIC
        MEMBER AutoOrder    := 105		// 0 or 1
        MEMBER HpLocking    := 106      // LOGIC 
        MEMBER NewIndexLock := 107      // LOGIC 
        MEMBER Autoshare    := 108		// 0 or 1
        MEMBER StrictRead   := 109		// LOGIC
        MEMBER BlobCircref	:= 110		// LOGIC
        MEMBER Optimize     := 111		// LOGIC
        MEMBER FoxLock      := 112		// LOGIC

        MEMBER RddInfoMax   := 119      // no value

        MEMBER WinCodepage	:= 120		// Numeric
        MEMBER DosCodepage	:= 121		// Numeric
        MEMBER CollationMode:= 122		// CollationMode 

        // FoxPro settings
        /// <summary>FoxPro: Is Textmerge enabled.</summary>
        MEMBER TextMerge    := 130      // Logic 
        /// <summary>FoxPro: Should CDX() and similar functions return full paths</summary>
        MEMBER FullPath     := 131      // Logic 
        /// <summary>FoxPro: Add Space between ? and ?? field expressions</summary>
        MEMBER Space        := 132      // Logic 
        
        MEMBER FoxCollate   := 133
        MEMBER Near         := 134
        MEMBER MemoWidth    := 135
        MEMBER SqlAnsi      := 136

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
DEFINE _SET_EXTRA       := Set.Extra       	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EXTRAFILE   := Set.ExtraFile
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
/// <include file="CoreComments.xml" path="comments/Set/*" />
DEFINE _SET_HBOUTLOGINFO   :=  Set.Hboutloginfo  	
/// <include file="CoreComments.xml" path="comments/Set/*" />
DEFINE _SET_CODEPAGE       :=  Set.Codepage      	
/// <include file="CoreComments.xml" path="comments/Set/*" />
DEFINE _SET_OSCODEPAGE     :=  Set.Oscodepage    	
/// <include file="CoreComments.xml" path="comments/Set/*" />
DEFINE _SET_TIMEFORMAT     :=  Set.Timeformat    	
/// <include file="CoreComments.xml" path="comments/Set/*" />
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
        CASE Set.Console     	
        CASE Set.Alternate   	
        CASE Set.Extra       	
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
        CASE Set.Errrorlog   	
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
        CASE Set.Near         
        CASE Set.SqlAnsi     
        CASE Set.HandleEvent 
        CASE Set.Rushmore    
        CASE Set.SmartFilter 
        CASE Set.NullValue   
        CASE Set.Lexical
        CASE Set.HardCommit
            RETURN FALSE

        CASE Set.Ansi           
        CASE Set.Bell
        CASE Set.Exclusive   	
        CASE Set.Space       
        CASE Set.FullPath    
        CASE Set.Optimize     
        CASE Set.AutoOpen
        CASE Set.Defextensions
        CASE Set.ForceOpt
        CASE Set.Trimfilename
        CASE Set.TextMerge    
			RETURN TRUE
       
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
        CASE Set.MemoWidth   
        CASE Set.CharSet     
        CASE Set.DevTimeOut  
        CASE Set.Colormode   
        CASE Set.Collation
        CASE Set.IdleRepeat
            RETURN 0L

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
        CASE Set.ExtraFile   	
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
        CASE Set.FoxCollate
        CASE Set.Language
        CASE Set.Timeformat
            RETURN String.Empty
        CASE Set.Default
        CASE Set.DefaultDir
            RETURN System.Environment.CurrentDirectory

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
        CASE Set.FileError   
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
        
        CASE Set.LastRddError   // Exception object
            RETURN NULL        
        CASE Set.FileException  // Last File exception
            RETURN NULL

        CASE Set.Intl           
        CASE Set.CollationMode
            RETURN CollationMode.Windows



        CASE Set.DosCodepage
            RETURN 437L

        CASE Set.WinCodepage
            RETURN 1250L

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
        END SWITCH



RETURN NULL


