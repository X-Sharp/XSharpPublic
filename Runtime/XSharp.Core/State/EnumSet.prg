//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp
BEGIN NAMESPACE XSharp
    /// <summary>Values that match the Visual Objects SET_* defines </summary>
    /// <remarks>Global settings are stored in the RuntimeState and are thread specific.
    /// The numeric value of these sets indicate the key of the setting in the settings dictionary on the runtimestate.
    /// </remarks>
    /// <seealso cref='T:XSharp.RuntimeState'>RuntimeState</seealso>
    /// <seealso cref='M:XSharp.RuntimeState.GetValue``1(System.Int32)'>RuntimeState.GetValue</seealso>
    /// <seealso cref='M:XSharp.RuntimeState.SetValue``1(System.Int32,``0)'>RuntimeState.SetValue</seealso>
    ENUM Set
        MEMBER EXACT       := 1			// LOGIC
        MEMBER FIXED	   := 2			// LOGIC
        MEMBER DECIMALS    := 3			// INT
        MEMBER DATEFORMAT  := 4			// STRING
        MEMBER EPOCH       := 5			// INT
        MEMBER PATH        := 6			// STRING
        MEMBER DEFAULT	   := 7			// STRING
        MEMBER EXCLUSIVE   := 8			// LOGIC
        MEMBER SOFTSEEK    := 9			// LOGIC
        MEMBER UNIQUE      := 10		// LOGIC
        MEMBER DELETED     := 11		// LOGIC
        MEMBER CANCEL      := 12		// LOGIC
        MEMBER @@DEBUG     := 13	
        MEMBER TYPEAHEAD   := 14		// INT
        MEMBER COLOR       := 15		// STRING
        MEMBER CURSOR      := 16		// INT
        MEMBER CONSOLE     := 17		// LOGIC
        MEMBER ALTERNATE   := 18		// LOGIC
        MEMBER ALTFILE     := 19		// STRING
        MEMBER DEVICE      := 20		// STRING
        MEMBER EXTRA       := 21		// LOGIC
        MEMBER EXTRAFILE   := 22		// STRING
        MEMBER PRINTER     := 23		// LOGIC
        MEMBER PRINTFILE   := 24		// STRING
        MEMBER MARGIN      := 25		// INT
        MEMBER BELL        := 26		// LOGIC
        MEMBER CONFIRM     := 27		// LOGIC
        MEMBER ESCAPE      := 28		// LOGIC
        MEMBER INSERT      := 29		// LOGIC
        MEMBER EXIT        := 30		// LOGIC
        MEMBER INTENSITY   := 31		// LOGIC
        MEMBER SCOREBOARD  := 32		// LOGIC
        MEMBER DELIMITERS  := 33		// STRING
        MEMBER DELIMCHARS  := 34		// STRING
        MEMBER WRAP        := 35		// LOGIC
        MEMBER MESSAGE     := 36		// INT
        MEMBER MCENTER     := 37		// LOGIC
        MEMBER SCROLLBREAK := 38		// LOGIC
        MEMBER ERRRORLOG   := 39		// LOGIC

        MEMBER NETERR      	:= 40	// LOGIC
        MEMBER DIGITS      	:= 41	// INT   
        MEMBER AMEXT		:= 42	// STRING
        MEMBER PMEXT	    := 43	// STRING
        MEMBER ANSI      	:= 44	// LOGIC 
        MEMBER YIELD     	:= 45	// LOGIC 
        MEMBER LOCKTRIES   	:= 46	// INT   
        MEMBER AMPM		    := 47	// LOGIC
        MEMBER CENTURY	    := 48	// LOGIC
        MEMBER DIGITFIXED  	:= 49	// LOGIC
        MEMBER DECIMALSEP  	:= 50	// INT
        MEMBER THOUSANDSEP 	:= 51	// INT
        MEMBER TIMESEP     	:= 52	// INT
        MEMBER FIELDSTORE  	:= 53
        MEMBER SCIENCE     	:= 54	// LOGIC
        MEMBER CPU			:= 55	// INT
        MEMBER FLOATDELTA	:= 56	// System.Double
        MEMBER MATH			:= 57	// INT
        MEMBER INTERNATIONAL:= 58	// STRING
        MEMBER DATECOUNTRY  := 59	// INT

        // 60 - 69 unused

        // X# helper state
        MEMBER EpochCent     := 70		// Numeric
        MEMBER EpochYear     := 71		// Numeric
        MEMBER DateFormatNet := 72		// String
        MEMBER DateFormatEmpty := 73    // String
        MEMBER OPTIONVO11	:= 74	// Logic
        MEMBER OPTIONOVF	:= 75	// Logic
        MEMBER NOMETHOD		:= 76	// STRING
        MEMBER APPMODULE	:= 77	// System.Reflection.Module
        MEMBER PATHARRAY    := 78	// String[]
        MEMBER NatDLL		:= 79   // string
        MEMBER CollationTable := 80  // byte[]
        MEMBER ErrorLevel   := 81  // DWORD
        MEMBER ErrorBlock   := 82  // Codeblock
        MEMBER OPTIONVO13	:= 83	// Logic
        MEMBER LastRddError := 84   // Exception object
        MEMBER Dialect      := 85   // XSharpDialect value
        // 86 - 97 unused
        MEMBER DICT        := 98	// LOGIC
        MEMBER INTL        := 99	// LOGIC

        // Vulcan RDDInfo Settings
        MEMBER RDDINFO		:= 100
        MEMBER MEMOBLOCKSIZE:= 101		// INT
        MEMBER DEFAULTRDD	:= 102		// STRING
        MEMBER MEMOEXT	    := 103		// STRING
        MEMBER AUTOOPEN     := 104		// LOGIC
        MEMBER AUTOORDER    := 105		// LOGIC
        MEMBER HPLOCKING    := 106      // LOGIC 
        MEMBER NEWINDEXLOCK := 107      // LOGIC 
        MEMBER AUTOSHARE    := 108		// LOGIC
        MEMBER STRICTREAD   := 109		// LOGIC
        MEMBER BLOBCIRCREF	:= 110		// LOGIC
        MEMBER OPTIMIZE     := 111		// LOGIC
        MEMBER FOXLOCK      := 112		// LOGIC
        MEMBER WINCODEPAGE	:= 113		// Numeric
        MEMBER DOSCODEPAGE	:= 114		// Numeric
        MEMBER COLLATIONMODE:= 115		// COLLATIONMODE (STRING)

        // FoxPro settings
        MEMBER TextMerge    := 130      // Logic Is Textmerge enabled 
        MEMBER FullPath     := 131      // Logic Should CDX() and similar functions return full paths
        MEMBER Space        := 132      // Logic Space between ? and ?? field expressions
        

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


        // 180 - 197 Harbour extensions
        MEMBER LANGUAGE       :=  180  
        MEMBER IDLEREPEAT     :=  181 
        MEMBER FILECASE       :=  182			
        MEMBER DIRCASE        :=  183 
        MEMBER DIRSEPARATOR   :=  184 
        MEMBER EOF            :=  185 
        MEMBER HARDCOMMIT     :=  186 
        MEMBER FORCEOPT       :=  187 
        MEMBER DBFLOCKSCHEME  :=  188 
        MEMBER DEFEXTENSIONS  :=  189 
        MEMBER EOL            :=  190 
        MEMBER TRIMFILENAME   :=  191	
        MEMBER HBOUTLOG       :=  192 
        MEMBER HBOUTLOGINFO   :=  193 
        MEMBER CODEPAGE       :=  194				// Map to Vulcan setting ?
        MEMBER OSCODEPAGE     :=  195				// Map to Vulcan setting ?
        MEMBER TIMEFORMAT     :=  196			
        MEMBER DBCODEPAGE     :=  197				// Map to Vulcan setting ?
        
        // Start of user values
        MEMBER User           := 200
        // Advantage extensions
        MEMBER AXSLOCKING           := User+1
        MEMBER RIGHTSCHECKING       := User+2
        MEMBER CONNECTION_HANDLE    := User+3
        MEMBER EXACTKEYPOS          := User+4
        MEMBER SQL_QUERY            := User+5
        MEMBER SQL_TABLE_PASSWORDS  := User+6
        MEMBER COLLATION_NAME       := User+7
        MEMBER SQL_TIMEOUT          := User+8
        MEMBER SQL_PARAMETERS       := User+9
        
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
DEFINE _SET_DATEFORMAT  := Set.DATEFORMAT  	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EPOCH       := Set.EPOCH       	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_PATH        := Set.PATH        	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEFAULT     := Set.DEFAULT     	
    
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EXCLUSIVE   := Set.EXCLUSIVE   	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SOFTSEEK    := Set.SOFTSEEK    	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_UNIQUE      := Set.UNIQUE      	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DELETED     := Set.DELETED     	
    
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CANCEL      := Set.CANCEL
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEBUG       := Set.DEBUG       	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_TYPEAHEAD   := Set.TYPEAHEAD   	
    
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_COLOR       := Set.COLOR       	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CURSOR      := Set.CURSOR      	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CONSOLE     := Set.CONSOLE     	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_ALTERNATE   := Set.ALTERNATE   	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_ALTFILE     := Set.ALTFILE     	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEVICE      := Set.DEVICE      	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EXTRA       := Set.EXTRA       	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EXTRAFILE   := Set.EXTRAFILE   	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_PRINTER     := Set.PRINTER     	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_PRINTFILE   := Set.PRINTFILE   	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MARGIN      := Set.MARGIN      	
    
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_BELL        := Set.BELL        	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CONFIRM     := Set.CONFIRM     	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_ESCAPE      := Set.ESCAPE      	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_INSERT      := Set.INSERT      	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EXIT        := Set.EXIT        	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_INTENSITY   := Set.INTENSITY   	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SCOREBOARD  := Set.SCOREBOARD  	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DELIMITERS  := Set.DELIMITERS  	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DELIMCHARS  := Set.DELIMCHARS  	
    
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_WRAP        := Set.WRAP        	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MESSAGE     := Set.MESSAGE     	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MCENTER     := Set.MCENTER     	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SCROLLBREAK := Set.SCROLLBREAK 	

// 48 and 49 unused
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DIGITS      	:= Set.DIGITS      
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_NETERR      	:= Set.NETERR      
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_ANSI      		:= Set.ANSI      
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_YIELD     		:= Set.YIELD     
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_LOCKTRIES   	:= Set.LOCKTRIES   
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AMEXT			:= Set.AMEXT		
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AMPM			:= Set.AMPM		   
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_PMEXT	    	:= Set.PMEXT	   
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CENTURY	    	:= Set.CENTURY	   
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DIGITFIXED  	:= Set.DIGITFIXED  
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DECIMALSEP  	:= Set.DECIMALSEP  
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_THOUSANDSEP 	:= Set.THOUSANDSEP 
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_TIMESEP     	:= Set.TIMESEP     
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FIELDSTORE  	:= Set.FIELDSTORE  
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SCIENCE     	:= Set.SCIENCE     
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CPU				:= Set.CPU			
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FLOATDELTA		:= Set.FLOATDELTA	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MATH			:= Set.MATH			
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_INTERNATIONAL	:= Set.INTERNATIONAL
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DATECOUNTRY		:= Set.DATECOUNTRY
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DICT			:= Set.Dict			
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_INTL			:= Set.Intl		

/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_BLOB_CIRCULAR_ARRAY_REF := Set.BLOBCIRCREF
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_USER := Set.User

    
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_COLLATIONMODE	:= Set.COLLATIONMODE	

// Vulcan RDDInfo Settings
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_RDDINFO				:= Set.RDDINFO		
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MEMOBLOCKSIZE		:= Set.MEMOBLOCKSIZE
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEFAULTRDD			:= Set.DEFAULTRDD	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_MEMOEXT	    		:= Set.MEMOEXT	    
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AUTOOPEN    		:= Set.AUTOOPEN     
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AUTOORDER   		:= Set.AUTOORDER    
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_HPLOCKING   		:= Set.HPLOCKING    
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_HPLOCK      		:= Set.HPLOCKING    
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_NEWINDEXLOCK		:= Set.NEWINDEXLOCK 
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AUTOSHARE   		:= Set.AUTOSHARE    
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_STRICTREAD  		:= Set.STRICTREAD   
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_BLOBCIRCREF			:= Set.BLOBCIRCREF	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_OPTIMIZE    		:= Set.OPTIMIZE     
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FOXLOCK     		:= Set.FOXLOCK      
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_WINCODEPAGE			:= Set.WINCODEPAGE	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DOSCODEPAGE			:= Set.DOSCODEPAGE	

// Harbour extensions
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_LANGUAGE       :=  Set.LANGUAGE      	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_IDLEREPEAT     :=  Set.IDLEREPEAT    	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FILECASE       :=  Set.FILECASE      	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DIRCASE        :=  Set.DIRCASE       	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DIRSEPARATOR   :=  Set.DIRSEPARATOR  	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EOF            :=  Set.EOF           	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_HARDCOMMIT     :=  Set.HARDCOMMIT    	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_FORCEOPT       :=  Set.FORCEOPT      	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DBFLOCKSCHEME  :=  Set.DBFLOCKSCHEME 	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DEFEXTENSIONS  :=  Set.DEFEXTENSIONS 	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EOL            :=  Set.EOL           	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_TRIMFILENAME   :=  Set.TRIMFILENAME  	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_HBOUTLOG       :=  Set.HBOUTLOG      	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_HBOUTLOGINFO   :=  Set.HBOUTLOGINFO  	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CODEPAGE       :=  Set.CODEPAGE      	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_OSCODEPAGE     :=  Set.OSCODEPAGE    	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_TIMEFORMAT     :=  Set.TIMEFORMAT    	
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_DBCODEPAGE     :=  Set.DBCODEPAGE    	
    
// Advantage additions
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_AXSLOCKING           := Set.AXSLOCKING         
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_RIGHTSCHECKING       := Set.RIGHTSCHECKING     
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_CONNECTION_HANDLE    := Set.CONNECTION_HANDLE  
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_EXACTKEYPOS          := Set.EXACTKEYPOS        
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SQL_QUERY            := Set.SQL_QUERY          
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SQL_TABLE_PASSWORDS  := Set.SQL_TABLE_PASSWORDS
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_COLLATION_NAME       := Set.COLLATION_NAME     
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SQL_PARAMETERS       :=  Set.SQL_PARAMETERS     
/// <include file="CoreComments.xml" path="Comments/Set/*" />
DEFINE _SET_SQL_TIMEOUT          :=  Set.SQL_TIMEOUT        


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

    
#endregion


DEFINE _MAX_PATH := 260
DEFINE MAX_PATH := 260





