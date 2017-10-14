//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
#using XSharp
BEGIN NAMESPACE XSharp
	/// <Summary>Values that match the Visual Objects SET_* defines </Summary>
	ENUM Set
		MEMBER EXACT       := 1	
		MEMBER FIXED		 := 2	
		MEMBER DECIMALS    := 3	
		MEMBER DATEFORMAT  := 4	
		MEMBER EPOCH       := 5	
		MEMBER PATH        := 6	
		MEMBER DEFAULT		:= 7	
		MEMBER EXCLUSIVE   := 8	
		MEMBER SOFTSEEK    := 9	
		MEMBER UNIQUE      := 10	
		MEMBER DELETED     := 11	
		MEMBER CANCEL      := 12	
		MEMBER @@DEBUG     := 13	
		MEMBER TYPEAHEAD   := 14	
		MEMBER COLOR       := 15	
		MEMBER CURSOR      := 16	
		MEMBER CONSOLE     := 17	
		MEMBER ALTERNATE   := 18	
		MEMBER ALTFILE     := 19	
		MEMBER DEVICE      := 20	
		MEMBER EXTRA       := 21	
		MEMBER EXTRAFILE   := 22	
		MEMBER PRINTER     := 23	
		MEMBER PRINTFILE   := 24	
		MEMBER MARGIN      := 25	
		MEMBER BELL        := 26	
		MEMBER CONFIRM     := 27	
		MEMBER ESCAPE      := 28	
		MEMBER INSERT      := 29	
		MEMBER @@EXIT      := 30	
		MEMBER INTENSITY   := 31	
		MEMBER SCOREBOARD  := 32	
		MEMBER DELIMITERS  := 33	
		MEMBER DELIMCHARS  := 34	
		MEMBER WRAP        := 35	
		MEMBER MESSAGE     := 36	
		MEMBER MCENTER     := 37	
		MEMBER SCROLLBREAK := 38	
		
		MEMBER EVENTMASK   := 39  /* CA-Cl*pper 5.3 compatible */
		MEMBER VIDEOMODE   := 40  /* CA-Cl*pper 5.3 compatible */
		MEMBER MBLOCKSIZE  := 41  /* CA-Cl*pper 5.3 compatible */
		MEMBER MFILEEXT    := 42  /* CA-Cl*pper 5.3 compatible */
		MEMBER STRICTREAD  := 43  /* CA-Cl*pper 5.3 compatible */
		MEMBER OPTIMIZE    := 44  /* CA-Cl*pper 5.3 compatible */
		MEMBER AUTOPEN     := 45  /* CA-Cl*pper 5.3 compatible */
		MEMBER AUTORDER    := 46  /* CA-Cl*pper 5.3 compatible */
		MEMBER AUTOSHARE   := 47  /* CA-Cl*pper 5.3 compatible */ 		
		// 48 and 49 unused
		MEMBER DIGITS      := 50	// Vulcan had 39
		MEMBER NETERR      := 51	// Vulcan had 40
		MEMBER HPLOCK      := 52    // Vulcan had 41
		MEMBER @@ANSI      := 53	// Vulcan had 44
		MEMBER @@YIELD     := 54	// Vulcan had 45
		MEMBER LOCKTRIES   := 55	// Vulcan had 46
		MEMBER AmExt		 := 56
		MEMBER AmPm		    := 57
		MEMBER PmExt	    := 58
		MEMBER Century	    := 59
		MEMBER DigitFixed  := 60
		MEMBER DecimalSep  := 60
		MEMBER ThousandSep := 61
		MEMBER TimeSep     := 62
		MEMBER FieldStore  := 63
		MEMBER Science     := 64
		// 65 - 97 unused
		MEMBER DICT        := 98	
		MEMBER INTL        := 99	
		
		// 100 - 117 Harbour extensions
		MEMBER LANGUAGE       :=  100  
		MEMBER IDLEREPEAT     :=  101 
		MEMBER FILECASE       :=  102 
		MEMBER DIRCASE        :=  103 
		MEMBER DIRSEPARATOR   :=  104 
		MEMBER EOF            :=  105 
		MEMBER HARDCOMMIT     :=  106 
		MEMBER FORCEOPT       :=  107 
		MEMBER DBFLOCKSCHEME  :=  108 
		MEMBER DEFEXTENSIONS  :=  109 
		MEMBER EOL            :=  110 
		MEMBER TRIMFILENAME   :=  111 
		MEMBER HBOUTLOG       :=  112 
		MEMBER HBOUTLOGINFO   :=  113 
		MEMBER CODEPAGE       :=  114 
		MEMBER OSCODEPAGE     :=  115 
		MEMBER TIMEFORMAT     :=  116 
		MEMBER DBCODEPAGE     :=  117				// -> To RDD Settings ?
		
		// 118 - 149 unused
		// Advantage extensions
		MEMBER AXSLOCKING           := 150
		MEMBER RIGHTSCHECKING       := 151
		MEMBER CONNECTION_HANDLE    := 152
		MEMBER EXACTKEYPOS          := 153
		MEMBER SQL_QUERY            := 154
		MEMBER SQL_TABLE_PASSWORDS  := 155
		MEMBER COLLATION_NAME       := 156
		
		// Start of user values
		MEMBER User           := 200
		
	END ENUM
END NAMESPACE
#region Defines
	DEFINE _SET_EXACT       := Set.Exact		
	DEFINE _SET_FIXED       := Set.Fixed 		
	DEFINE _SET_DECIMALS    := Set.Decimals		
	DEFINE _SET_DATEFORMAT  := Set.DATEFORMAT  	
	DEFINE _SET_EPOCH       := Set.EPOCH       	
	DEFINE _SET_PATH        := Set.PATH        	
	DEFINE _SET_DEFAULT     := Set.DEFAULT     	
	
	DEFINE _SET_EXCLUSIVE   := Set.EXCLUSIVE   	
	DEFINE _SET_SOFTSEEK    := Set.SOFTSEEK    	
	DEFINE _SET_UNIQUE      := Set.UNIQUE      	
	DEFINE _SET_DELETED     := Set.DELETED     	
	
	DEFINE _SET_CANCEL      := Set.CANCEL
	DEFINE _SET_DEBUG       := Set.DEBUG       	
	DEFINE _SET_TYPEAHEAD   := Set.TYPEAHEAD   	
	
	DEFINE _SET_COLOR       := Set.COLOR       	
	DEFINE _SET_CURSOR      := Set.CURSOR      	
	DEFINE _SET_CONSOLE     := Set.CONSOLE     	
	DEFINE _SET_ALTERNATE   := Set.ALTERNATE   	
	DEFINE _SET_ALTFILE     := Set.ALTFILE     	
	DEFINE _SET_DEVICE      := Set.DEVICE      	
	DEFINE _SET_EXTRA       := Set.EXTRA       	
	DEFINE _SET_EXTRAFILE   := Set.EXTRAFILE   	
	DEFINE _SET_PRINTER     := Set.PRINTER     	
	DEFINE _SET_PRINTFILE   := Set.PRINTFILE   	
	DEFINE _SET_MARGIN      := Set.MARGIN      	
	
	DEFINE _SET_BELL        := Set.BELL        	
	DEFINE _SET_CONFIRM     := Set.CONFIRM     	
	DEFINE _SET_ESCAPE      := Set.ESCAPE      	
	DEFINE _SET_INSERT      := Set.INSERT      	
	DEFINE _SET_EXIT        := Set.EXIT        	
	DEFINE _SET_INTENSITY   := Set.INTENSITY   	
	DEFINE _SET_SCOREBOARD  := Set.SCOREBOARD  	
	DEFINE _SET_DELIMITERS  := Set.DELIMITERS  	
	DEFINE _SET_DELIMCHARS  := Set.DELIMCHARS  	
	
	DEFINE _SET_WRAP        := Set.WRAP        	
	DEFINE _SET_MESSAGE     := Set.MESSAGE     	
	DEFINE _SET_MCENTER     := Set.MCENTER     	
	DEFINE _SET_SCROLLBREAK := Set.SCROLLBREAK 	
	
	DEFINE _SET_EVENTMASK   := Set.EVENTMASK   	
	DEFINE _SET_VIDEOMODE   := Set.VIDEOMODE   	
	DEFINE _SET_MBLOCKSIZE  := Set.MBLOCKSIZE  	
	DEFINE _SET_MFILEEXT    := Set.MFILEEXT    	
	DEFINE _SET_STRICTREAD  := Set.STRICTREAD  	
	DEFINE _SET_OPTIMIZE    := Set.OPTIMIZE    	
	DEFINE _SET_AUTOPEN     := Set.AUTOPEN     	
	DEFINE _SET_AUTORDER    := Set.AUTORDER    	
	DEFINE _SET_AUTOSHARE   := Set.AUTOSHARE   	
	
	DEFINE _SET_DIGITS      := Set.DIGITS      	
	DEFINE _SET_NETERR      := Set.NETERR      	
	DEFINE _SET_HPLOCK      := Set.HPLOCK      	
	DEFINE _SET_ANSI        := Set.ANSI        	
	DEFINE _SET_YIELD       := Set.YIELD       	
	DEFINE _SET_LOCKTRIES   := Set.LOCKTRIES   	
	DEFINE _SET_AMEXT		:= Set.AmExt		
	DEFINE _SET_AMPM		:= Set.AmPm			
	DEFINE _SET_PMEXT		:= Set.PmExt		
	DEFINE _SET_CENTURY		:= Set.Century		
	DEFINE _SET_DIGITFIXED	:= Set.DigitFixed	
	DEFINE _SET_DECIMALSEP  := Set.DecimalSep   
	DEFINE _SET_THOUSANDSEP := Set.ThousandSep  
	DEFINE _SET_TIMESEP     := Set.TimeSep      
	DEFINE _SET_FIELDSTORE	:= Set.FieldStore	
	DEFINE _SET_SCIENCE  	:= Set.Science		
	
	DEFINE _SET_DICT        := Set.Dict			
	DEFINE _SET_INTL        := Set.Intl			
	
	DEFINE _SET_LANGUAGE       :=  Set.LANGUAGE      	
	DEFINE _SET_IDLEREPEAT     :=  Set.IDLEREPEAT    	
	DEFINE _SET_FILECASE       :=  Set.FILECASE      	
	DEFINE _SET_DIRCASE        :=  Set.DIRCASE       	
	DEFINE _SET_DIRSEPARATOR   :=  Set.DIRSEPARATOR  	
	DEFINE _SET_EOF            :=  Set.EOF           	
	DEFINE _SET_HARDCOMMIT     :=  Set.HARDCOMMIT    	
	DEFINE _SET_FORCEOPT       :=  Set.FORCEOPT      	
	DEFINE _SET_DBFLOCKSCHEME  :=  Set.DBFLOCKSCHEME 	
	DEFINE _SET_DEFEXTENSIONS  :=  Set.DEFEXTENSIONS 	
	DEFINE _SET_EOL            :=  Set.EOL           	
	DEFINE _SET_TRIMFILENAME   :=  Set.TRIMFILENAME  	
	DEFINE _SET_HBOUTLOG       :=  Set.HBOUTLOG      	
	DEFINE _SET_HBOUTLOGINFO   :=  Set.HBOUTLOGINFO  	
	DEFINE _SET_CODEPAGE       :=  Set.CODEPAGE      	
	DEFINE _SET_OSCODEPAGE     :=  Set.OSCODEPAGE    	
	DEFINE _SET_TIMEFORMAT     :=  Set.TIMEFORMAT    	
	DEFINE _SET_DBCODEPAGE     :=  Set.DBCODEPAGE    	
	
	// Advantage additions
	DEFINE _SET_AXSLOCKING           := Set.AXSLOCKING         
	DEFINE _SET_RIGHTSCHECKING       := Set.RIGHTSCHECKING     
	DEFINE _SET_CONNECTION_HANDLE    := Set.CONNECTION_HANDLE  
	DEFINE _SET_EXACTKEYPOS          := Set.EXACTKEYPOS        
	DEFINE _SET_SQL_QUERY            := Set.SQL_QUERY          
	DEFINE _SET_SQL_TABLE_PASSWORDS  := Set.SQL_TABLE_PASSWORDS
	DEFINE _SET_COLLATION_NAME       := Set.COLLATION_NAME     
	
#endregion
