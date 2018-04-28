//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.IO
// The classes below are simple. No properties, but all public fields.

BEGIN NAMESPACE XSharp.RDD

CLASS DbEvalInfo  
	PUBLIC ScopeInfo AS DbScopeInfo
	PUBLIC Block	 AS ICodeBlock
END CLASS 

CLASS DbFilterInfo
	PUBLIC FilterText	AS STRING
	PUBLIC FilterBlock	AS ICodeBlock
	PUBLIC Active		AS LOGIC
	PUBLIC Optimized	AS LOGIC
	METHOD Clear() AS VOID
		SELF:FilterBlock := NULL
		SELF:FilterText	 := NULL
		SELF:Active		 := FALSE
		SELF:Optimized	 := FALSE
		RETURN   
	METHOD Clone AS DbFilterInfo
		LOCAL oCLone AS DbFilterInfo
		oCLone := DbFilterInfo{}
		oCLone:FilterBlock := SELF:FilterBlock
		oClone:FilterText  := SELF:FilterText
		oClone:Optimized   := SELF:Optimized
		oClone:Active	   := FALSE
		RETURN oClone
END CLASS 
                
STRUCTURE DbLockInfo
	PUBLIC RecId		AS OBJECT
	PUBLIC @@Method		AS LockMethod
	PUBLIC Result		AS LOGIC
	ENUM LockMethod
		MEMBER Exclusive := 1
		MEMBER Multiple  := 2
		MEMBER File 	 := 3
	END ENUM	
END STRUCTURE

CLASS DbOpenInfo  
	PUBLIC ReadOnly 	AS LOGIC
	PUBLIC Shared		AS LOGIC
	PUBLIC Alias		AS STRING
	PUBLIC Extension    AS STRING
	PUBLIC FileName		AS STRING
	PUBLIC WorkArea		AS LONG    
	
	CONSTRUCTOR()
		
	CONSTRUCTOR(sFileName AS STRING, sAlias AS STRING, liWorkArea AS LONG, lShared AS LOGIC, lReadOnly AS LOGIC)
		FileName 	:= sFileName
        Extension   := Path.GetExtension(sFileName)
		Alias	 	:= sAlias
		WorkArea	:= liWorkArea
		Shared		:= lShared
		ReadOnly	:= lReadOnly

	public property FileMode as DWORD 
	get
		local nMode as DWORD
		nMode := FO_COMPAT
		if (Shared)
			nMode |= FO_SHARED
		else
			nMode |= FO_EXCLUSIVE
		endif
		if (ReadOnly)
			nMode |= FO_READ
		else
			nMode |= FO_READWRITE
		endif
		return nMode
	end get
	end property
END CLASS  

CLASS DbOrderCondInfo                     
	PUBLIC Active			AS LOGIC
	PUBLIC Additive			AS LOGIC
	PUBLIC All				AS LOGIC
	PUBLIC Custom			AS LOGIC
	PUBLIC @@Descending 	AS LOGIC
	PUBLIC EvalBlock		AS ICodeBlock 
	PUBLIC ForBlock			AS ICodeBlock 
	PUBLIC ForExpression	AS STRING    
	PUBLIC NextCount		AS LONG
	PUBLIC NoOptimize		AS LOGIC
	PUBLIC RecNo			AS LONG
	PUBLIC Rest				AS LOGIC
	PUBLIC Scoped			AS LOGIC
	PUBLIC StartRecNo		AS LONG
	PUBLIC StepSize			AS LONG
	PUBLIC UseCurrent		AS LOGIC
	PUBLIC WhileBlock		AS ICodeBlock 	
END CLASS

CLASS DbOrderCreateInfo 
	PUBLIC BagName		AS STRING
	PUBLIC Order		AS OBJECT
	PUBLIC Expression	AS STRING
	PUBLIC Unique		AS LOGIC
	PUBLIC Block		AS ICodeBlock
	PUBLIC OrdCondInfo	AS DbOrderCondInfo
END CLASS

CLASS DbOrderInfo  
	PUBLIC AllTags		AS LOGIC
	PUBLIC Expression	AS OBJECT
	PUBLIC Order		AS OBJECT
	PUBLIC BagName		AS STRING
	//PUBLIC Result		AS OBJECT 	// Is this needed ?
END CLASS

CLASS DbRelInfo 
	PUBLIC Key			AS STRING
	PUBLIC Block		AS ICodeBlock
	PUBLIC Child		AS IRDD	
	PUBLIC Parent		AS IRDD
END CLASS


CLASS DbScopeInfo 
	PUBLIC IgnoreDuplicates AS LOGIC
	PUBLIC IgnoreFilter		AS LOGIC
	PUBLIC IncludeDeleted	AS LOGIC
	PUBLIC ForBlock			AS ICodeBlock
	PUBLIC ForExpression	AS STRING
	PUBLIC Last				AS LOGIC 
	PUBLIC NextCount		AS LONG
	PUBLIC RecId			AS OBJECT
	PUBLIC Rest				AS LOGIC     
	PUBLIC WhileBlock		AS ICodeBlock
	PUBLIC WhileExpression	AS STRING    
	
	CONSTRUCTOR()
		SELF:Clear()

	PUBLIC METHOD Clear() AS VOID
		IgnoreDuplicates := FALSE
		IgnoreFilter	 := FALSE
		IncludeDeleted 	 := FALSE
		ForBlock		 := NULL
		ForExpression	 := NULL
		Last			 := FALSE
		NextCount		 := 0
		RecId			 := NULL
		Rest			 := FALSE
		WhileBlock		 := NULL
		WhileExpression  := NULL

	METHOD Clone AS DbScopeInfo
		LOCAL oClone AS DbScopeInfo
		oClone := DbScopeInfo{}
		oClone:IgnoreDuplicates     := SELF:IgnoreDuplicates 
		oClone:IgnoreFilter	        := SELF:IgnoreFilter	 
		oClone:IncludeDeleted       := SELF:IncludeDeleted
		oClone:ForBlock		        := SELF:ForBlock		
		oClone:ForExpression        := SELF:ForExpression
		oClone:Last			        := SELF:Last			
		oClone:NextCount	        := SELF:NextCount	
		oClone:RecId		        := SELF:RecId		
		oClone:Rest			        := SELF:Rest			 
		oClone:WhileBlock		    := SELF:WhileBlock		
		oClone:WhileExpression      := SELF:WhileExpression  
		RETURN oClone
		
END CLASS

STRUCTURE DbSeekInfo
	PUBLIC Last 	AS LOGIC
	PUBLIC SoftSeek AS LOGIC
	PUBLIC Value	AS OBJECT
END STRUCTURE

CLASS DbSortInfo 
	PUBLIC TransInfo AS DbTransInfo
	PUBLIC Items	 AS DbSortItem[]
END CLASS

STRUCTURE DbSortItem           
	PUBLIC FieldNo 	AS LONG
	PUBLIC OffSet	AS LONG
	PUBLIC Length	AS LONG
	PUBLIC Flags	AS LONG  
	PUBLIC CONST SF_Default := 0 AS LONG
	PUBLIC CONST SF_Case	:= 1 AS LONG
	PUBLIC CONST SF_Numeric := 2 AS LONG
	PUBLIC CONST SF_Ascii	 := 4 AS LONG       
	PUBLIC CONST SF_Long	   := 0x80 AS LONG
	PUBLIC CONST SF_Descending := 0x100 AS LONG
END STRUCTURE

CLASS DbTransInfo  
	PUBLIC Scope		AS DbScopeInfo
	PUBLIC Source		AS IRDD
	PUBLIC Destination 	AS IRDD
	PUBLIC Items		AS DbTransItem[]
	PUBLIC Flags		AS LONG
	PUBLIC CONST Match	:= 1 AS LONG
	PUBLIC CONST PutRec	:= 2 AS LONG
END CLASS

STRUCTURE DbTransItem
	PUBLIC Source 		AS LONG
	PUBLIC Destination 	AS LONG	
END STRUCTURE

STRUCTURE RddFieldInfo
	PUBLIC Name 		AS STRING
	PUBLIC FieldType 	AS DBFieldType
	PUBLIC Length 		AS LONG
	PUBLIC Decimals 	AS LONG
	PUBLIC Alias 		AS STRING
	CONSTRUCTOR(sName AS STRING, sType AS STRING, nLength AS LONG, nDecimals AS LONG)
		Name 		:= sName
		Length 		:= nLength
		Decimals 	:= nDecimals                            
		IF !String.IsNullOrEmpty(sType)
			FieldType := (DbFieldType) Char.ToUpper(sType[0])
		ELSE
			FieldType := DBFieldType.Unknown
		ENDIF  
		Alias       := sName
		RETURN
	CONSTRUCTOR(sName AS STRING, nType AS DbFieldType, nLength AS LONG, nDecimals AS LONG)
		Name 		:= sName                                
		FieldType 	:= nType
		Length 		:= nLength
		Decimals 	:= nDecimals
		Alias       := sName
		RETURN
	
END STRUCTURE

CLASS DbJoinList
END CLASS

CLASS OrderStatus
END CLASS

CLASS RddList
END CLASS


CLASS DbFieldNames
END CLASS


CLASS DbWORKAREASTATUS
END CLASS




END NAMESPACE




