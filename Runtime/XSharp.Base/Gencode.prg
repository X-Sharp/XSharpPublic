//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp
/// <Summary>Error code Enum that matches the Visual Objecs Generic Error Code</Summary>
ENUM Gencode
	MEMBER WAIT         := 0
	MEMBER ARG          := 1
	MEMBER BOUND        := 2
	MEMBER STROVERFLOW  := 3
	MEMBER NUMOVERFLOW  := 4
	MEMBER ZERODIV      := 5
	MEMBER NUMERR       := 6
	MEMBER SYNTAX       := 7
	MEMBER COMPLEXITY   := 8
	MEMBER MEMOVERFLOW  := 9
	MEMBER SEQUENCE		:= 10
	MEMBER MEM          := 11
	MEMBER NOFUNC       := 12
	MEMBER NOMETHOD     := 13
	MEMBER NOVAR        := 14
	MEMBER NOALIAS      := 15
	MEMBER NOVARMETHOD  := 16
	MEMBER BADALIAS     := 17
	MEMBER DUPALIAS     := 18
	MEMBER NULLVAR      := 19
	MEMBER CREATE       := 20
	MEMBER OPEN         := 21
	MEMBER CLOSE        := 22
	MEMBER READ         := 23
	MEMBER WRITE        := 24
	MEMBER PRINT        := 25
	MEMBER NOATOM       := 26
	MEMBER NOCLASS      := 27
	MEMBER WRONGCLASS   := 28
	MEMBER REFERENCE    := 29
	MEMBER UNSUPPORTED  := 30
	MEMBER LIMIT        := 31
	MEMBER CORRUPTION   := 32
	MEMBER DATATYPE     := 33
	MEMBER DATAWIDTH    := 34
	MEMBER NOTABLE      := 35
	MEMBER NOORDER      := 36
	MEMBER SHARED       := 37
	MEMBER UNLOCKED     := 38
	MEMBER READONLY     := 39
	MEMBER APPENDLOCK   := 40
	MEMBER LOCK			:= 41
	// missing 42 .. 44                        
	MEMBER LOCK_ERROR   := 45
	MEMBER LOCK_TIMEOUT := 46
	MEMBER STACK        := 47
	MEMBER EVALSTACK    := 48
	MEMBER ERRORBLOCK   := 49
	MEMBER PROTECTION   := 50
		
	MEMBER BADPTR       := 51
	MEMBER BADPAGEFAULT := 52
	MEMBER ERRORBUILD   := 53
	MEMBER DYNPTR       := 54
	// Last Member                        
	MEMBER MAX          := 54
		
END ENUM
END NAMESPACE
#region Gencode Defines
	
DEFINE EG_WAIT           := GenCode.WAIT         
DEFINE EG_ARG            := GenCode.ARG          
DEFINE EG_BOUND          := GenCode.BOUND        
DEFINE EG_STROVERFLOW    := GenCode.STROVERFLOW  
DEFINE EG_NUMOVERFLOW    := GenCode.NUMOVERFLOW  
DEFINE EG_ZERODIV        := GenCode.ZERODIV      
DEFINE EG_NUMERR         := GenCode.NUMERR       
DEFINE EG_SYNTAX         := GenCode.SYNTAX       
DEFINE EG_COMPLEXITY     := GenCode.COMPLEXITY   
DEFINE EG_MEMOVERFLOW    := GenCode.MEMOVERFLOW  
DEFINE EG_SEQUENCE       := GenCode.SEQUENCE     
DEFINE EG_MEM            := GenCode.MEM          
DEFINE EG_NOFUNC         := GenCode.NOFUNC       
DEFINE EG_NOMETHOD       := GenCode.NOMETHOD     
DEFINE EG_NOVAR          := GenCode.NOVAR        
DEFINE EG_NOALIAS        := GenCode.NOALIAS      
DEFINE EG_NOVARMETHOD    := GenCode.NOVARMETHOD  
DEFINE EG_BADALIAS       := GenCode.BADALIAS     
DEFINE EG_DUPALIAS       := GenCode.DUPALIAS     
DEFINE EG_NULLVAR        := GenCode.NULLVAR      
DEFINE EG_CREATE         := GenCode.CREATE       
DEFINE EG_OPEN           := GenCode.OPEN         
DEFINE EG_CLOSE          := GenCode.CLOSE        
DEFINE EG_READ           := GenCode.READ         
DEFINE EG_WRITE          := GenCode.WRITE        
DEFINE EG_PRINT          := GenCode.PRINT        
DEFINE EG_NOATOM         := GenCode.NOATOM       
DEFINE EG_NOCLASS        := GenCode.NOCLASS      
DEFINE EG_WRONGCLASS     := GenCode.WRONGCLASS   
DEFINE EG_REFERENCE      := GenCode.REFERENCE    
DEFINE EG_UNSUPPORTED    := GenCode.UNSUPPORTED  
DEFINE EG_LIMIT          := GenCode.LIMIT        
DEFINE EG_CORRUPTION     := GenCode.CORRUPTION   
DEFINE EG_DATATYPE       := GenCode.DATATYPE     
DEFINE EG_DATAWIDTH      := GenCode.DATAWIDTH    
DEFINE EG_NOTABLE        := GenCode.NOTABLE      
DEFINE EG_NOORDER        := GenCode.NOORDER      
DEFINE EG_SHARED         := GenCode.SHARED       
DEFINE EG_UNLOCKED       := GenCode.UNLOCKED     
DEFINE EG_READONLY       := GenCode.READONLY     
DEFINE EG_APPENDLOCK     := GenCode.APPENDLOCK   
DEFINE EG_LOCK           := GenCode.LOCK         
DEFINE EG_LOCK_ERROR     := GenCode.LOCK_ERROR   
DEFINE EG_LOCK_TIMEOUT   := GenCode.LOCK_TIMEOUT 
DEFINE EG_STACK          := GenCode.STACK        
DEFINE EG_EVALSTACK      := GenCode.EVALSTACK    
DEFINE EG_ERRORBLOCK     := GenCode.ERRORBLOCK   
DEFINE EG_PROTECTION     := GenCode.PROTECTION   
DEFINE EG_BADPTR         := GenCode.BADPTR       
DEFINE EG_BADPAGEFAULT   := GenCode.BADPAGEFAULT 
DEFINE EG_ERRORBUILD     := GenCode.ERRORBUILD   
DEFINE EG_DYNPTR         := GenCode.DYNPTR       
DEFINE EG_MAX            := GenCode.MAX          
#endregion