//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

begin namespace XSharp
/// <Summary>Error code Enum that matches the Visual Objecs Generic Error Code</Summary>
enum Gencode
	member WAIT         := 0
	member ARG          := 1
	member BOUND        := 2
	member STROVERFLOW  := 3
	member NUMOVERFLOW  := 4
	member ZERODIV      := 5
	member NUMERR       := 6
	member SYNTAX       := 7
	member COMPLEXITY   := 8
	member MEMOVERFLOW  := 9
	member @@SEQUENCE	:= 10
	member MEM          := 11
	member NOFUNC       := 12
	member NOMETHOD     := 13
	member NOVAR        := 14
	member NOALIAS      := 15
	member NOVARMETHOD  := 16
	member BADALIAS     := 17
	member DUPALIAS     := 18
	member NULLVAR      := 19
	member CREATE       := 20
	member OPEN         := 21
	member CLOSE        := 22
	member READ         := 23
	member WRITE        := 24
	member PRINT        := 25
	member NOATOM       := 26
	member NOCLASS      := 27
	member WRONGCLASS   := 28
	member REFERENCE    := 29
	member UNSUPPORTED  := 30
	member LIMIT        := 31
	member CORRUPTION   := 32
	member DATATYPE     := 33
	member DATAWIDTH    := 34
	member NOTABLE      := 35
	member NOORDER      := 36
	member SHARED       := 37
	member UNLOCKED     := 38
	member READONLY     := 39
	member APPENDLOCK   := 40
	member LOCK			:= 41
	// missing 42 .. 44                        
	member LOCK_ERROR   := 45
	member LOCK_TIMEOUT := 46
	member STACK        := 47
	member EVALSTACK    := 48
	member ERRORBLOCK   := 49
	member PROTECTION   := 50

	member BADPTR       := 51
	member BADPAGEFAULT := 52
	member ERRORBUILD   := 53
	member DYNPTR       := 54
	// Last Member                        
	member MAX          := 54

end enum
end namespace
#region Gencode Defines

define EG_WAIT           := GenCode.WAIT         
define EG_ARG            := GenCode.ARG          
define EG_BOUND          := GenCode.BOUND        
define EG_STROVERFLOW    := GenCode.STROVERFLOW  
define EG_NUMOVERFLOW    := GenCode.NUMOVERFLOW  
define EG_ZERODIV        := GenCode.ZERODIV      
define EG_NUMERR         := GenCode.NUMERR       
define EG_SYNTAX         := GenCode.SYNTAX       
define EG_COMPLEXITY     := GenCode.COMPLEXITY   
define EG_MEMOVERFLOW    := GenCode.MEMOVERFLOW  
define EG_SEQUENCE       := GenCode.SEQUENCE     
define EG_MEM            := GenCode.MEM          
define EG_NOFUNC         := GenCode.NOFUNC       
define EG_NOMETHOD       := GenCode.NOMETHOD     
define EG_NOVAR          := GenCode.NOVAR        
define EG_NOALIAS        := GenCode.NOALIAS      
define EG_NOVARMETHOD    := GenCode.NOVARMETHOD  
define EG_BADALIAS       := GenCode.BADALIAS     
define EG_DUPALIAS       := GenCode.DUPALIAS     
define EG_NULLVAR        := GenCode.NULLVAR      
define EG_CREATE         := GenCode.CREATE       
define EG_OPEN           := GenCode.OPEN         
define EG_CLOSE          := GenCode.CLOSE        
define EG_READ           := GenCode.READ         
define EG_WRITE          := GenCode.WRITE        
define EG_PRINT          := GenCode.PRINT        
define EG_NOATOM         := GenCode.NOATOM       
define EG_NOCLASS        := GenCode.NOCLASS      
define EG_WRONGCLASS     := GenCode.WRONGCLASS   
define EG_REFERENCE      := GenCode.REFERENCE    
define EG_UNSUPPORTED    := GenCode.UNSUPPORTED  
define EG_LIMIT          := GenCode.LIMIT        
define EG_CORRUPTION     := GenCode.CORRUPTION   
define EG_DATATYPE       := GenCode.DATATYPE     
define EG_DATAWIDTH      := GenCode.DATAWIDTH    
define EG_NOTABLE        := GenCode.NOTABLE      
define EG_NOORDER        := GenCode.NOORDER      
define EG_SHARED         := GenCode.SHARED       
define EG_UNLOCKED       := GenCode.UNLOCKED     
define EG_READONLY       := GenCode.READONLY     
define EG_APPENDLOCK     := GenCode.APPENDLOCK   
define EG_LOCK           := GenCode.LOCK         
define EG_LOCK_ERROR     := GenCode.LOCK_ERROR   
define EG_LOCK_TIMEOUT   := GenCode.LOCK_TIMEOUT 
define EG_STACK          := GenCode.STACK        
define EG_EVALSTACK      := GenCode.EVALSTACK    
define EG_ERRORBLOCK     := GenCode.ERRORBLOCK   
define EG_PROTECTION     := GenCode.PROTECTION   
define EG_BADPTR         := GenCode.BADPTR       
define EG_BADPAGEFAULT   := GenCode.BADPAGEFAULT 
define EG_ERRORBUILD     := GenCode.ERRORBUILD   
define EG_DYNPTR         := GenCode.DYNPTR       
define EG_MAX            := GenCode.MAX          
#endregion
