//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp
Enum Gencode
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
    MEMBER @@SEQUENCE   := 10
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
    MEMBER @@LOCK       := 41
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

END Enum
END NAMESPACE
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