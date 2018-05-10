//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <summary>Base class for many types used in the VO SDK Classes</summary>
CLASS XSharp.VObject
    EXPORT oCargo AS OBJECT
    
    CONSTRUCTOR()
        SUPER()
        RETURN
        
    DESTRUCTOR()
        Destroy()
        RETURN
        
    VIRTUAL METHOD Destroy() AS USUAL CLIPPER
        RETURN SELF
        
        
END CLASS