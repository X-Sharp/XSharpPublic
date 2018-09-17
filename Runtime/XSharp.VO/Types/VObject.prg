//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <summary>Base class for many types used in the VO SDK Classes</summary>
CLASS XSharp.VObject
    /// <summary>Cargo slot.</summary>
    EXPORT oCargo AS OBJECT
    /// <summary></summary>
    CONSTRUCTOR()
        SUPER()
        RETURN
        
    DESTRUCTOR()
        Destroy()
        RETURN
    /// <summary>Free memory resources allocated for a VObject object and its derived objects.</summary> 
    VIRTUAL METHOD Destroy() AS USUAL CLIPPER
        RETURN SELF
        
        
END CLASS
