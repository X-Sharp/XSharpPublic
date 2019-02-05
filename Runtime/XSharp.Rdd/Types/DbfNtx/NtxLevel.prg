//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING System.Diagnostics

BEGIN NAMESPACE XSharp.RDD.NTX

    [DebuggerDisplay("Level: {PageOffset}")];
    INTERNAL CLASS NtxLevel INHERIT NtxPage
        // This special subclass of NtxPage is used during index creation
        // It has some extra properties to help keeping track of the number of keys on the page
        INTERNAL PROPERTY Exp       AS LONG AUTO
        INTERNAL PROPERTY BaseKeys  AS LONG AUTO
        INTERNAL PROPERTY Keys      AS LONG AUTO
        INTERNAL PROPERTY ExtraKeys AS LONG AUTO
        INTERNAL PROPERTY Parents   AS LONG AUTO
        
        INTERNAL CONSTRUCTOR(order AS NtxOrder )
            SUPER(order, 0)
            SELF:Exp        := 0
            SELF:BaseKeys   := 0
            SELF:Keys       := 0
            SELF:Parents    := 0
            SELF:ExtraKeys  := 0
            
            
        INTERNAL METHOD Write( offset AS LONG ) AS LOGIC
            LOCAL result AS LOGIC
            //
            SELF:PageOffset := offset
            result := SELF:Write()
            RETURN result
            
            
    END CLASS
    
    
END NAMESPACE
