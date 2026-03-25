//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// Aliases to functions in XSharp.RT
USING System

/// <include file="XSharp.XPP.Docs.xml" path="doc/DbDescend/*" />
FUNCTION DbDescend() AS LOGIC
    RETURN OrdDescend()

/// <include file="XSharp.XPP.Docs.xml" path="doc/DbSetDescend/*" />
FUNCTION DbSetDescend(lNewDescend AS LOGIC) AS LOGIC
    LOCAL old := OrdDescend() AS LOGIC
    OrdDescend(NIL,NIL, lNewDescend)
    RETURN old



/// <include file="XSharp.XPP.Docs.xml" path="doc/DbCargo/*" />
FUNCTION DbCargo(xNewValue) AS USUAL CLIPPER
    LOCAL nArea AS DWORD
    LOCAL old   AS OBJECT
    var wa := XSharp.RuntimeState.Workareas 
    nArea   := wa:CurrentWorkareaNO
    old     := wa:GetCargo(nArea) 
    IF PCOUNT() != 0
        wa:SetCargo(nArea, xNewValue)
    ENDIF
    RETURN old
    
