//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text


#command VFPPROP <cName> <cType> => PROPERTY <cName> AS <cType> GET _GetProperty(<"cName">) SET _SetProperty(<"cName">, value)
#command VFPPROP <cName> <cType> <cLit> => PROPERTY <cName> AS <cType> GET _GetProperty(<(cLit)>) SET _SetProperty(<(cLit)>, value)

/// <summary>
/// The Abstract class.
/// </summary>
ABSTRACT CLASS XSharp.VFP.Abstract INHERIT XSharp.VFP.Empty
    VFPPROP Name STRING 
    VFPPROP BaseClass STRING 
    VFPPROP @@Class STRING "Class"
    VFPPROP ClassLibrary STRING 
    VFPPROP Comment STRING 
    VFPPROP Parent OBJECT
    VFPPROP ParentClass STRING 


    CONSTRUCTOR()
        SUPER()
        SELF:Name := ""
        SELF:Class := ClassName(SELF)
        SELF:ParentClass := SELF:GetType():BaseType:Name
        SELF:ClassLibrary := SELF:GetType():Assembly:ToString()
        SELF:Comment := ""
        SELF:BaseClass := ""
        RETURN
        
END CLASS

