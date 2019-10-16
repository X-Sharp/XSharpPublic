//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING System.Diagnostics
#command VFPPROP <cName> <cType> => PROPERTY <cName> AS <cType> GET _GetProperty(<"cName">) SET _SetProperty(<"cName">, value)

BEGIN NAMESPACE XSharp.VFP

	CLASS Custom INHERIT Abstract
        PROTECTED _Controls as Vfp.Collection

    VFPPROP Top    LONG
    VFPPROP Left   LONG
    VFPPROP Height LONG
    VFPPROP Width  LONG

    PROTECTED VIRTUAL METHOD _InitProperties AS VOID
        SELF:Top := 0
        SELF:Left := 0
        SELF:Height := 0
        SELF:Width := 0
        RETURN
        
    CONSTRUCTOR() CLIPPER
        SUPER()
        _Controls    := Vfp.Collection{}
        SELF:_InitProperties()
        SELF:Init(_Args())
        RETURN

    DESTRUCTOR()
        SELF:Destroy()
        RETURN
        
    // Events defined in FoxPro
    VIRTUAL METHOD Init() AS USUAL CLIPPER
       RETURN TRUE
        
    VIRTUAL METHOD Destroy() AS USUAL
       RETURN TRUE
        
    VIRTUAL METHOD Error(nErrpr, cMethod, nLine) AS USUAL
       RETURN TRUE

    #region Nested Items

    METHOD AddObject(cName as string , oObject as object) AS LOGIC
        SELF:_SetProperty(cName, oObject)
        SELF:_Controls:AddObject(oObject, cName)
        RETURN TRUE
        


    METHOD NewObject(cObjectName, cClassName, cModule, cInApplication, aParams) AS OBJECT
        RETURN NULL_OBJECT

    METHOD RemoveObject(cName) AS LOGIC
        RETURN FALSE
    #endregion

  
        
    #region Other
    [Obsolete("This method is not supported in X#")];
    METHOD ShowWhatsThis() AS LOGIC
        RETURN FALSE
    #endregion

        

	END CLASS
END NAMESPACE 
