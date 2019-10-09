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

    [DebuggerDisplay("{Class}")];
	CLASS Custom INHERIT Abstract
        PROTECTED _oControls as Vfp.Collection

    VFPPROP Top    LONG
    VFPPROP Left   LONG
    VFPPROP Height LONG
    VFPPROP Width  LONG
 
    CONSTRUCTOR() CLIPPER
        SUPER()
        SELF:Top := SELF:Left := SELF:Height := SELF:Width := 0
        _oControls    := Vfp.Collection{}
        SELF:Init(_Args())
        RETURN

    // Events defined in FoxPro
    VIRTUAL METHOD Init() AS USUAL CLIPPER
       RETURN TRUE
        
    VIRTUAL METHOD Destroy() AS USUAL
       RETURN TRUE
        
    VIRTUAL METHOD Error(nErrpr, cMethod, nLine) AS USUAL
       RETURN TRUE

    #region Nested Items

    METHOD AddObject(cName, cClass, cOLEClass, aParams) AS OBJECT
        RETURN NULL_OBJECT


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
