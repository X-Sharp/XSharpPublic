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
        PROTECTED _Controls as VFP.Collection
        
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
            _Controls    := VFP.Collection{}
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
        
        VIRTUAL METHOD Error(nErrpr, cMethod, nLine) AS USUAL CLIPPER
            RETURN TRUE
            
        #region Nested Items
        
        METHOD AddObject(cName as string , oObject as object) AS LOGIC
            SELF:_SetProperty(cName, oObject)
            SELF:_Controls:AddObject(oObject, cName)
            RETURN TRUE
        

        METHOD AddProperty(cPropertyName, uValue, nVisibility, cDescription) AS LOGIC CLIPPER
            RETURN SUPER:__AddProperty(cPropertyName, uValue, nVisibility, cDescription)

        METHOD RemoveProperty(cPropertyName) AS LOGIC CLIPPER
            RETURN SUPER:__RemoveProperty(cPropertyName)


        METHOD NewObject(cObjectName, cClassName, cModule, cInApplication, aParams) AS OBJECT CLIPPER
            RETURN NULL_OBJECT
            
        METHOD RemoveObject(cName) AS LOGIC CLIPPER
            RETURN FALSE
            #endregion
            
        
        
        #region Designer Related and Other
        
        [Obsolete("This method is not supported in X#")];
        METHOD ResetToDefault(cName) AS VOID CLIPPER
            RETURN
       [Obsolete("This method is not supported in X#")];
        VIRTUAL METHOD ReadExpression(cPropertyName) AS STRING CLIPPER
            RETURN ""
        
        [Obsolete("This method is not supported in X#")];
        VIRTUAL METHOD WriteExpression(cPropertyName, uValue ) AS LOGIC CLIPPER
            RETURN FALSE
        
        [Obsolete("This method is not supported in X#")];
        VIRTUAL METHOD ReadMethod() AS STRING STRICT
            RETURN ""
        
        [Obsolete("This method is not supported in X#")];
        VIRTUAL METHOD SaveAsClass(cClassLib, cClass, cDescription) AS LOGIC CLIPPER
            RETURN FALSE
        
        [Obsolete("This method is not supported in X#")];
        VIRTUAL METHOD WriteMethod(cMethodName, cMethodText, lCreateMethod, nVisibility, cDescription) AS STRING  CLIPPER
            RETURN ""        
        
        [Obsolete("This method is not supported in X#")];
        METHOD ShowWhatsThis() AS LOGIC
            RETURN FALSE
        #endregion
        
        
        END CLASS
END NAMESPACE 
