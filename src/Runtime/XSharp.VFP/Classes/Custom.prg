//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using System.Collections.Generic
using System.Text
using System.Diagnostics

begin namespace XSharp.VFP

    class Custom inherit XSharp.VFP.Abstract
        protected _Controls as VFP.Collection
        property Top        as long auto
        property Left       as long auto
        property Height     as long auto
        property Width      as long auto
        property Controls as VFP.Collection GET _Controls

        PROTECTED OVERRIDE METHOD _InitProperties AS VOID
            SELF:Top := 0
            SELF:Left := 0
            SELF:Height := 0
            SELF:Width := 0
             SELF:_Controls    := VFP.Collection{}

        RETURN
        constructor() clipper
             super()
           self:Init(_Args())
            return

        destructor()
            self:Destroy()
            return

        // Events defined in FoxPro
        virtual method Init() as usual clipper
        return true

        virtual method Destroy() as usual
        return true

        virtual method Error(nErrpr, cMethod, nLine) as usual clipper
            return true

        #region Nested Items

        method AddObject(cName as string , oObject as object) as logic
            self:NoIvarPut(cName, oObject)
            self:_Controls:AddObject(oObject, cName)
            return true


        method AddProperty(cPropertyName, uValue, nVisibility, cDescription) as logic clipper
            return super:_AddProperty(cPropertyName, uValue, nVisibility, cDescription)

        method RemoveProperty(cPropertyName as string) as logic
            return super:_RemoveProperty(cPropertyName)


        method NewObject(cObjectName, cClassName, cModule, cInApplication, aParams) as object clipper
            return null_object

        method RemoveObject(cName) as logic clipper
            return false
            #endregion



        #region Designer Related and Other

        [Obsolete("This method is not supported in X#")];
        METHOD ResetToDefault(cName) AS VOID CLIPPER
            RETURN
       [Obsolete("This method is not supported in X#")];
        virtual method ReadExpression(cPropertyName) as string clipper
            RETURN ""

        [Obsolete("This method is not supported in X#")];
        virtual method WriteExpression(cPropertyName, uValue ) as logic clipper
            RETURN FALSE

        [Obsolete("This method is not supported in X#")];
        virtual method ReadMethod() as string strict
            RETURN ""

        [Obsolete("This method is not supported in X#")];
        virtual method SaveAsClass(cClassLib, cClass, cDescription) as logic clipper
            RETURN FALSE

        [Obsolete("This method is not supported in X#")];
        virtual method WriteMethod(cMethodName, cMethodText, lCreateMethod, nVisibility, cDescription) as string  clipper
            return ""

        [Obsolete("This method is not supported in X#")];
        METHOD ShowWhatsThis() AS LOGIC
            return false
        #endregion


        END CLASS
end namespace
