        //
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
using System.Text



/// <summary>
/// The Abstract class.
/// </summary>
abstract class XSharp.VFP.Abstract inherit XSharp.VFP.Empty
    property Name       as string auto
    property BaseClass  as string auto
    property @@Class    as string  auto := "Class"
    property ClassLibrary as string auto
    property Comment    as string auto
    property Parent     as object auto
    property ParentClass as string auto


    CONSTRUCTOR()
        SUPER()
        RETURN

    PROTECTED OVERRIDE METHOD _InitProperties AS VOID
        SELF:Name           := ""
        SELF:Class          := ClassName(SELF)
        SELF:ParentClass    := SELF:GetType():BaseType:Name
        SELF:ClassLibrary   := SELF:GetType():Assembly:ToString()
        SELF:Comment        := ""
        SELF:BaseClass      := ""
    RETURN

end class

