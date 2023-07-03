//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING Mono.Cecil

BEGIN NAMESPACE XSharpModel

CLASS XPEEventSymbol INHERIT XPEMemberSymbol
    PROPERTY Accessors      AS AccessorKind AUTO
    PROPERTY IsSpecialName  AS LOGIC AUTO GET PRIVATE SET
    CONSTRUCTOR(def AS EventDefinition, asm AS XAssembly)
        SUPER(def:Name, Kind.Event, Modifiers.Public,  asm)
        SELF:DeclaringType         := def:DeclaringType:GetXSharpTypeName()
        SELF:OriginalTypeName      := RemoveGenericParameters(def:EventType:FullName)
        SELF:TypeName              := SELF:Signature:DataType    := def:EventType:GetXSharpTypeName()
        if def:AddMethod != NULL
            VAR xMethod := XPEMethodSymbol{def:AddMethod, asm}
            SELF:Attributes := xMethod:Attributes
        elseif def:RemoveMethod != null
            VAR xMethod := XPEMethodSymbol{def:RemoveMethod, asm}
            SELF:Attributes := xMethod:Attributes
        endif
        IF def:HasCustomAttributes
            SUPER:_custatts       := def:CustomAttributes
        ENDIF
        IF def:AddMethod != NULL
            SELF:Accessors |= AccessorKind.Add
        ENDIF
        IF def:RemoveMethod != NULL
            SELF:Accessors |= AccessorKind.Remove
        ENDIF
        SELF:IsSpecialName := def:IsSpecialName

    PROPERTY ClassGenText as STRING
        GET
            var result := SELF:VisibilityKeyword + " "
            result += SELF:ModifiersKeyword + " "
            result += SELF:KindKeyword + " "
            result += SELF:Prototype
            result += IIF(Accessors.HasFlag(AccessorKind.Add), " ADD","")
            result += IIF(Accessors.HasFlag(AccessorKind.Remove), " REMOVE","")
            RETURN result:Replace("  ", " ")
        END GET
    END PROPERTY


END CLASS
END NAMESPACE // XSharpModel.PE
