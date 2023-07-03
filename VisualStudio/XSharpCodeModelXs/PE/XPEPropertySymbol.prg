//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING Mono.Cecil

BEGIN NAMESPACE XSharpModel

CLASS XPEPropertySymbol INHERIT XPEMemberSymbol
    PROPERTY Accessors      AS AccessorKind AUTO
    PROPERTY IsSpecialName  AS LOGIC AUTO GET PRIVATE SET

    CONSTRUCTOR(def AS PropertyDefinition, asm AS XAssembly)
        SUPER(def:Name, Kind.Property, Modifiers.Public,  asm)
        SELF:Parent          := NULL
        SELF:DeclaringType   := def:DeclaringType:GetXSharpTypeName()
        SELF:OriginalTypeName   := RemoveGenericParameters(def:PropertyType:FullName)
        SELF:TypeName        := SELF:Signature:DataType := def:PropertyType:GetXSharpTypeName()
        if def:GetMethod != NULL
            VAR xMethod := XPEMethodSymbol{def:GetMethod, asm}
            SELF:Attributes := xMethod:Attributes
        ELSEIF def:SetMethod != NULL
            VAR xMethod := XPEMethodSymbol{def:SetMethod, asm}
            SELF:Attributes := xMethod:Attributes
        ENDIF
        IF def:HasCustomAttributes
            SUPER:_custatts       := def:CustomAttributes
        ENDIF
        IF def:GetMethod != NULL
            SELF:Accessors |= AccessorKind.Get
        ENDIF
        IF def:SetMethod != NULL
            SELF:Accessors |= AccessorKind.Set
        ENDIF
        IF def:HasParameters
            SELF:AddParameters(def:Parameters)
        ENDIF
        SELF:IsSpecialName := def:IsSpecialName
    PROPERTY ClassGenText as STRING
        GET
            var result := SELF:VisibilityKeyword + " "
            result += SELF:ModifiersKeyword + " "
            result += SELF:KindKeyword + " "
            result += SELF:Prototype
            result += IIF(Accessors.HasFlag(AccessorKind.Get), " GET","")
            result += IIF(Accessors.HasFlag(AccessorKind.Set), " SET","")
            RETURN result:Replace("  ", " ")
        END GET
    END PROPERTY

END CLASS
END NAMESPACE // XSharpModel.PE
