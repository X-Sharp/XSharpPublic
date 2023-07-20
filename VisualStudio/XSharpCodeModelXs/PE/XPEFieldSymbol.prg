//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING Mono.Cecil

BEGIN NAMESPACE XSharpModel

CLASS XPEFieldSymbol INHERIT XPEMemberSymbol
    PROPERTY IsSpecialName  AS LOGIC AUTO GET PRIVATE SET

    STATIC METHOD ConvertAttributes (attributes AS FieldAttributes) as Modifiers
        var modifiers := Modifiers.None
        var visattributes := _AND(attributes, FieldAttributes.FieldAccessMask)
        SWITCH visattributes
        CASE FieldAttributes.Private    // 1
            modifiers :=   Modifiers.Private
        CASE FieldAttributes.Assembly  // 3
            modifiers :=   Modifiers.Internal
        CASE FieldAttributes.Family     // 4
            modifiers :=   Modifiers.Protected
        CASE FieldAttributes.FamORAssem // 5
        CASE FieldAttributes.FamANDAssem // 5
            modifiers :=   Modifiers.ProtectedInternal
        CASE FieldAttributes.Public     // 6
            modifiers :=   Modifiers.Public
        END SWITCH
        IF attributes:HasFlag(FieldAttributes.InitOnly)
            modifiers |= Modifiers.InitOnly
        ENDIF
        IF attributes:HasFlag(FieldAttributes.Literal)
            modifiers |= Modifiers.Const
        ENDIF
        IF attributes:HasFlag(FieldAttributes.Static)
            modifiers |= Modifiers.Static
        ENDIF
        RETURN modifiers

    CONSTRUCTOR(def AS FieldDefinition, asm AS XAssembly)
        SUPER(def:Name, Kind.Field, ConvertAttributes(def:Attributes),  asm)
        SELF:DeclaringType   := def:DeclaringType:GetXSharpTypeName()
        SELF:OriginalTypeName := RemoveGenericParameters(def:FieldType:FullName)
        SELF:TypeName        := SELF:Signature:DataType      := def:FieldType:GetXSharpTypeName()
        IF def:IsLiteral .AND. def:Constant != NULL
            SELF:Value := def:Constant:ToString()
        endif
        IF def:HasCustomAttributes
            SUPER:_custatts       := def:CustomAttributes
        ENDIF
        IF DeclaringType == asm:GlobalClassName
            if self:Modifiers:HasFlag(Modifiers.Const) .or. self:Modifiers:HasFlag(Modifiers.InitOnly)
                self:Kind := Kind.VODefine
            else
                self:Kind := Kind.VOGlobal
            endif
        ENDIF
        SELF:IsSpecialName := def:IsSpecialName

     PROPERTY ClassGenText as STRING
        GET
            if SELF:Kind == Kind.VODefine
                var result := "DEFINE "+ SELF:Prototype
                RETURN result:Replace("  ", " ")
            elseif SELF:Kind == Kind.VOGlobal
                var result := "GLOBAL "+ SELF:Prototype
                RETURN result:Replace("  ", " ")
            else
                var result := SELF:VisibilityKeyword + " "
                result += SELF:ModifiersKeyword + " "
                IF SELF:Kind != Kind.Field
                    result += SELF:KindKeyword + " "
                ENDIF
                result += SELF:Prototype
                RETURN result:Replace("  ", " ")
            endif
        END GET
    END PROPERTY

END CLASS
END NAMESPACE // XSharpModel.PE
