//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING Mono.Cecil
using XSharp.Settings
BEGIN NAMESPACE XSharpModel

CLASS XPEMethodSymbol  INHERIT XPEMemberSymbol
    PRIVATE _methoddef       as MethodDefinition
    PRIVATE _ccAttrib        AS Mono.Cecil.CustomAttribute
    PROPERTY IsSpecialName   AS LOGIC AUTO GET PRIVATE SET
    STATIC METHOD ConvertAttributes (attributes AS MethodAttributes) as Modifiers
        var modifiers := Modifiers.None
        var visattributes := _AND(attributes, MethodAttributes.MemberAccessMask)
        SWITCH visattributes
        CASE MethodAttributes.Private    // 1
            modifiers :=  Modifiers.Private
        CASE MethodAttributes.Assembly  // 3
            modifiers :=  Modifiers.Internal
        CASE MethodAttributes.Family     // 4
            modifiers :=  Modifiers.Protected
        CASE MethodAttributes.FamORAssem // 5
        CASE MethodAttributes.FamANDAssem // 5
            modifiers :=  Modifiers.ProtectedInternal
        CASE MethodAttributes.Public     // 6
            modifiers :=  Modifiers.Public
        OTHERWISE
            modifiers := Modifiers.Public
        END SWITCH
        IF attributes:HasFlag(MethodAttributes.Abstract)
            modifiers |= Modifiers.Abstract
        ENDIF
        IF attributes:HasFlag(MethodAttributes.Final)
            modifiers |= Modifiers.Sealed
        ENDIF
        IF attributes:HasFlag(MethodAttributes.PInvokeImpl)
            modifiers |= Modifiers.External
        ENDIF
        IF attributes:HasFlag(MethodAttributes.UnmanagedExport)
            modifiers |= Modifiers.External
        ENDIF
        IF attributes:HasFlag(MethodAttributes.Virtual)
            IF attributes:HasFlag(MethodAttributes.NewSlot)
                modifiers |= Modifiers.Virtual
            ELSE
                modifiers |= Modifiers.Override
            ENDIF
        ELSEIF attributes:HasFlag(MethodAttributes.NewSlot)
            modifiers |= Modifiers.New
        ENDIF
        IF attributes:HasFlag(MethodAttributes.Static)
            modifiers |= Modifiers.Static
        ENDIF
        RETURN modifiers

    CONSTRUCTOR(def AS MethodDefinition, asm AS XAssembly)
        SUPER(def:Name, Kind.Method, ConvertAttributes(def:Attributes),  asm)
        SELF:DeclaringType   := def:DeclaringType:GetXSharpTypeName()
        IF DeclaringType:EndsWith("Functions")
            SELF:Attributes := _AND(SELF:Attributes, ~Modifiers.Static)
            SELF:Kind      := Kind.Function
        ENDIF
        SELF:OriginalTypeName   := RemoveGenericParameters(def:ReturnType:FullName)
        SELF:TypeName           := SELF:Signature:DataType := def:ReturnType:GetXSharpTypeName()
        SELF:_methoddef         := def
        IF def:HasCustomAttributes
            SUPER:_custatts       := def:CustomAttributes
            FOREACH VAR attr IN def:CustomAttributes
                SWITCH attr:AttributeType:FullName
                CASE KnownTypes.SystemExtension
                    SELF:Signature:IsExtension := TRUE
                    if asm:Types:ContainsKey(SELF:DeclaringType)
                        SELF:DeclaringTypeSym := asm:Types[SELF:DeclaringType]
                    endif
                CASE KnownTypes.XSharpClipper
                CASE KnownTypes.VulcanClipper
                    SELF:CallingConvention := CallingConvention.Clipper
                    _ccAttrib := attr
                END SWITCH
            NEXT
        ENDIF
        IF def:HasGenericParameters
            SELF:_generic := def:HasGenericParameters
            SELF:_signature:ReadGenericParameters(def:GenericParameters)
        ENDIF
        SELF:IsSpecialName := def:IsSpecialName
    PROTECTED INTERNAL OVERRIDE METHOD Resolve() AS VOID
        IF ! _resolved .and. _methoddef != NULL
            // Add Generic parameters first so have that info when processing the parameters
            IF _methoddef:HasGenericParameters
                SELF:AddTypeParameters(_methoddef:GenericParameters)
            ENDIF
            IF _methoddef:HasParameters
                IF SELF:CallingConvention = CallingConvention.Clipper
                    SELF:AddParameters(_ccAttrib)
                ELSE
                    SELF:AddParameters(_methoddef:Parameters)
                ENDIF
            ENDIF
            SUPER:Resolve()
            SELF:_methoddef := NULL
        ENDIF

        RETURN
    PROPERTY ClassGenText as STRING
        GET
            var result := SELF:VisibilityKeyword + " "
            result += SELF:ModifiersKeyword + " "
            result += SELF:KindKeyword + " "
            var temp := SELF:GetProtoType(TRUE)
            if (SELF:Kind == Kind.Constructor)
                temp := temp.Replace('}',')')
                var pos := temp.IndexOf('{')
                temp := "("+temp:Substring(pos+1)
                result += temp
            ELSE
                result += temp
            endif
            if self:CallingConvention == CallingConvention.Clipper
                result += " CLIPPER"
            endif
            RETURN result:Replace("  ", " ")
        END GET
    END PROPERTY

    METHOD Clone() AS IXMemberSymbol
        SELF:Resolve()
        var  clone := (XPEMethodSymbol) SUPER:Clone()
        clone:_methoddef := NULL
        clone:_resolved := TRUE
        RETURN clone


END CLASS
END NAMESPACE // XSharpModel.PE
