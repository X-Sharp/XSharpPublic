//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING XSharpModel
USING LanguageService.CodeAnalysis.XSharp
USING LanguageService.SyntaxTree
USING LanguageService.CodeAnalysis.XSharp.SyntaxParser

BEGIN NAMESPACE XSharpModel

[DebuggerDisplay("{ToString(),nq}")];
CLASS XSourceMemberSymbol INHERIT XSourceEntity IMPLEMENTS IXMemberSymbol,IXSourceEntity
    // Fields
    PRIVATE _signature    AS XMemberSignature
    PROPERTY InitExit     AS STRING AUTO
    PROPERTY SubType      AS Kind AUTO
    PROPERTY DeclaringType  AS STRING AUTO
    PROPERTY ReturnType   AS STRING GET TypeName SET TypeName := value
    PROPERTY SourceCode   AS STRING AUTO
#region constructors


    CONSTRUCTOR(name AS STRING, kind AS Kind, attributes AS Modifiers, ;
            span AS TextRange, position AS TextInterval, returnType AS STRING, modifiers AS IList<IToken>, isStatic := FALSE AS LOGIC)
        SUPER(name, kind, attributes, span, position)
        SELF:Parent       := NULL
        SELF:ReturnType   := returnType
        SELF:IsStatic     := isStatic
        IF attributes:HasFlag(Modifiers.Static)
            SELF:IsStatic := TRUE
        ENDIF
        SELF:_signature   := XMemberSignature{}
        IF modifiers?:Count > 0
            SELF:BlockTokens:AddRange(modifiers)
        ENDIF

    CONSTRUCTOR(sig AS XMemberSignature, kind AS Kind, attributes AS Modifiers,  ;
            span AS TextRange, position AS TextInterval, modifiers AS IList<IToken>, isStatic := FALSE AS LOGIC)
        SUPER(sig:Id, kind, attributes, span, position)
        SELF:Parent       := NULL
        SELF:ReturnType   := sig:DataType
        SELF:IsStatic     := isStatic
        SELF:_signature   := sig
        IF attributes:HasFlag(Modifiers.Static)
            SELF:IsStatic := TRUE
        ENDIF
        FOREACH var par in sig:Parameters
            par:Parent := SELF
        NEXT
        IF modifiers?:Count > 0
            SELF:BlockTokens:AddRange(modifiers)
        ENDIF
    CONSTRUCTOR(dbresult AS XDbResult, file AS XFile)
        SELF(dbresult:MemberName, dbresult:Kind, dbresult:Attributes, dbresult:TextRange, dbresult:TextInterval, dbresult:ReturnType, null, dbresult:Modifiers:HasFlag(Modifiers.Static))
        SELF:File        := file
        SELF:CopyValuesFrom(dbresult)
        SELF:SourceCode  := dbresult:SourceCode
#endregion

    STATIC METHOD FromDbResult( dbresult AS XDbResult, file AS XFile) AS XSourceMemberSymbol
        RETURN XSourceMemberSymbol{dbresult, file}


    METHOD AddParameters( list AS IList<XSourceParameterSymbol>) AS VOID
        IF list != NULL
            FOREACH VAR par IN list
                SELF:AddParameter(par)
            NEXT
        ENDIF
        RETURN

    METHOD AddParameter(oVar AS XSourceParameterSymbol) AS VOID
        oVar:Parent := SELF
        oVar:File   := SELF:File
        _signature:Parameters:Add(oVar)
        oVar:Parent := SELF
        RETURN


#region Properties. Some are implemented as Extension methods, others forwarded to the signature

    PROPERTY Description AS STRING GET SELF:GetDescription()
    PROPERTY FullName AS STRING GET SELF:GetFullName()

    PROPERTY HasParameters     AS LOGIC GET _signature:HasParameters
    PROPERTY ParameterCount    AS INT   GET _signature:ParameterCount

    PROPERTY ParameterList      AS STRING GET _signature:ParameterList

    PROPERTY ComboParameterList AS STRING
    GET
        TRY
            VAR parameters := ""
            FOREACH variable AS IXParameterSymbol IN SELF:Parameters
                IF (parameters:Length > 0)
                    parameters := parameters + ", "
                ENDIF
                VAR cType := variable:ShortTypeName
                IF variable:IsTyped .AND. variable:ParamType != ParamType.As
                    parameters += variable:ParamTypeDesc:TrimStart() + cType
                ELSE
                    parameters += cType
                ENDIF
            NEXT
            RETURN parameters
        CATCH
            RETURN ""
        END TRY
    END GET
    END PROPERTY
    PROPERTY Parameters         AS IList<IXParameterSymbol> GET _signature:Parameters:ToArray()

    PROPERTY Signature         AS XMemberSignature  GET _signature SET _signature := @@value
    PROPERTY CallingConvention AS CallingConvention GET _signature:CallingConvention SET _signature:CallingConvention := @@value


    PROPERTY Prototype      AS STRING GET SELF:GetProtoType()

    PROPERTY ComboPrototype AS STRING
    GET
        TRY
            IF SELF:Kind == Kind.Undefine
                RETURN "("+SELF:Name+")"
            ENDIF
            VAR vars := ""
            VAR desc := ""
            IF SELF:Kind:HasParameters()
                IF ( SELF:Kind == Kind.@@Constructor )
                    vars := "{" + SELF:ComboParameterList + "}"
                ELSEIF SELF:Kind:IsProperty()
                    IF SELF:ParameterCount > 0
                        vars := "[" + SELF:ComboParameterList + "]"
                    ENDIF
                ELSE
                    vars := "(" + SELF:ComboParameterList + ")"
                ENDIF
            ENDIF
            IF ( SELF:Kind == Kind.@@Constructor )
                desc := XLiterals.ConstructorName + vars
            ELSE
                desc := SELF:Name + vars
            ENDIF
            IF SELF:Kind:HasReturnType()
                desc := desc +  XLiterals.AsKeyWord + SELF:TypeName:GetXSharpTypeName()
            ENDIF
            RETURN desc
        CATCH
            RETURN ""
        END TRY
    END GET
    END PROPERTY

    PROPERTY ParentType     AS IXTypeSymbol
    GET
        TRY
            IF SELF:Parent IS IXTypeSymbol
                RETURN (IXTypeSymbol) SELF:Parent
            ENDIF
            IF SELF:Parent IS IXMemberSymbol
                RETURN ((IXMemberSymbol) SELF:Parent):ParentType
            ENDIF
        CATCH
            NOP
        END TRY
        RETURN NULL
    END GET
    END PROPERTY
    PROPERTY IsExtension    AS LOGIC    GET _signature:IsExtension
    PROPERTY XMLSignature   AS STRING GET SELF:GetXmlSignature()
    PROPERTY OriginalTypeName  AS STRING               GET SELF:TypeName
    PROPERTY TypeParameters as IList<STRING>           GET SELF:_signature:TypeParameters:ToArray()
    PROPERTY TypeParameterList AS STRING               GET SELF:_signature:TypeParameterList
    PROPERTY TypeParameterConstraints as IList<STRING> GET SELF:_signature:TypeParameterContraints:ToArray()
    PROPERTY TypeParameterConstraintsList AS STRING    GET SELF:_signature:TypeParameterConstraintsList


    PROPERTY ModifiersKeyword as STRING
    GET
        IF SELF:Kind:IsLocal()
            RETURN ""
        ELSE
            RETURN SUPER:ModifiersKeyword
        ENDIF
    END GET
    END PROPERTY

    PROPERTY VisibilityKeyword as STRING
    GET
        IF SELF:Kind:IsLocal()
            RETURN "PRIVATE"
        ELSE
            RETURN SUPER:VisibilityKeyword
        ENDIF
    END GET
    END PROPERTY

    PROPERTY Glyph                   AS LONG
    GET
        VAR glyph := SUPER:Glyph
        IF SELF:Name:EndsWith(XLiterals.XppDeclaration)
            glyph := glyph - (glyph % 6) + ImageListOverlay.ImageListOverlayArrow
        ENDIF
        RETURN glyph
    END GET
    END PROPERTY

    METHOD Clone() AS IXMemberSymbol
        var clone := (XSourceMemberSymbol) SELF:MemberwiseClone()
        clone:_signature := SELF:_signature:Clone()
        RETURN (IXMemberSymbol) clone


    METHOD WithName(newName as STRING) AS IXMemberSymbol
        var clone := (XSourceMemberSymbol) SELF:MemberwiseClone()
        clone:Name := newName
        clone:_signature := SELF:_signature:Clone()
        RETURN (IXMemberSymbol) clone

    METHOD ToString() AS STRING
        VAR result := i"{Kind} {Name}"
        if SELF:_signature != NULL .and. SELF:_signature:TypeParameters:Count > 0
            result += self:_signature:ToString()
        ENDIF
        RETURN result

#endregion
END CLASS

END NAMESPACE

