//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.Diagnostics
USING System
USING System.Linq
USING LanguageService.CodeAnalysis.XSharp
USING LanguageService.SyntaxTree
BEGIN NAMESPACE XSharpModel


/// <summary>An entity in a source file. </summary>
[DebuggerDisplay("{Kind}, {Name,nq}")];
CLASS XSourceEntity INHERIT XSourceSymbol IMPLEMENTS IXSymbol, IXSourceSymbol
#region Simple Properties

    PROPERTY FullName                AS STRING           GET SELF:Name
    PROPERTY Namespace               AS STRING           AUTO

    PROPERTY ParentName              AS STRING           GET SELF:Parent?:FullName
    PROPERTY ComboPrototype          AS STRING           GET SELF:FullName
    PROPERTY Prototype               AS STRING           GET SELF:FullName
    PROPERTY Dialect                 AS XSharpDialect    AUTO
    PROPERTY CustomAttributes        AS STRING           AUTO
    PROPERTY SingleLine              AS LOGIC            AUTO
    PROPERTY Value                   AS STRING           AUTO
    PROPERTY XmlComments             AS STRING           AUTO
    PRIVATE _typeName                AS STRING
    PROPERTY IsGeneric               AS LOGIC            GET GenericArgs != NULL
    PROPERTY GenericArgs             AS STRING[]         AUTO GET PRIVATE SET
    PROPERTY StartOfXmlComments      AS LONG             AUTO
    PROPERTY BlockTokens             AS IList<IToken>     AUTO := List<IToken>{}
#endregion


    CONSTRUCTOR(Name AS STRING, kind AS Kind)
        SUPER(Name, kind, Modifiers.Public)


    CONSTRUCTOR(Name AS STRING, kind AS Kind, attributes AS Modifiers, range AS TextRange, interval AS TextInterval)
        SUPER(Name, kind, attributes, range, interval)
        SELF:Dialect := XSharpDialect.Core

    METHOD IncludesLine(nLine as LONG ) AS LOGIC
        RETURN SELF:Range:StartLine <= nLine .and. SELF:Range:EndLine >= nLine

    METHOD IncludesPosition(nPos as LONG ) AS LOGIC
        RETURN SELF:Interval:Start <= nPos .and. SELF:Interval:Stop >= nPos

    METHOD ForceComplete() AS VOID
        LOCAL ParentName AS STRING
        LOCAL thisName AS STRING
        LOCAL tmp AS XSourceTypeSymbol

        IF SELF:Parent == NULL .AND. ! String.IsNullOrEmpty(SELF:ParentName)

            ParentName := SELF:ParentName
            thisName := SELF:FullName
            IF ParentName:IndexOf(".") == -1 .AND. thisName:IndexOf(".") > 0

                ParentName := thisName:Substring(0, (thisName:LastIndexOf(".") + 1)) + ParentName
            ENDIF
            IF SELF:File != NULL .AND. ParentName != "System.Object"

                tmp := SELF:File:Project:Lookup(ParentName, SELF:File:Usings:ToArray())
                IF tmp != NULL

                    SELF:Parent := tmp
                    // Ensure whole tree is resolved.
                    SELF:Parent:ForceComplete()
                ENDIF
            ENDIF
        ENDIF


#region Complexer properties
        // Properties

    PROPERTY Description AS STRING
    GET
        RETURN SELF:ModVis + " "+KindKeyword +  " "+SELF:Prototype
    END GET
    END PROPERTY


    PROPERTY IsTyped                  AS LOGIC GET !String.IsNullOrEmpty(_typeName)
    PROPERTY TypeName AS STRING
    GET
        IF IsTyped
            RETURN SELF:_typeName
        ELSE
            RETURN XLiterals.UsualType
        ENDIF
    END GET
    SET
        SELF:_typeName := VALUE
        SELF:CheckForGenericTypeName()
    END SET
    END PROPERTY


    PRIVATE METHOD CheckForGenericTypeName() AS VOID
        VAR pos := SELF:TypeName:IndexOf("<")
        IF pos > 0
            VAR tmp := SELF:TypeName:Substring(pos)
            SELF:GenericArgs := tmp:Split(<CHAR>{'<',',','>'}, StringSplitOptions.RemoveEmptyEntries)
        ELSE
            SELF:GenericArgs := NULL
        ENDIF

    METHOD CopyValuesFrom(dbresult AS XDbResult) AS VOID
        SUPER:CopyValuesFrom(dbresult)
        IF String.IsNullOrEmpty(SELF:XmlComments)
            SELF:XmlComments := dbresult:XmlComments
        ENDIF


#endregion
END CLASS

END NAMESPACE


