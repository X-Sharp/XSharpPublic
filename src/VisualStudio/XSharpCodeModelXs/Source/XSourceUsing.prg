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
NAMESPACE XSharpModel

CLASS XSourceUsing INHERIT XSourceMemberSymbol
    PROPERTY IsGlobal as LOGIC GET SELF:Modifiers:HasFlag(Modifiers.Global)
    PROPERTY Alias as STRING GET ReturnType SET ReturnType := VALUE
    PROPERTY Description AS STRING GET SELF:Prototype
    CONSTRUCTOR(Name AS STRING, attributes AS Modifiers)
        SUPER(Name, Kind.Using, attributes, TextRange.Empty, TextInterval.Empty, "", null)

    CONSTRUCTOR(Name AS STRING, attributes AS Modifiers, range AS TextRange, interval AS TextInterval, alias as string)
        SUPER(Name, Kind.Using, attributes, range, interval, alias, null)


END CLASS
