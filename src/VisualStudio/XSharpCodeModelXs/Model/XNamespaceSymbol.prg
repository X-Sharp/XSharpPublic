//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharpModel
USING LanguageService.SyntaxTree

NAMESPACE XSharpModel
     // an entity in the source code
[DebuggerDisplay("{Kind}, {Name,nq}")];
CLASS XNamespaceSymbol INHERIT XSymbol

CONSTRUCTOR(name AS STRING)
    SUPER(name, Kind.Namespace, Modifiers.Public)
    RETURN
PROPERTY Prototype as STRING GET SELF:KindKeyword+" "+SELF:Name
PROPERTY Location AS STRING GET ""
END CLASS
