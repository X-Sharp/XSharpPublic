//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharpModel
USING System.Diagnostics
USING System.Collections.Generic
USING LanguageService.SyntaxTree

BEGIN NAMESPACE XSharpModel
     // an entity in the source code
   [DebuggerDisplay("{ToString(),nq}")];
   CLASS XSourceNamespaceSymbol INHERIT XSourceTypeSymbol

    CONSTRUCTOR(name AS STRING, span AS TextRange, position AS TextInterval, oFile AS XFile)
        SUPER(name, Kind.Namespace, Modifiers.Public, span, position, oFile)
        RETURN
	END CLASS
END NAMESPACE // XSharpModel.Model
