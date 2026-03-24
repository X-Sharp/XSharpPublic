//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp
USING System.Collections.Generic
USING System.Text
/// <include file="XSharp.CoreDocs.xml" path="doc/GetMacroCompiler/*" />
FUNCTION GetMacroCompiler () AS System.Type
	RETURN XSharp.RuntimeState._macrocompilerType
	
/// <include file="XSharp.CoreDocs.xml" path="doc/SetMacroCompiler/*" />
FUNCTION SetMacroCompiler (oCompiler AS System.Type) AS System.Type
VAR old := XSharp.RuntimeState._macrocompilerType
XSharp.RuntimeState._macrocompilerType := oCompiler
XSharp.RuntimeState._macrocompiler := NULL
RETURN old



/// <include file="XSharp.CoreDocs.xml" path="doc/SetMacroCompiler_2/*" />
FUNCTION SetMacroCompiler (oCompiler AS IMacroCompiler) AS IMacroCompiler
VAR old := XSharp.RuntimeState._macrocompiler
XSharp.RuntimeState._macrocompiler := oCompiler
XSharp.RuntimeState._macrocompilerType := oCompiler:GetType()
RETURN old


/// <summary>
/// Set the delegate that may be used to decide which symbol to call for ambiguous symbols
/// </summary>
/// <param name="resolver">The delegate to call.</param>
/// <returns>The previously delegate.</returns>
/// <seealso cref="IMacroCompiler"/>
/// <seealso cref="IMacroCompiler2"/>
/// <seealso cref="MacroCompilerResolveAmbiguousMatch"/>

FUNCTION SetMacroDuplicatesResolver(resolver as MacroCompilerResolveAmbiguousMatch) AS MacroCompilerResolveAmbiguousMatch
    VAR old := XSharp.RuntimeState._macroresolver
    XSharp.RuntimeState.MacroResolver := resolver
    RETURN old
    
