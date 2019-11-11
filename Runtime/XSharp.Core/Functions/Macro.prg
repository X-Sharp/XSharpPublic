//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp
USING System.Collections.Generic
USING System.Text
/// <summary>
/// Get the type of the class that is used to compile macros
/// </summary>
/// <returns>The type of the currently defined MacroCompiler. This may be NULL if no type has been set yet and no macros have been compiled.</returns>
/// <seealso cref="T:XSharp.IMacroCompiler"/>
FUNCTION GetMacroCompiler () AS System.Type
	RETURN XSharp.RuntimeState._macroCompilerType
	
/// <summary>
/// Set the type of the class that must be used to compile macros
/// </summary>
/// <param name="oCompiler">The type of the class that implements the macro compiler. This type MUST implement IMacroCompiler.</param>
/// <returns>The type of the previously defined MacroCompiler. This may be NULL if no type has been set yet and no macros have been compiled.</returns>
/// <seealso cref="T:XSharp.IMacroCompiler"/>
FUNCTION SetMacroCompiler (oCompiler AS System.Type) AS System.Type
VAR old := XSharp.RuntimeState._macroCompilerType
XSharp.RuntimeState._macroCompilerType := oCompiler
XSharp.RuntimeState._macroCompiler := NULL
RETURN old



/// <summary>
/// Set the class that must be used to compile macros
/// </summary>
/// <param name="oCompiler">The object that implements the macro compiler.</param>
/// <returns>The previously defined MacroCompiler. This may be NULL if no compiler has been set yet and no macros have been compiled.</returns>
/// <seealso cref="T:XSharp.IMacroCompiler"/>
FUNCTION SetMacroCompiler (oCompiler AS IMacroCompiler) AS IMacroCompiler
VAR old := XSharp.RuntimeState._macroCompiler
XSharp.RuntimeState._macroCompiler := oCompiler
XSharp.RuntimeState._macroCompilerType := oCompiler:GetType()
RETURN old


/// <summary>
/// Set the delegate that may be used to decide which symbol to call for ambiguous symbols
/// </summary>
/// <param name="resolver">The delegate to call.</param>
/// <returns>The previously delegate.</returns>
/// <seealso cref="T:XSharp.IMacroCompiler"/>
/// <seealso cref="T:XSharp.IMacroCompiler2"/>
/// <seealso cref="T:XSharp.MacroCompilerResolveAmbiguousMatch"/>

FUNCTION SetMacroDuplicatesResolver(resolver as MacroCompilerResolveAmbiguousMatch) AS MacroCompilerResolveAmbiguousMatch
    VAR old := XSharp.RuntimeState._macroResolver
    XSharp.RuntimeState.MacroResolver := resolver
    RETURN old
    
