//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


[assembly: System.Runtime.CompilerServices.InternalsVisibleTo("xsi,PublicKey="+XSharp.Constants.PublicKeyLong)]

// THIS IS A HACK to prevent errors from showing up in the IDE
#if !XSHARP_RUNTIME
namespace Microsoft.CodeAnalysis.Scripting
{
    internal class ScriptingResources : LanguageService.CodeAnalysis.Scripting.ScriptingResources { }
}

namespace Microsoft.CodeAnalysis.CSharp.Scripting
{
    internal class CSharpScriptingResources : LanguageService.CodeAnalysis.Scripting.XSharpScriptingResources { }
}
#endif
// HACK ENDS HERE
