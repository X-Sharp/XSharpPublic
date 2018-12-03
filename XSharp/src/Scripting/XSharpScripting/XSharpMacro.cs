using System;
using System.Text;
using System.Threading;
using Microsoft.CodeAnalysis.Scripting;
using Microsoft.CodeAnalysis.Scripting.Hosting;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.Scripting
{
    public static class XSharpMacro 
    {
        /// <summary>
        /// Create a new C# script.
        /// </summary>
        /// <param name="code">The source code of the script.</param>
        /// <param name="options">The script options.</param>
        /// <param name="allowSingleQuotes">Are single quoted literal strings in Macros allowed (true = VO compatible).</param>
        /// <param name="assemblyLoader">Custom  assembly loader.</param>
        /// <typeparam name="T">The return type of the script</typeparam>
        public static T Compile<T>(string code, ScriptOptions options = null, bool allowSingleQuotes = true, InteractiveAssemblyLoader assemblyLoader = null)
        {
            Type globalsType = null;
            CancellationToken cancellationToken = default(CancellationToken);
            return Script.CreateInitialScript<T>(XSharpMacroCompiler.GetInstance(allowSingleQuotes), SourceText.From(code, options?.FileEncoding), options, globalsType, assemblyLoader).RunAsync(null, cancellationToken).GetEvaluationResultAsync().Result;
        }
    }
}
