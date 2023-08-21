using System;
using System.Text;
using System.Threading;
using Microsoft.CodeAnalysis.Scripting;
using Microsoft.CodeAnalysis.Scripting.Hosting;
using Microsoft.CodeAnalysis.Text;
#nullable disable
namespace Microsoft.CodeAnalysis.CSharp.Scripting
{
    public static class XSharpMacro 
    {
        public static XSharpSpecificCompilationOptions GetOptions(int numdialect)
        {
            var dialect = (XSharpDialect)numdialect;
            var notcore = dialect != XSharpDialect.Core;
            var result = new XSharpSpecificCompilationOptions { Dialect = dialect };
            result.ImplicitNameSpace = true;
            result.NoStdDef = false;
            result.MemVars = dialect.SupportsMemvars();
            result.UndeclaredMemVars = dialect == XSharpDialect.FoxPro;
            result.InitLocals = notcore;
            result.LateBinding = notcore;
            result.Vo1 = false; // Init/Axit
            result.Vo2 = true;  // Initialize strings
            result.Vo3 = true;  // virtual methods
            result.Vo4 = notcore;  // implicit Signed/Unsigned
            result.Vo5 = notcore;  // implicit CLIPPER calling convention
            result.Vo6 = dialect.AllowPointerMagic();  // implicit pointer conversions
            result.Vo7 = notcore;  // implicit casts
            result.Vo8 = false; // compatible preprocessor
            result.Vo9 = dialect != XSharpDialect.Core;  // allow missing returns
            result.Vo10 = notcore; // Compatible IIF
            result.Vo11 = notcore; // Compatible numeric conversions
            result.Vo12 = notcore; // Compatible Integer divisions
            result.Vo13 = notcore; // Compatible string Comparisons
            result.Vo14 = notcore; // Float Literals
            result.Vo15 = notcore; // Missing types as USUAL
            result.Vo16 = notcore; // Generate Clipper constructors
            result.Fox1 = dialect == XSharpDialect.FoxPro; // Inherit from abstract class
            result.AllowDotForInstanceMembers = dialect.AllowDotForInstanceMembers();
            //result.Fox2 = dialect == XSharpDialect.FoxPro; // Expose Locals
            result.Xpp1 = dialect == XSharpDialect.XPP; // Inherit from Custom class
            result.RuntimeAssemblies = RuntimeAssemblies.XSharpRT | RuntimeAssemblies.XSharpCore;
            switch (dialect)
            {
                case XSharpDialect.VO:
                case XSharpDialect.Vulcan:
                    result.RuntimeAssemblies |= RuntimeAssemblies.XSharpVO;
                    break;
                case XSharpDialect.XPP:
                    result.RuntimeAssemblies |= RuntimeAssemblies.XSharpXPP;
                    break;
                case XSharpDialect.FoxPro:
                    result.RuntimeAssemblies |= RuntimeAssemblies.XSharpVFP;
                    break;
                case XSharpDialect.Harbour:
                    result.RuntimeAssemblies |= RuntimeAssemblies.XSharpHarbour;
                    break;
                default:
                    break;
            }
            return result;
        }

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
            var cancellationToken = default(CancellationToken);
            return Script.CreateInitialScript<T>(XSharpMacroCompiler.GetInstance(allowSingleQuotes), SourceText.From(code, options?.FileEncoding), options, globalsType, assemblyLoader).RunAsync(null, cancellationToken).GetEvaluationResultAsync().Result;
        }
    }
}
