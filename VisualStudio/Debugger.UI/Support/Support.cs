using Microsoft.VisualStudio.Debugger.CallStack;
using Microsoft.VisualStudio.Debugger.Evaluation;
using Microsoft.VisualStudio.Debugger;
using Microsoft.VisualStudio.Shell;
using System;
using System.Linq;
using System.Threading.Tasks;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Debugger.Clr;
using Microsoft.VisualStudio.Shell.Interop;
using System.Windows;
using XSharpModel;
using XSharp.Debugger.Support;

namespace XSharp.Debugger.UI
{
    internal class Support
    {
        internal static DkmLanguage language;
        private const DkmEvaluationFlags inspectionFlags =
            DkmEvaluationFlags.TreatAsExpression |
            DkmEvaluationFlags.ForceRealFuncEval;

        internal static EnvDTE.Debugger debugger;
        internal static MemvarsWindow memvarsWindow = null;
        internal static SettingsWindow settingsWindow = null;
        internal static GlobalsWindow globalsWindow = null;
        internal static WorkareasWindow workareasWindow = null;
        internal static DkmProcess currentProcess = null;


        static Support()
        {
            var lang_name = Constants.DbgXSharpLanguageName;
            var vend_id = Guid.Parse(Constants.XSharpVendorString);
            var lang_id = Guid.Parse(Constants.XSharpLanguageString);
            language = DkmLanguage.Create(lang_name, new DkmCompilerId(vend_id, lang_id));
            debugger = XSharpDebuggerUIPackage.XInstance.Dte.Debugger;
        }
        internal static string StripResult(string str)
        {
            if (str != null )
            {
                if (str.IndexOf(RtLink.ErrorPrefix) == -1)
                {
                    if (str.StartsWith("\"") && str.EndsWith("\""))
                    {
                        str = str.Substring(1, str.Length - 2);
                    }
                }
                else
                {
                    str = "";
                }
            }
            return str;
        }
        internal static async Task<string> GetMemVarsAsync()
        {
            var result = await Support.ExecExpressionAsync("XSharp.Debugger.Support.RtLink.GetMemVars()");
            return StripResult(result);
        }
        internal static async Task<string> GetGlobalsAsync()
        {
            var result = await Support.ExecExpressionAsync("XSharp.Debugger.Support.RtLink.GetGlobals()");
            return StripResult(result);
        }
        internal static async Task<string> GetSettingsAsync()
        {
            var result = await Support.ExecExpressionAsync("XSharp.Debugger.Support.RtLink.GetSettings()");
            return StripResult(result);
        }
        internal static async Task<string> GetWorkareasAsync()
        {
            var result = await Support.ExecExpressionAsync("XSharp.Debugger.Support.RtLink.GetWorkareas()");
            return StripResult(result);
        }

        internal static async Task<string> LoadAssemblyAsync( string fileName)
        {
            var loaded = await ExecExpressionAsync("System.Reflection.Assembly.LoadFile(\"" + fileName + "\")");
            return loaded;
        }
        static async Task<DkmProcess> LoadSupportDLLAsync()
        {
            var proc = DkmProcess.GetProcesses().FirstOrDefault();
            if (proc != currentProcess)
            {
                currentProcess = proc;
                var path = System.IO.Path.GetDirectoryName(typeof(Support).Assembly.Location);
                var support = System.IO.Path.Combine(path, "XSharp.Debugger.Support.dll");
                var loaded = await LoadAssemblyAsync(support);
            }
            return proc;
        }

        internal static async Task<string> ExecExpressionAsync(string source)
        {
            if (XDebuggerSettings.DebuggerMode != DebuggerMode.Break)
            {
                return "";
            }
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            var sf = debugger.CurrentStackFrame;
            if (sf == null)
            {
                return "";
            }
            var dkmsf = DkmStackFrame.ExtractFromDTEObject(sf);
            var thr = dkmsf.Thread;
            var proc = await LoadSupportDLLAsync();
            var expr = DkmLanguageExpression.Create(language, DkmEvaluationFlags.None, source, null);
            var wl = DkmWorkList.Create(null);
            var insp = DkmInspectionSession.Create(proc, null);
            var inst = dkmsf.RuntimeInstance;
            var ictx = DkmInspectionContext.Create(insp, inst, thr, 0,
                                                   inspectionFlags, DkmFuncEvalFlags.None, 10,
                                                   language, null);

            DkmEvaluationResult res = null;
            ictx.EvaluateExpression(wl, expr, dkmsf, (result) => { res = result.ResultObject; });
            wl.Execute();
            string value = "";
            if (res is DkmSuccessEvaluationResult sucres)
            {
                value = sucres.Value;
            }
            return value;
        }

        internal static void RegisterWindow(object oWindow)
        {
            switch (oWindow)
            {
                case MemvarsWindow mw:
                    memvarsWindow = mw;
                    break;
                case SettingsWindow sw:
                    settingsWindow = sw;
                    break;
                case GlobalsWindow gw:
                    globalsWindow = gw;
                    break;
                case WorkareasWindow ww:
                    workareasWindow = ww;
                    break;

            }
        }
        internal static void RefreshWindows()
        {
            if (XDebuggerSettings.DebuggerMode == DebuggerMode.Break)
            {
                if (memvarsWindow != null && memvarsWindow.Control.IsVisible)
                {
                    memvarsWindow.Refresh();
                }
                if (settingsWindow != null && settingsWindow.Control.IsVisible)
                {
                    settingsWindow.Refresh();
                }
                if (globalsWindow != null && globalsWindow.Control.IsVisible)
                {
                    globalsWindow.Refresh();
                }
                if (workareasWindow != null && workareasWindow.Control.IsVisible)
                {
                    workareasWindow.Refresh();
                }
            }
        }
        internal static void ClearWindows()
        {
            if (memvarsWindow != null )
            {
                memvarsWindow.Clear();
            }
            if (settingsWindow != null)
            {
                settingsWindow.Clear();
            }
            if (globalsWindow != null)
            {
                globalsWindow.Clear();
            }
            if (workareasWindow != null )
            {
                workareasWindow.Clear();
            }
        }
        
    }
}
