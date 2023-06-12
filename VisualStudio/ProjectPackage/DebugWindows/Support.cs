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

namespace XSharp.Project.DebugWindows
{
    internal class Support
    {
        internal static ShowMemvarsWindow globalsWindow = null;
        private static readonly DkmLanguage language = null;
        private static readonly EnvDTE.Debugger debugger;
        private const DkmEvaluationFlags inspectionFlags =
            DkmEvaluationFlags.TreatAsExpression |
            DkmEvaluationFlags.ForceRealFuncEval;

        static Support()
        {
            var lang_name = Constants.DbgXSharpLanguageName;
            var vend_id = Guid.Parse(Constants.XSharpVendorString);
            var lang_id = Guid.Parse(Constants.XSharpLanguageString);
            language = DkmLanguage.Create(lang_name, new DkmCompilerId(vend_id, lang_id));
            debugger = XSharpProjectPackage.XInstance.Dte.Debugger;
        }

        internal static async Task<string> ExecExpressionAsync(string source)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            var sf = debugger.CurrentStackFrame;
            var dkmsf = DkmStackFrame.ExtractFromDTEObject(sf);
            var thr = dkmsf.Thread;
            var proc = DkmProcess.GetProcesses().FirstOrDefault();

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
            string value = (res as DkmSuccessEvaluationResult)?.Value;
            return value;
        }

        internal static void RegisterWindow(object oWindow)
        {
            if (oWindow is ShowMemvarsWindow glob)
                globalsWindow = glob;
        }
        internal static void RefreshWindows()
        {
            if (globalsWindow != null  && globalsWindow.Control.IsVisible)
            {
                globalsWindow.Refresh();
            }
        }
    }
}
