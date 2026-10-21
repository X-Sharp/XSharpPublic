//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Debugger;
using Microsoft.VisualStudio.Debugger.Clr;
using Microsoft.VisualStudio.Debugger.ComponentInterfaces;
using Microsoft.VisualStudio.Debugger.Evaluation;
using Microsoft.VisualStudio.Debugger.Evaluation.ClrCompilation;
using System.Collections.ObjectModel;

namespace XSharpDebugger.ResultProvider
{
    public sealed class XSharpResultProvider : IDkmClrResultProvider
    {
        public void GetChildren(DkmEvaluationResult result, DkmWorkList workList, int initialRequestSize, DkmInspectionContext inspectionContext, DkmCompletionRoutine<DkmGetChildrenAsyncResult> completionRoutine)
        {
            Logger.Information("Debugger: ResultProvider.GetChildren");
            result.GetChildren(workList, initialRequestSize, inspectionContext, completionRoutine);
            return;
        }

        public DkmClrValue GetClrValue(DkmSuccessEvaluationResult successResult)
        {
            var value = successResult.GetClrValue();
            Logger.Information($"Debugger: ResultProvider.GetClrValue: {value.ToString()}");
            return value;
        }

        public void GetItems(DkmEvaluationResultEnumContext enumContext, DkmWorkList workList, int startIndex, int count, DkmCompletionRoutine<DkmEvaluationEnumAsyncResult> completionRoutine)
        {
            enumContext.GetItems(workList, startIndex, count, completionRoutine);
            Logger.Information($"Debugger: ResultProvider.GetItems: {count}");
            return;
        }

        public void GetResult(DkmClrValue clrValue, DkmWorkList workList, DkmClrType declaredType, DkmClrCustomTypeInfo customTypeInfo, DkmInspectionContext inspectionContext, ReadOnlyCollection<string> formatSpecifiers, string resultName, string resultFullName, DkmCompletionRoutine<DkmEvaluationAsyncResult> completionRoutine)
        {
            Logger.Information("Debugger: ResultProvider.GetResult");
            clrValue.GetResult(workList, declaredType, customTypeInfo, inspectionContext, formatSpecifiers, resultName, resultFullName, completionRoutine);
            return;
        }

        public string GetUnderlyingString(DkmEvaluationResult result)
        {
            var resString = result.GetUnderlyingString();
            Logger.Information($"Debugger: ResultProvider.GetUnderlyingString: {resString}");
            return resString;
        }
    }
}
