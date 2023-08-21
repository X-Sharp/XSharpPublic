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
            result.GetChildren(workList, initialRequestSize, inspectionContext, completionRoutine);
            return;
        }

        public DkmClrValue GetClrValue(DkmSuccessEvaluationResult successResult)
        {
            return successResult.GetClrValue();
        }

        public void GetItems(DkmEvaluationResultEnumContext enumContext, DkmWorkList workList, int startIndex, int count, DkmCompletionRoutine<DkmEvaluationEnumAsyncResult> completionRoutine)
        {
            enumContext.GetItems(workList, startIndex, count, completionRoutine);
            return;
        }

        public void GetResult(DkmClrValue clrValue, DkmWorkList workList, DkmClrType declaredType, DkmClrCustomTypeInfo customTypeInfo, DkmInspectionContext inspectionContext, ReadOnlyCollection<string> formatSpecifiers, string resultName, string resultFullName, DkmCompletionRoutine<DkmEvaluationAsyncResult> completionRoutine)
        {
            clrValue.GetResult(workList, declaredType, customTypeInfo, inspectionContext, formatSpecifiers, resultName, resultFullName, completionRoutine);
            return;
        }

        public string GetUnderlyingString(DkmEvaluationResult result)
        {
            var resString = result.GetUnderlyingString();
            return resString;
        }
    }
}
