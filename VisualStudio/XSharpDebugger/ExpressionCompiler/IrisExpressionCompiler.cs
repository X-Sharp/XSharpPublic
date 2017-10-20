// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

using Microsoft.VisualStudio.Debugger.Clr;
using Microsoft.VisualStudio.Debugger.ComponentInterfaces;
using Microsoft.VisualStudio.Debugger.Evaluation;
using Microsoft.VisualStudio.Debugger.Evaluation.ClrCompilation;
using System.Collections.ObjectModel;

namespace XSharpDebugger.ExpressionCompiler
{
    /// <summary>
    /// This class is the main entry point into the Expression Compiler.  The debug engine calls
    /// into methods here for the following purposes:
    ///    1.  Expression Evaluation:  When the user hovers over values in the editor (and the
    ///        language provides a language service), uses the Quick Watch window, or adds an
    ///        expression to the Watch window, the debug engine will ultimately call
    ///        IDkmClrExpressionCompiler.CompileExpression.
    ///    2.  Local Variables List:  When the user views the Locals window or an extension asks
    ///        for arguments or local variables via the DTE, the debug engine will ultimately call
    ///        IDkmClrExpressionCompiler.GetClrLocalVariableQuery.
    ///    3.  Modification of values:  When the user edits a value from the Watch or Locals window
    ///        the debug engine will ultimately call IDkmClrExpressionCompiler.CompileAssignment
    /// 
    /// See the method comments below for more details about each method.
    /// </summary>
    public sealed class XSharpExpressionCompiler : IDkmClrExpressionCompiler
    {
        /// <summary>
        /// This method is called by the debug engine to compile an expression that the user wants
        /// to evaluate.  Before the call, we have the text of the expression and information about
        /// the context we want to evaluate in (code location, evaluation flags, etc.).  The result
        /// of the call is a &quot;query&quot; containing IL the debugger will execute to get the
        /// result of the expression.
        /// </summary>
        /// <param name="expression">This is the raw expression to compile</param>
        /// <param name="instructionAddress">Instruction address or code location to use as the
        /// context of the compilation.</param>
        /// <param name="inspectionContext">Context of the evaluation.  This contains options/flags
        /// to be used during compilation. It also contains the InspectionSession.  The inspection
        /// session is the object that provides lifetime management for our objects.  When the user
        /// steps or continues the process, the debug engine will dispose of the inspection session</param>
        /// <param name="error">[Out] If the there are any compile errors, this parameter is set to
        /// the error message to display to the user</param>
        /// <param name="result">[Out] If compilation was successful, this is the output query.</param>
        void IDkmClrExpressionCompiler.CompileExpression(
            DkmLanguageExpression expression,
            DkmClrInstructionAddress instructionAddress,
            DkmInspectionContext inspectionContext,
            out string error,
            out DkmCompiledClrInspectionQuery result)
        {
            error = null;
            result = null;
            using (DebugCompilerContext context = ContextFactory.CreateExpressionContext(inspectionContext, instructionAddress, expression.Text))
            {
                context.GenerateQuery();

                error = context.FirstError;
                if (string.IsNullOrEmpty(error))
                {
                    result = DkmCompiledClrInspectionQuery.Create(
                        instructionAddress.RuntimeInstance,
                        null,
                        expression.Language.Id,
                        new ReadOnlyCollection<byte>(context.GetPeBytes()),
                        context.ClassName,
                        context.MethodName,
                        new ReadOnlyCollection<string>(context.FormatSpecifiers),
                        context.ResultFlags,
                        DkmEvaluationResultCategory.Data,
                        DkmEvaluationResultAccessType.None,
                        DkmEvaluationResultStorageType.None,
                        DkmEvaluationResultTypeModifierFlags.None,
                        null);
                }
            }
        }

        /// <summary>
        /// This method is called by the debug engine to retrieve the current local variables.
        /// The result of this call will be a query containing the names of the local variables
        /// as well as IL code to retrieve each variable value.
        /// </summary>
        /// <param name="inspectionContext">Context of the evaluation.  This contains options/flags
        /// to be used during compilation. It also contains the InspectionSession.  The inspection
        /// session is the object that provides lifetime management for our objects.  When the user
        /// steps or continues the process, the debug engine will dispose of the inspection session</param>
        /// <param name="instructionAddress">Instruction address or code location to use as the
        /// reference point for where we need to retrieve the local variables</param>
        /// <param name="argumentsOnly">True if only arguments are needed</param>
        /// <returns>A local variables query</returns>
        DkmCompiledClrLocalsQuery IDkmClrExpressionCompiler.GetClrLocalVariableQuery(DkmInspectionContext inspectionContext, DkmClrInstructionAddress instructionAddress, bool argumentsOnly)
        {
            using (DebugCompilerContext context = ContextFactory.CreateLocalsContext(inspectionContext, instructionAddress, argumentsOnly))
            {
                context.GenerateQuery();

                return DkmCompiledClrLocalsQuery.Create(
                    inspectionContext.RuntimeInstance,
                    null,
                    inspectionContext.Language.Id,
                    new ReadOnlyCollection<byte>(context.GetPeBytes()),
                    context.ClassName,
                    new ReadOnlyCollection<DkmClrLocalVariableInfo>(context.GeneratedLocals));
            }
        }

        /// <summary>
        /// This method is called by the debug engine when the user modifies the result of a
        /// previous evaluation.  The result of this call will be a query containing the IL code
        /// necessary to assign the value.
        /// </summary>
        /// <param name="expression">The text the user entered as the new value</param>
        /// <param name="instructionAddress">Instruction address or code location to use as the
        /// context of the compilation.</param>
        /// <param name="lValue">The L-Value of the assigment.  This is a previous evaluation result.</param>
        /// <param name="error">[Out] If the there are any compile errors, this parameter is set to
        /// the error message to display to the user</param>
        /// <param name="result">[Out] If compilation was successful, this is the output query to
        /// execute to perform the assignment.</param>
        void IDkmClrExpressionCompiler.CompileAssignment(DkmLanguageExpression expression, DkmClrInstructionAddress instructionAddress, DkmEvaluationResult lValue, out string error, out DkmCompiledClrInspectionQuery result)
        {
            error = null;
            result = null;
            using (DebugCompilerContext context = ContextFactory.CreateAssignmentContext(lValue, instructionAddress, expression.Text))
            {
                context.GenerateQuery();

                error = context.FirstError;
                if (string.IsNullOrEmpty(error))
                {
                    result = DkmCompiledClrInspectionQuery.Create(
                        instructionAddress.RuntimeInstance,
                        null,
                        expression.Language.Id,
                        new ReadOnlyCollection<byte>(context.GetPeBytes()),
                        context.ClassName,
                        context.MethodName,
                        new ReadOnlyCollection<string>(context.FormatSpecifiers),
                        DkmClrCompilationResultFlags.None,
                        DkmEvaluationResultCategory.Data,
                        DkmEvaluationResultAccessType.None,
                        DkmEvaluationResultStorageType.None,
                        DkmEvaluationResultTypeModifierFlags.None,
                        null);
                }
            }
        }
    }
}
