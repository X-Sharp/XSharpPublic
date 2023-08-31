//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//#define XDEBUG
using LanguageService.CodeAnalysis;
using LanguageService.CodeAnalysis.XSharp;
using Eval=LanguageService.CodeAnalysis.XSharp.ExpressionEvaluator;
using Microsoft.VisualStudio.Debugger;
using Microsoft.VisualStudio.Debugger.CallStack;
using Microsoft.VisualStudio.Debugger.Clr;
using Microsoft.VisualStudio.Debugger.ComponentInterfaces;
using Microsoft.VisualStudio.Debugger.Evaluation;
using Microsoft.VisualStudio.Debugger.Evaluation.ClrCompilation;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using XSharp.Settings;

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
    public sealed class XSharpExpressionCompiler : IDkmClrExpressionCompiler,
        IDkmLanguageInstructionDecoder,
        IDkmClrExpressionCompilerCallback,
        IDkmLanguageFrameDecoder
    {
        IDkmClrExpressionCompiler compiler;
        IDkmLanguageFrameDecoder decoder;

        public XSharpExpressionCompiler()
        {
            if (!XSettings.IsVs15)
            {
                UpdateXSharpParseOptions();
                compiler = new Eval.XSharpExpressionCompiler();
                decoder = new Eval.XSharpFrameDecoder();
            }
            else
            {
                compiler = null;
                decoder = new FrameDecoder.XSharpFrameDecoder();
            }
        }
        static void UpdateXSharpParseOptions()
        {
            var xoptions = Eval.XSyntaxHelpers.XSharpOptions;
            xoptions.SetDialect((XSharpDialect)XDebuggerSettings.Dialect);
            xoptions.SetOption(CompilerOption.MemVars, XDebuggerSettings.MemVars);
            xoptions.SetOption(CompilerOption.UndeclaredMemVars, XDebuggerSettings.UndeclaredMemvars);
            xoptions.SetOption(CompilerOption.ArrayZero, XDebuggerSettings.ArrayZero);
            xoptions.SetOption(CompilerOption.Vo4, XDebuggerSettings.Vo4);
            xoptions.SetOption(CompilerOption.Vo6, XDebuggerSettings.Vo6);
            xoptions.SetOption(CompilerOption.Vo7, XDebuggerSettings.Vo7);
            xoptions.SetOption(CompilerOption.Vo10, XDebuggerSettings.Vo10);
            xoptions.SetOption(CompilerOption.Vo12, XDebuggerSettings.Vo12);
            xoptions.SetOption(CompilerOption.Vo13, XDebuggerSettings.Vo13);
            xoptions.SetOption(CompilerOption.Vo14, XDebuggerSettings.Vo14);
            xoptions.SetOption(CompilerOption.LateBinding, XDebuggerSettings.LateBinding && !XDebuggerSettings.NoLateBinding);
            xoptions.SetOption(CompilerOption.AllowDotForInstanceMembers, true);
            XSharpString.CaseSensitive = XDebuggerSettings.CaseSensitive;
            Eval.XSyntaxHelpers.XSharpOptions = xoptions;
        }

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
#if XDEBUG
            XSolution.WriteOutputMessage("CompileExpression: " + expression.Text);
#endif
            if (compiler != null)
            {
                NewCompileExpression(expression, instructionAddress, inspectionContext, out error, out result);
            }
            else
            {
                OldCompileExpression(expression, instructionAddress, inspectionContext, out error, out result);
            }
        }
        void NewCompileExpression(
                    DkmLanguageExpression expression,
                    DkmClrInstructionAddress instructionAddress,
                    DkmInspectionContext inspectionContext,
                    out string error,
                    out DkmCompiledClrInspectionQuery result)
        {
            try
            {
                UpdateXSharpParseOptions();
                compiler.CompileExpression(expression, instructionAddress, inspectionContext, out error, out result);
            }
            catch (Exception e)
            {
                Logger.Exception(e, "Debugger:CompileExpression");
                OldCompileExpression(expression, instructionAddress, inspectionContext, out error, out result);
            }
        }

        void OldCompileExpression(
                    DkmLanguageExpression expression,
                    DkmClrInstructionAddress instructionAddress,
                    DkmInspectionContext inspectionContext,
                    out string error,
                    out DkmCompiledClrInspectionQuery result)
        {
            error = null;
            result = null;
            bool changed = false;
            string originalExpr = expression.Text;
            // We use a trick to change the Text when sending it to C#, by retrieving the field info.
            // This field has a property get but not a property set.
            var fi = typeof(DkmLanguageExpression).GetField("m_Text", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
            if (fi != null)
            {
                var newexpr = originalExpr;
                if (expression.Text.StartsWith("SELF", System.StringComparison.OrdinalIgnoreCase))
                {
                    changed = true;
                    newexpr = "this" + originalExpr.Substring(4);
                }
                else if (expression.Text.StartsWith("SUPER", System.StringComparison.OrdinalIgnoreCase))
                {
                    changed = true;
                    newexpr = "base" + originalExpr.Substring(5);
                }
                if (newexpr.Contains(":"))
                {
                    newexpr = newexpr.Replace(':', '.');
                    changed = true;
                }
                if (changed && fi != null)
                {
                    fi.SetValue(expression, newexpr);
                }

            }
            expression.CompileExpression(instructionAddress, inspectionContext, out error, out result);
            if (changed && fi != null)
            {
                fi.SetValue(expression, originalExpr);
            }
            return;
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
            DkmCompiledClrLocalsQuery result;
#if XDEBUG
            XSolution.WriteOutputMessage("GetClrLocalVariableQuery ");
#endif
            if (compiler != null)
            {
                result = NewClrLocalVariableQuery(inspectionContext, instructionAddress, argumentsOnly);
            }
            else
            {
                result = OldClrLocalVariableQuery(inspectionContext, instructionAddress, argumentsOnly);
            }
            var newlocals = new List<DkmClrLocalVariableInfo>();
            bool changed = false;
            foreach (var loc in result.LocalInfo)
            {
                if (loc.VariableName.Contains("$"))
                {
                    // do not add
                    changed = true;
                }
                else if (loc.VariableName == "this")
                {
                    // rename
                    var selfName = XSharpType.FormatKeyword("SELF");
                    var newloc = DkmClrLocalVariableInfo.Create(selfName, selfName, loc.MethodName, loc.CompilationFlags, loc.ResultCategory, loc.CustomTypeInfo);
                    newlocals.Add(newloc);
                    changed = true;
                }
                else
                {
                    newlocals.Add(loc);
                }
            }
            if (changed)
            {
                result = DkmCompiledClrLocalsQuery.Create(result.RuntimeInstance,
                    result.DataContainer, result.LanguageId, result.Binary, result.TypeName,
                    new ReadOnlyCollection<DkmClrLocalVariableInfo>(newlocals));
            }
            return result;
        }

        DkmCompiledClrLocalsQuery NewClrLocalVariableQuery(DkmInspectionContext inspectionContext, DkmClrInstructionAddress instructionAddress, bool argumentsOnly)
        {
            try
            {
                UpdateXSharpParseOptions();
                return compiler.GetClrLocalVariableQuery(inspectionContext, instructionAddress, argumentsOnly);
            }
            catch (Exception e)
            {
                Logger.Exception(e, "Debugger:ClrLocalVariableQuery");
                return OldClrLocalVariableQuery(inspectionContext, instructionAddress, argumentsOnly);
            }
        }
        DkmCompiledClrLocalsQuery OldClrLocalVariableQuery(DkmInspectionContext inspectionContext, DkmClrInstructionAddress instructionAddress, bool argumentsOnly)
        {
            return inspectionContext.GetClrLocalVariableQuery(instructionAddress, argumentsOnly);
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
#if XDEBUG
            XSolution.WriteOutputMessage("CompileAssignment: "+expression.Text);
#endif
            if (compiler != null)
            {
                NewCompileAssignment(expression, instructionAddress, lValue, out error, out result);
            }
            else
            {
                OldCompileAssignment(expression, instructionAddress, lValue, out error, out result);
            }

        }
        void NewCompileAssignment(DkmLanguageExpression expression, DkmClrInstructionAddress instructionAddress, DkmEvaluationResult lValue, out string error, out DkmCompiledClrInspectionQuery result)
        {
            try
            {
                UpdateXSharpParseOptions();
                compiler.CompileAssignment(expression, instructionAddress, lValue, out error, out result);
            }
            catch (Exception e)
            {
                Logger.Exception(e, "Debugger:CompileAssignment");
                OldCompileAssignment(expression, instructionAddress, lValue, out error, out result);
            }
        }
        void OldCompileAssignment(DkmLanguageExpression expression, DkmClrInstructionAddress instructionAddress, DkmEvaluationResult lValue, out string error, out DkmCompiledClrInspectionQuery result)
        {
            expression.CompileAssignment(instructionAddress, lValue, out error, out result);
        }

        public string GetMethodName(DkmLanguageInstructionAddress languageInstructionAddress, DkmVariableInfoFlags argumentFlags)
        {
            if (compiler != null)
            {
                var e = (IDkmLanguageInstructionDecoder)compiler;
                var result = e.GetMethodName(languageInstructionAddress, argumentFlags);
                return result;
            }
            return languageInstructionAddress.GetMethodName(argumentFlags);
        }

        public void CompileDisplayAttribute(DkmLanguageExpression expression, DkmClrModuleInstance moduleInstance, int token, out string error, out DkmCompiledClrInspectionQuery result)
        {
            if (compiler != null)
            {
                var e = (IDkmClrExpressionCompilerCallback)compiler;
                e.CompileDisplayAttribute(expression, moduleInstance, token, out error, out result);
            }
            else
            {
                expression.CompileDisplayAttribute(moduleInstance, token, out error, out result);
            }
        }
        public void GetFrameName(DkmInspectionContext inspectionContext, DkmWorkList workList, DkmStackWalkFrame frame, DkmVariableInfoFlags argumentFlags, DkmCompletionRoutine<DkmGetFrameNameAsyncResult> completionRoutine)
        {
            if (decoder != null)
            {
                decoder.GetFrameName(inspectionContext, workList, frame, argumentFlags, completionRoutine);
            }
            else
            {
                inspectionContext.GetFrameName(workList, frame, argumentFlags, completionRoutine);
            }
        }

        public void GetFrameReturnType(DkmInspectionContext inspectionContext, DkmWorkList workList, DkmStackWalkFrame frame, DkmCompletionRoutine<DkmGetFrameReturnTypeAsyncResult> completionRoutine)
        {
            if (decoder != null)
            {
                decoder.GetFrameReturnType(inspectionContext, workList, frame, completionRoutine);
            }
            else
            {
                inspectionContext.GetFrameReturnType(workList, frame, completionRoutine);
            }
        }
    }
}
