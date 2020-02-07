//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Debugger.Clr;
using Microsoft.VisualStudio.Debugger.ComponentInterfaces;
using Microsoft.VisualStudio.Debugger.Evaluation;
using Microsoft.VisualStudio.Debugger.Evaluation.ClrCompilation;
using System.Collections.ObjectModel;
using System.Collections.Generic;
using System.Collections.Immutable;

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
            bool changed = false;
            string originalExpr = expression.Text;
            // We use a trick to change the Text when sending it to C#, by retrieveing the field info.
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
                // check for literal array
                var lbrkt = newexpr.IndexOf('[');
                var rbrkt = newexpr.IndexOf(']');
                if (lbrkt > 0 && rbrkt > 0 && lbrkt < rbrkt)
                {
                    newexpr = AdjustArrayIndices(newexpr, ref changed);
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
            var result = inspectionContext.GetClrLocalVariableQuery(instructionAddress, argumentsOnly);
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
                    var newloc = DkmClrLocalVariableInfo.Create("SELF", "SELF", loc.MethodName, loc.CompilationFlags, loc.ResultCategory, loc.CustomTypeInfo);
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
            // when the user assigns a value in the debugger then this method is called.
            // we may want to change an expression like "{1,2,3}" to "new object[] {1,2,3}"
            // and "2020.12.03" to "XSharp.RT.Functions.ConDate(2020,12,03)"
            error = null;
            result = null;
            expression.CompileAssignment(instructionAddress, lValue, out error, out result);
        }

        private static string AdjustArrayIndices(string newexpr, ref bool changed)
        {
            var sb = new System.Text.StringBuilder();
            bool instring = false;
            bool inindex = false;
            foreach (var c in newexpr)
            {
                switch (c)
                {
                    case '[':
                        if (!instring)
                        {
                            inindex = true;
                        }
                        sb.Append(c);
                        break;
                    case ',':
                        if (!instring && inindex)
                        {
                            sb.Append("-1][");
                            changed = true;
                        }
                        else
                        {
                            sb.Append(c);
                        }
                        break;
                    case ']':
                        if (!instring)
                        {
                            sb.Append("-1");
                            changed = true;
                            inindex = false;
                        }
                        sb.Append(c);
                        break;
                    case '"':
                        instring = !instring;
                        sb.Append(c);
                        break;
                    default:
                        sb.Append(c);
                        break;
                }
            }
            return sb.ToString();
        }
    }
}
