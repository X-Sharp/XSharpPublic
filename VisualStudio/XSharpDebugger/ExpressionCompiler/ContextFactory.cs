// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

using Microsoft.VisualStudio.Debugger.Clr;
using Microsoft.VisualStudio.Debugger.Evaluation;
using Microsoft.VisualStudio.Debugger.Evaluation.ClrCompilation;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace XSharpDebugger.ExpressionCompiler
{
    /// <summary>
    /// Factory for creating instances of DebugCompilerContext.  We use a factory here because
    /// creation of the context is somewhat non-trivial.
    /// </summary>
    internal static class ContextFactory
    {
        public static DebugCompilerContext CreateExpressionContext(DkmInspectionContext inspectionContext, DkmClrInstructionAddress address, string expression)
        {
            InspectionSession ownedSession = null;
            InspectionScope scope;
            if (inspectionContext != null)
            {
                InspectionSession session = InspectionSession.GetInstance(inspectionContext.InspectionSession);
                scope = session.GetScope(address);
            }
            else
            {
                // There is no inspection context when compiling breakpoint conditions.  Create a
                // new temporary session.  The context will need to dispose of this new session
                // when it is disposed.
                ownedSession = new InspectionSession();
                scope = ownedSession.GetScope(address);
            }

            MemoryStream input;
            StreamReader reader;
            CreateInputStream(expression, out input, out reader);

            DebugCompilerContext context = new DebugCompilerContext(
                ownedSession,
                scope,
                input,
                reader,
                typeof(ExpressionTranslator),
                "$.M1",
                null /* Generated locals is not applicable for compiling expressions */,
                null /* Assignment L-Value only applies to assigments */,
                false /* "ArgumentsOnly" only applies to local variable query */);
            context.InitializeSymbols();

            return context;
        }

        public static DebugCompilerContext CreateAssignmentContext(DkmEvaluationResult lValue, DkmClrInstructionAddress address, string expression)
        {
            MemoryStream input;
            StreamReader reader;
            CreateInputStream(expression, out input, out reader);

            InspectionSession session = InspectionSession.GetInstance(lValue.InspectionSession);
            InspectionScope scope = session.GetScope(address);

            DebugCompilerContext context = new DebugCompilerContext(
                null /* null because the context doesn't own the lifetime of the session */,
                scope,
                input,
                reader,
                typeof(AssignmentTranslator),
                "$.M1",
                null /* Generated locals is not applicable for assigments */,
                lValue.FullName,
                false /* "ArgumentsOnly" only applies to local variable query */);
            context.InitializeSymbols();

            return context;
        }

        public static DebugCompilerContext CreateLocalsContext(DkmInspectionContext inspectionContext, DkmClrInstructionAddress address, bool argumentsOnly)
        {
            MemoryStream input;
            StreamReader reader;
            CreateInputStream(string.Empty, out input, out reader);

            InspectionSession session = InspectionSession.GetInstance(inspectionContext.InspectionSession);
            InspectionScope scope = session.GetScope(address);

            DebugCompilerContext context = new DebugCompilerContext(
                null /* null because the context doesn't own the lifetime of the session */,
                scope,
                input,
                reader,
                typeof(LocalVariablesTranslator),
                null /* Method name is not applicable because we create multiple methods for Locals. */,
                new List<DkmClrLocalVariableInfo>(),
                null /* Assignment L-Value only applies to assigments */,
                argumentsOnly);
            context.InitializeSymbols();

            return context;
        }

        private static void CreateInputStream(string expression, out MemoryStream input, out StreamReader reader)
        {
            byte[] buffer = Encoding.Default.GetBytes(expression);
            input = new MemoryStream(buffer);
            reader = new StreamReader(input);
        }
    }
}
