// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

using Microsoft.VisualStudio.Debugger.Evaluation;
using Microsoft.VisualStudio.Debugger.Evaluation.ClrCompilation;
using System.Collections.Generic;

namespace XSharpDebugger.ExpressionCompiler
{
    /// <summary>
    /// A subclass of the Translator class that is specific to generating IL to get local
    /// variable values in the debugger.
    /// </summary>
    internal class LocalVariablesTranslator : Translator
    {
        private Dictionary<IrisType, Function> _functions = new Dictionary<IrisType, Function>();
        private DebugCompilerContext _context;

        public LocalVariablesTranslator(DebugCompilerContext context)
            : base(context)
        {
            _context = context;
        }

        public override void TranslateInput()
        {
            _context.Emitter.BeginProgram(_context.ClassName, _context.Importer.ImportedAssemblies);

            foreach (Symbol global in _context.SymbolTable.Global)
                MaybeGenerateEntryForSymbol(global);

            foreach (Symbol local in _context.SymbolTable.Local)
                MaybeGenerateEntryForSymbol(local);

            _context.Emitter.EndProgram();
        }

        private void MaybeGenerateEntryForSymbol(Symbol symbol)
        {
            if (_context.ArgumentsOnly && symbol.StorageClass != StorageClass.Argument)
            {
                // We are only showing arguments
                return;
            }

            IrisType symbolType = symbol.Type;
            if (symbolType.IsMethod || symbolType == IrisType.Invalid || symbolType == IrisType.Void)
            {
                // This symbol doesn't belong in the Locals window.
                // Don't generate an entry for it.
                return;
            }

            if (symbol.Name.StartsWith("$."))
            {
                // Don't show compiler internal symbols
                return;
            }

            string methodName = _context.NextMethodName();
            IrisType derefType = DerefType(symbolType);

            // Emit code for the method to get the symbol value.
            MethodGenerator.BeginMethod(methodName, derefType, _context.ParameterVariables, _context.LocalVariables, entryPoint: false, methodFileName: null);
            EmitLoadSymbol(symbol, SymbolLoadMode.Dereference);
            MethodGenerator.EndMethod();

            // Generate the local entry to pass back to the debug engine
            DkmClrCompilationResultFlags resultFlags = DkmClrCompilationResultFlags.None;
            if (derefType == IrisType.Boolean)
            {
                // The debugger uses "BoolResult" for breakpoint conditions so setting the flag
                // here has no effect currently, but we set it for the sake of consistency.
                resultFlags |= DkmClrCompilationResultFlags.BoolResult;
            }
            else if (derefType.IsArray)
            {
                // Iris doesn't support modification of an array itself
                resultFlags |= DkmClrCompilationResultFlags.ReadOnlyResult;
            }

            string fullName = symbol.Name;

            _context.GeneratedLocals.Add(DkmClrLocalVariableInfo.Create(
                fullName,
                fullName,
                methodName,
                resultFlags,
                DkmEvaluationResultCategory.Data,
                null));
        }
    }
}
