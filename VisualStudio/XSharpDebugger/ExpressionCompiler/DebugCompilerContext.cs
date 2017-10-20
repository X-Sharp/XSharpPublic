// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

using Microsoft.VisualStudio.Debugger.Evaluation.ClrCompilation;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace XSharpDebugger.ExpressionCompiler
{
    /// <summary>
    /// A subclass of the CompilerContext class to deal with all of the context for the various
    /// types of compilation we need to do in the debugger.
    /// </summary>
    internal class DebugCompilerContext : CompilerContext
    {
        public readonly List<string> FormatSpecifiers = new List<string>();
        public readonly List<DkmClrLocalVariableInfo> GeneratedLocals;
        public readonly InspectionScope Scope;
        public readonly string AssignmentLValue;
        public readonly string ClassName;
        public readonly string MethodName;
        public readonly bool ArgumentsOnly;

        private static uint s_nextClass;

        private InspectionSession _ownedSession;
        private MemoryStream _input;
        private StreamReader _reader;
        private Method _irisMethod;
        private Type _translatorType;
        private uint _nextMethod;

        public DebugCompilerContext(
            InspectionSession ownedSession,
            InspectionScope scope,
            MemoryStream input,
            StreamReader reader,
            Type translatorType,
            string methodName,
            List<DkmClrLocalVariableInfo> generatedLocals,
            string assignmentLValue,
            bool argumentsOnly)
            : this(scope.Session.Importer, reader, CompilationFlags.NoDebug | CompilationFlags.WriteDll)
        {
            _ownedSession = ownedSession;
            _input = input;
            _reader = reader;
            _translatorType = translatorType;
            Scope = scope;
            MethodName = methodName;
            ClassName = string.Format("$.C{0}", s_nextClass++);
            GeneratedLocals = generatedLocals;
            AssignmentLValue = assignmentLValue;
            ArgumentsOnly = argumentsOnly;
        }

        private DebugCompilerContext(Importer importer, StreamReader inputReader, CompilationFlags flags)
            : base("fake.iris", inputReader, importer, new PeEmitter(flags), flags)
        {
        }

        public DkmClrCompilationResultFlags ResultFlags
        {
            get;
            set;
        }

        public Variable[] ParameterVariables
        {
            get
            {
                return _irisMethod.GetParameters();
            }
        }

        public Variable[] LocalVariables
        {
            get
            {
                return Scope.GetLocals().Select(v => v.Variable).ToArray();
            }
        }

        public byte[] GetPeBytes()
        {
            if (ErrorCount == 0)
            {
                Emitter.Flush();
                return ((PeEmitter)Emitter).GetPeBytes();
            }
            else
            {
                return null;
            }
        }

        public void InitializeSymbols()
        {
            ImportedMethod currentMethod = Scope.TryImportCurrentMethod();
            if (currentMethod == null)
                return; // Nothing to evaluate if we can't get the current method

            // Add compiler intrinsics
            AddIntrinsics();

            // Add debugger intrinsics
            // (Not implemented yet)

            // Add globals
            ImportedType type = currentMethod.DeclaringType;
            foreach (ImportedField importedfield in type.GetFields())
            {
                IrisType irisType = importedfield.FieldType;
                if (irisType != IrisType.Invalid)
                    SymbolTable.Add(importedfield.Name, irisType, StorageClass.Global, importedfield);
            }

            // Add methods
            foreach (ImportedMethod importedMethod in type.GetMethods())
            {
                Method method = importedMethod.ConvertToIrisMethod();
                if (IsValidMethod(method))
                    SymbolTable.Add(importedMethod.Name, method, StorageClass.Global, importedMethod);
            }

            // Create symbol for query method and transition the SymbolTable to method scope
            _irisMethod = currentMethod.ConvertToIrisMethod();
            SymbolTable.OpenMethod("$.query", _irisMethod);

            // Add symbols for parameters
            foreach (Variable param in _irisMethod.GetParameters())
                SymbolTable.Add(param.Name, param.Type, StorageClass.Argument);

            // Add symbols for local variables
            foreach (LocalVariable local in Scope.GetLocals())
                SymbolTable.Add(local.Name, local.Type, StorageClass.Local, local.Slot);
        }

        public void GenerateQuery()
        {
            Translator.TranslateInput();
        }

        public string NextMethodName()
        {
            return string.Format("$.M{0}", _nextMethod++);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (_reader != null)
                {
                    _reader.Dispose();
                    _reader = null;
                }

                if (_input != null)
                {
                    _input.Dispose();
                    _input = null;
                }

                if (_ownedSession != null)
                {
                    _ownedSession.Dispose();
                    _ownedSession = null;
                }
            }

            base.Dispose(disposing);
        }

        protected override ImportedModule ReferenceMscorlib()
        {
            return Scope.ImportMscorlib();
        }

        protected override ImportedModule ReferenceExternal(string moduleName)
        {
            return Scope.ImportModule(moduleName);
        }

        protected override Translator CreateTranslator()
        {
            return (Translator)Activator.CreateInstance(_translatorType, this);
        }

        private bool IsValidMethod(Method method)
        {
            Function func = method as Function;
            if (func != null && func.ReturnType == IrisType.Invalid)
                return false;

            foreach (Variable param in method.GetParameters())
            {
                if (param.Type == IrisType.Invalid)
                    return false;
            }

            return true;
        }
    }
}
