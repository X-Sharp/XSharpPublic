//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Debugger;
using Microsoft.VisualStudio.Debugger.Clr;
using Microsoft.VisualStudio.Debugger.Symbols;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;


namespace XSharpDebugger
{
    /// <summary>
    /// A scope to do evaluations in.
    /// 
    /// This class does translation from the debug engine's / CLR's understanding of the current
    /// scope into scope information that's understood by the X# compiler.
    /// </summary>
    internal class InspectionScope
    {
        public readonly DkmClrInstructionAddress InstructionAddress;
        public readonly DkmModule SymModule;
        public readonly int CurrentMethodToken;
        public readonly InspectionSession Session;

        private Dictionary<string, ImportedModule> _modules = new Dictionary<string, ImportedModule>(StringComparer.OrdinalIgnoreCase);
        private ImportedModule _mscorlib;
        private ImportedMethod _currentMethod;
        private LocalVariable[] _cachedLocals;

        public InspectionScope(DkmClrInstructionAddress address, InspectionSession session)
        {
            InstructionAddress = address;
            SymModule = address.ModuleInstance.Module;
            CurrentMethodToken = address.MethodId.Token;
            Session = session;
        }

        public ImportedMethod TryImportCurrentMethod()
        {
            if (_currentMethod != null)
                return _currentMethod;

            IntPtr metadataBlock;
            uint blockSize;
            try
            {
                metadataBlock = InstructionAddress.ModuleInstance.GetMetaDataBytesPtr(out blockSize);
            }
            catch (DkmException)
            {
                // This can fail when dump debugging if the full heap is not available
                return null;
            }

            ImportedModule module = Session.Importer.ImportModule(metadataBlock, blockSize);
            _currentMethod = module.GetMethod(InstructionAddress.MethodId.Token);
            return _currentMethod;
        }

        public LocalVariable[] GetLocals()
        {
            if (_cachedLocals != null)
                return _cachedLocals;

            ImportedMethod method = TryImportCurrentMethod();
            _cachedLocals = GetLocalsImpl(method).ToArray();

            return _cachedLocals;
        }

        public ImportedModule ImportMscorlib()
        {
            if (_mscorlib != null)
                return _mscorlib;

            DkmClrAppDomain currentAppDomain = InstructionAddress.ModuleInstance.AppDomain;
            if (currentAppDomain.IsUnloaded)
                return null;

            foreach (DkmClrModuleInstance moduleInstance in currentAppDomain.GetClrModuleInstances())
            {
                if (!moduleInstance.IsUnloaded && moduleInstance.ClrFlags.HasFlag(DkmClrModuleFlags.RuntimeModule))
                {
                    _mscorlib = ImportModule(moduleInstance);
                    return _mscorlib;
                }
            }

            return null;
        }

        public ImportedModule ImportModule(string name)
        {
            ImportedModule result;
            if (!_modules.TryGetValue(name, out result))
            {
                DkmClrAppDomain currentAppDomain = InstructionAddress.ModuleInstance.AppDomain;
                if (currentAppDomain.IsUnloaded)
                    return null;

                foreach (DkmClrModuleInstance moduleInstance in currentAppDomain.GetClrModuleInstances())
                {
                    if (!moduleInstance.IsUnloaded)
                    {
                        string path = moduleInstance.FullName;
                        string fileName = Path.GetFileName(path);
                        if (string.Equals(name, fileName, StringComparison.OrdinalIgnoreCase))
                        {
                            result = ImportModule(moduleInstance);
                            if (result != null)
                                _modules.Add(name, result);
                            break;
                        }
                    }
                }
            }

            return result;
        }

        private IEnumerable<LocalVariable> GetLocalsImpl(ImportedMethod method)
        {
            if (method != null)
            {
                // Get the local symbols from the PDB (symbol file).  If symbols aren't loaded, we
                // can't show any local variables
                DkmClrLocalVariable[] symbols = GetLocalSymbolsFromPdb().ToArray();
                if (symbols.Length != 0)
                {
                    // To determine the local types, we need to decode the local variable signature
                    // token.  Get the token from the debugger, then use our importer
                    // to get the variables types.  We can then construct the correlated list of local
                    // types and names.
                    int localVarSigToken = InstructionAddress.ModuleInstance.GetLocalSignatureToken(CurrentMethodToken);
                    ImmutableArray<XSharpType> localTypes = method.Module.DecodeLocalVariableTypes(localVarSigToken);
                    foreach (DkmClrLocalVariable localSymbol in symbols)
                    {
                        int slot = localSymbol.Slot;
                        yield return new LocalVariable(localSymbol.Name, localTypes[slot], slot);
                    }
                }
            }
        }

        private IEnumerable<DkmClrLocalVariable> GetLocalSymbolsFromPdb()
        {
            // We need symbols to get local variables
            if (SymModule != null)
            {
                DkmClrMethodScopeData[] scopes = SymModule.GetMethodSymbolStoreData(InstructionAddress.MethodId);
                foreach (DkmClrMethodScopeData scope in scopes)
                {
                    if (InScope(scope))
                    {
                        foreach (DkmClrLocalVariable var in scope.LocalVariables)
                            yield return var;
                    }
                }
            }
        }

        private ImportedModule ImportModule(DkmClrModuleInstance debuggerModule)
        {
            IntPtr metadataBlock;
            uint blockSize;
            try
            {
                metadataBlock = debuggerModule.GetMetaDataBytesPtr(out blockSize);
                return Session.Importer.ImportModule(metadataBlock, blockSize);
            }
            catch (DkmException)
            {
                // This can fail when dump debugging if the full heap is not available
                return null;
            }
        }

        private bool InScope(DkmClrMethodScopeData scope)
        {
            uint offset = InstructionAddress.ILOffset;
            return scope.ILRange.StartOffset <= offset && scope.ILRange.EndOffset >= offset;
        }
    }
}
