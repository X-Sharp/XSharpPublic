//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Reflection.Metadata;

namespace XSharpDebugger
{
    /// <summary>
    /// The Importer class manages modules that are imported into the compiler.  These imported
    /// modules may either be references used during compilation or modules being debugged.
    /// </summary>
    public sealed class Importer : IDisposable
    {
        private List<ImportedFile> _importedFiles = new List<ImportedFile>();

        private HashSet<string> _importedAssemblyNames = new HashSet<string>();
        private Dictionary<string, ImportedModule> _modulePathMap = new Dictionary<string, ImportedModule>();
        private Dictionary<IntPtr, ImportedModule> _modulePtrMap = new Dictionary<IntPtr, ImportedModule>();

        public IEnumerable<string> ImportedAssemblies
        {
            get
            {
                return _importedAssemblyNames;
            }
        }

        public void Dispose()
        {
            foreach (ImportedFile file in _importedFiles)
                file.Dispose();

            _importedAssemblyNames.Clear();
            _importedFiles.Clear();
            _modulePathMap.Clear();
        }

        public ImportedModule ImportModule(string path)
        {
            ImportedModule module;
            if (!_modulePathMap.TryGetValue(path, out module))
            {
                ImportedFile file = ImportedFile.Create(path);
                module = file.Module;

                _importedFiles.Add(file);
                _modulePathMap.Add(path, module);

                AddAssembly(module);
            }

            return module;
        }

        public ImportedModule ImportModule(IntPtr metadataPtr, uint blockSize)
        {
            ImportedModule module;
            if (!_modulePtrMap.TryGetValue(metadataPtr, out module))
            {
                module = new ImportedModule(metadataPtr, blockSize);
                _modulePtrMap.Add(metadataPtr, module);
                AddAssembly(module);
            }

            return module;
        }

        private void AddAssembly(ImportedModule module)
        {
            MetadataReader mdReader = module.Reader;
            AssemblyDefinition assemblyDef = mdReader.GetAssemblyDefinition();
            string name = mdReader.GetString(assemblyDef.Name);
            if (!_importedAssemblyNames.Contains(name))
                _importedAssemblyNames.Add(name);
        }
    }
}
