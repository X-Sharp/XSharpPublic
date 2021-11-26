//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.IO;
using System.Reflection.PortableExecutable;

namespace XSharpDebugger
{
    /// <summary>
    /// This class handles reading and ownership of a PE file that is imported into the compiler
    /// </summary>
    internal sealed class ImportedFile : IDisposable
    {
        private PEReader _peReader;
        private Stream _peStream;

        private ImportedFile(Stream peStream)
        {
            _peReader = new PEReader(peStream);
            _peStream = peStream;
        }

        public ImportedModule Module
        {
            get
            {
                return new ImportedModule(_peReader.GetMetadata());
            }
        }

        public void Dispose()
        {
            _peReader.Dispose();
            _peStream.Dispose();
        }

        public static ImportedFile Create(string path)
        {
            Stream input = File.OpenRead(path);
            return new ImportedFile(input);
        }
    }
}
