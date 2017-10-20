// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

using System.Reflection.Metadata;

namespace XSharpDebugger
{
    /// <summary>
    /// An abstract base class representing something that can be a member of an imported type.
    /// </summary>
    public abstract class ImportedMember
    {
        public readonly ImportedType DeclaringType;
        public readonly ImportedModule Module;

        private StringHandle _nameHandle;
        private string _cachedName;

        protected ImportedMember(ImportedModule module, StringHandle nameHandle, ImportedType declaringType)
        {
            Module = module;
            DeclaringType = declaringType;

            _nameHandle = nameHandle;
        }

        public string Name
        {
            get
            {
                if (_cachedName != null)
                    return _cachedName;

                _cachedName = Module.Reader.GetString(_nameHandle);

                return _cachedName;
            }
        }

        public abstract bool IsPublic
        {
            get;
        }

        public abstract bool IsStatic
        {
            get;
        }
    }
}
