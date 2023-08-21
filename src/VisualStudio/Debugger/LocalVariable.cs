//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

namespace XSharpDebugger
{
    /// <summary>
    /// LocalVariable is a pairing of an X# Variable and the slot number the value is stored in.
    /// </summary>
    internal class LocalVariable
    {
        public readonly Variable Variable;
        public readonly int Slot;

        public LocalVariable(string name, XSharpType type, int slot)
        {
            Variable = new Variable(type, name);
            Slot = slot;
        }

        public string Name => Variable.Name;

        public XSharpType Type => Variable.Type;
    }
}
