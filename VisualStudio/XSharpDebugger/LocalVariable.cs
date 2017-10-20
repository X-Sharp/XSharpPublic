// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.


namespace XSharpDebugger
{
    /// <summary>
    /// LocalVariable is a pairing of an Iris Variable and the slot number the value is stored in.
    /// </summary>
    internal class LocalVariable
    {
        public readonly Variable Variable;
        public readonly int Slot;

        public LocalVariable(string name, IrisType type, int slot)
        {
            Variable = new Variable(type, name);
            Slot = slot;
        }

        public string Name
        {
            get
            {
                return Variable.Name;
            }
        }

        public IrisType Type
        {
            get
            {
                return Variable.Type;
            }
        }
    }
}
