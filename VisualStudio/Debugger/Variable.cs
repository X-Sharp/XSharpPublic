//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
namespace XSharpDebugger
{
    /// <summary>
    /// This class represents a 'variable' in the X# language.  A variable consists of a name
    /// and a type.
    /// </summary>
    public sealed class Variable
    {
        public readonly string Name;
        public readonly XSharpType Type;
        public bool In { get; set; }                // Parameters only
        public bool Out { get; set; }               // Parameters only
        public Variable(XSharpType type, string name)
        {
            Type = type;
            Name = name;
        }

 
        public override string ToString()
        {
            return string.Format("{0} AS {1}", Name, Type);
        }
    }
}
