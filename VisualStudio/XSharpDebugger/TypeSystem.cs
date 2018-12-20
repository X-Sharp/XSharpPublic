//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Text;

namespace XSharpDebugger
{
    /// <summary>
    /// A type in the XSharp type system.  Each unique type should have one and only one instance of
    /// the XSharpType class.  This allows us to use reference equality to equate types.
    /// </summary>
    public class XSharpType
    {
        public static readonly XSharpType Char = new XSharpType("CHAR");
        public static readonly XSharpType Real4 = new XSharpType("REAL4");
        public static readonly XSharpType Real8 = new XSharpType("REAL8");
        public static readonly XSharpType Ptr = new XSharpType("PTR");
        public static readonly XSharpType SByte = new XSharpType("SBYTE");
        public static readonly XSharpType Integer = new XSharpType("INT");
        public static readonly XSharpType Int64 = new XSharpType("INT64");
        public static readonly XSharpType Short = new XSharpType("SHORT");
        public static readonly XSharpType Byte = new XSharpType("BYTE");
        public static readonly XSharpType Word = new XSharpType("WORD");
        public static readonly XSharpType DWord = new XSharpType("DWORD");
        public static readonly XSharpType UInt64 = new XSharpType("UINT64");
        public static readonly XSharpType String = new XSharpType("STRING");
        public static readonly XSharpType Boolean = new XSharpType("LOGIC");
        public static readonly XSharpType Void = new XSharpType("VOID");
        public static readonly XSharpType Object = new XSharpType("OBJECT");

        public static readonly XSharpType Usual = new XSharpType("USUAL");
        public static readonly XSharpType Date = new XSharpType("DATE");
        public static readonly XSharpType Float = new XSharpType("FLOAT");
        public static readonly XSharpType Symbol = new XSharpType("SYMBOL");
        public static readonly XSharpType Array = new XSharpType("ARRAY");
        public static readonly XSharpType Psz = new XSharpType("PSZ");
        public static readonly XSharpType Invalid = new XSharpType("INVALID");

        private static Dictionary<XSharpType, ArrayType> s_arrayTypes = new Dictionary<XSharpType, ArrayType>();
        private static Dictionary<XSharpType, ByRefType> s_byRefTypes = new Dictionary<XSharpType, ByRefType>();
        private static Dictionary<String, XSharpType> s_types = new  Dictionary<String, XSharpType>();
        private readonly string _name;

        static XSharpType()
        {
            s_types.Add("System.SByte", XSharpType.SByte);
            s_types.Add("System.Int16", XSharpType.Short);
            s_types.Add("System.Int32", XSharpType.Integer);
            s_types.Add("System.Int64", XSharpType.Int64);
            s_types.Add("System.Byte", XSharpType.Byte);
            s_types.Add("System.UInt16", XSharpType.Word);
            s_types.Add("System.UInt32", XSharpType.DWord);
            s_types.Add("System.UInt64", XSharpType.UInt64);
            s_types.Add("System.Single", XSharpType.Real4);
            s_types.Add("System.Double", XSharpType.Real8);
            s_types.Add("System.Boolean", XSharpType.Boolean);
            s_types.Add("System.Void", XSharpType.Void);
            s_types.Add("System.IntPtr", XSharpType.Ptr);
            s_types.Add("System.Char", XSharpType.Char);
            s_types.Add("System.String", XSharpType.String);
            s_types.Add("System.Object", XSharpType.Object);
            s_types.Add("Vulcan.__Usual", XSharpType.Usual);
            s_types.Add("Vulcan.__VODate", XSharpType.Date);
            s_types.Add("Vulcan.__VOFloat", XSharpType.Float);
            s_types.Add("Vulcan.__Symbol", XSharpType.Symbol);
            s_types.Add("Vulcan.__Array", XSharpType.Array);
            s_types.Add("Vulcan.__Psz", XSharpType.Psz);
            s_types.Add("XSharp.__Usual", XSharpType.Usual);
            s_types.Add("XSharp.__Date", XSharpType.Date);
            s_types.Add("XSharp.__Float", XSharpType.Float);
            s_types.Add("XSharp.__VODate", XSharpType.Date);
            s_types.Add("XSharp.__VOFloat", XSharpType.Float);
            s_types.Add("XSharp.__Symbol", XSharpType.Symbol);
            s_types.Add("XSharp.__Array", XSharpType.Array);
            s_types.Add("XSharp.__Psz", XSharpType.Psz);
        }


        internal string Name => _name;
        protected XSharpType(string name)
        {
            _name = name;
        }
        internal static XSharpType Create(string name)
        {
            if (s_types.ContainsKey(name))
                return s_types[name];
            var result = new XSharpType(name);
            s_types.Add(name, result);
            return result;
        }

        public virtual bool IsArray => false;

        public virtual bool IsByRef => false;

        public virtual bool IsFunction => false;

        public virtual bool IsProcedure => false;

        public virtual bool IsMethod => false;

        public bool IsPrimitive
        {
            get
            {
                return this == Integer || this == String || this == Boolean;
            }
        }

        public override string ToString() => _name;

        public XSharpType MakeArrayType()
        {
            return MakeCompoundType(s_arrayTypes, t => ArrayType.InternalCreate(t));
        }

        public XSharpType MakeByRefType()
        {
            return MakeCompoundType(s_byRefTypes, t => ByRefType.InternalCreate(t));
        }

        public virtual XSharpType GetElementType()
        {
            throw new InvalidOperationException("Type does not have an element type.");
        }

        private XSharpType MakeCompoundType<T>(Dictionary<XSharpType, T> existingTypes, Func<XSharpType, T> factory)
            where T : XSharpType
        {
            T type;
            if (!existingTypes.TryGetValue(this, out type))
            {
                type = factory(this);
                existingTypes.Add(this, type);
            }

            return type;
        }
    }

    public class Method : XSharpType
    {
        private readonly Variable[] _parameters;

        protected Method(string name, Variable[] parameters)
            : base(name)
        {
            _parameters = parameters;
        }

        public override bool IsMethod => true;

        public virtual XSharpType ReturnType => Void;

        public Variable[] GetParameters()
        {
            Variable[] result = new Variable[_parameters.Length];
            _parameters.CopyTo(result, 0);
            return result;
        }

        public override string ToString()
        {
            StringBuilder builder = new StringBuilder();
            builder.Append(base.ToString());
            builder.Append('(');

            bool first = true;
            foreach (Variable param in GetParameters())
            {
                if (!first)
                    builder.Append(", ");

                builder.Append(param);
                first = false;
            }

            builder.Append(')');
            return builder.ToString();
        }
    }

    public class Function : Method
    {
        private readonly XSharpType _returnType;

        protected Function(XSharpType returnType, Variable[] parameters)
            : base("function", parameters)
        {
            _returnType = returnType;
        }

        public override bool IsFunction => true;

        public override XSharpType ReturnType => _returnType;

        public static Function Create(XSharpType returnType, Variable[] parameters)
        {
            return new Function(returnType, parameters);
        }

        public override string ToString()
        {
            return base.ToString() + " : " + ReturnType.ToString();
        }
    }

    public class Procedure : Method
    {
        protected Procedure(Variable[] parameters)
            : base("procedure", parameters)
        {
        }

        public override bool IsProcedure => true;

        public static Procedure Create(Variable[] parameters)
        {
            return new Procedure(parameters);
        }
    }

    public class ArrayType : XSharpType
    {
        private readonly XSharpType _elementType;

        protected ArrayType(XSharpType elementType)
            : base(string.Format("{0}[]", elementType))
        {
            _elementType = elementType;
        }

        public override bool IsArray => true;

        public override XSharpType GetElementType() => _elementType;

        internal static ArrayType InternalCreate(XSharpType elementType)
        {
            return new ArrayType(elementType);
        }
    }

    public class ByRefType : XSharpType
    {
        private readonly XSharpType _elementType;

        protected ByRefType(XSharpType elementType)
            : base(elementType + "&")
        {
            _elementType = elementType;
        }

        public override bool IsByRef => true;

        public override XSharpType GetElementType() => _elementType;

        internal static ByRefType InternalCreate(XSharpType elementType)
        {
            return new ByRefType(elementType);
        }
    }
}
