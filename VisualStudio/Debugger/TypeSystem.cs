//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Text;
using XSharp;
using Meta = Microsoft.VisualStudio.Debugger.Metadata;
namespace XSharpDebugger
{
    internal enum KeywordCase
    {
        None = 0,
        Upper = 1,
        Lower = 2,
        Title = 3,
    }
    /// <summary>
    /// A type in the XSharp type system.  Each unique type should have one and only one instance of
    /// the XSharpType class.  This allows us to use reference equality to equate types.
    /// </summary>
    public class XSharpType
    {
        // This defines how the types will be shown in the locals and auto window
        internal static XSharpType Byte;
        internal static XSharpType Char ;
        internal static XSharpType DWord;
        internal static XSharpType Integer;
        internal static XSharpType Int64;
        internal static XSharpType Logic;
        internal static XSharpType Object;
        internal static XSharpType Ptr;
        internal static XSharpType Real4;
        internal static XSharpType Real8;
        internal static XSharpType SByte ;
        internal static XSharpType Short ;
        internal static XSharpType String;
        internal static XSharpType UInt64;
        internal static XSharpType Void ;
        internal static XSharpType Word;

        internal static XSharpType Array;
        internal static XSharpType ArrayBase;
        internal static XSharpType Binary;
        internal static XSharpType Currency;
        internal static XSharpType Date ;
        internal static XSharpType Float;
        internal static XSharpType FoxArray;
        internal static XSharpType Psz;
        internal static XSharpType Symbol ;
        internal static XSharpType Usual;
        internal static XSharpType Invalid;

        private static readonly Dictionary<XSharpType, ArrayType> s_arrayTypes = new Dictionary<XSharpType, ArrayType>();
        private static readonly Dictionary<XSharpType, ByRefType> s_byRefTypes = new Dictionary<XSharpType, ByRefType>();
        private static readonly Dictionary<String, XSharpType> s_types = new  Dictionary<String, XSharpType>();
        private readonly string _name;

        private static readonly KeywordCase KeywordCase;

        static XSharpType()
        {
            try
            {
                KeywordCase = (KeywordCase)Constants.GetSetting(Constants.RegistryKeywordCase, (int) KeywordCase.None);
            }
            catch
            {
                KeywordCase = KeywordCase.None;
            }

            Byte = new XSharpType("Byte");
            Char = new XSharpType("Char");
            DWord = new XSharpType("Dword");
            Integer = new XSharpType("Int");
            Int64 = new XSharpType("Int64");
            Logic = new XSharpType("Logic");
            Object = new XSharpType("Object");
            Ptr = new XSharpType("Ptr");
            Real4 = new XSharpType("Real4");
            Real8 = new XSharpType("Real8");
            SByte = new XSharpType("Sbyte");
            Short = new XSharpType("Short");
            String = new XSharpType("String");
            UInt64 = new XSharpType("Uint64");
            Void = new XSharpType("Void");
            Word = new XSharpType("Word");

            Array = new XSharpType("Array");
            ArrayBase = new XSharpType("Array Of");
            Binary = new XSharpType("Binary");
            Currency = new XSharpType("Currency");
            Date = new XSharpType("Date");
            Float = new XSharpType("Float");
            FoxArray = new XSharpType("Foxarray");
            Invalid = new XSharpType("Invalid");
            Psz = new XSharpType("Psz");
            Symbol = new XSharpType("Symbol");
            Usual = new XSharpType("Usual");
            // types below are sorted by name
            s_types.Add("System.Boolean", XSharpType.Logic);
            s_types.Add("System.Byte", XSharpType.Byte);
            s_types.Add("System.Char", XSharpType.Char);
            s_types.Add("System.Double", XSharpType.Real8);
            s_types.Add("System.Int16", XSharpType.Short);
            s_types.Add("System.Int32", XSharpType.Integer);
            s_types.Add("System.Int64", XSharpType.Int64);
            s_types.Add("System.IntPtr", XSharpType.Ptr);
            s_types.Add("System.Object", XSharpType.Object);
            s_types.Add("System.SByte", XSharpType.SByte);
            s_types.Add("System.Single", XSharpType.Real4);
            s_types.Add("System.String", XSharpType.String);
            s_types.Add("System.UInt16", XSharpType.Word);
            s_types.Add("System.UInt32", XSharpType.DWord);
            s_types.Add("System.UInt64", XSharpType.UInt64);
            s_types.Add("System.Void", XSharpType.Void);

            s_types.Add("Vulcan.__Array", XSharpType.Array);
            s_types.Add("Vulcan.__Psz", XSharpType.Psz);
            s_types.Add("Vulcan.__Usual", XSharpType.Usual);
            s_types.Add("Vulcan.__Symbol", XSharpType.Symbol);
            s_types.Add("Vulcan.__VODate", XSharpType.Date);
            s_types.Add("Vulcan.__VOFloat", XSharpType.Float);

            s_types.Add("XSharp.__Array", XSharpType.Array);
            s_types.Add("XSharp.__ArrayBase", XSharpType.ArrayBase);
            s_types.Add("XSharp.__Binary", XSharpType.Binary);
            s_types.Add("XSharp.__Currency", XSharpType.Currency);
            s_types.Add("XSharp.__Date", XSharpType.Date);
            s_types.Add("XSharp.__FoxArray", XSharpType.FoxArray);
            s_types.Add("XSharp.__Float", XSharpType.Float);
            s_types.Add("XSharp.__Psz", XSharpType.Psz);
            s_types.Add("XSharp.__Symbol", XSharpType.Symbol);
            s_types.Add("XSharp.__Usual", XSharpType.Usual);
            s_types.Add("XSharp.__VODate", XSharpType.Date);
            s_types.Add("XSharp.__VOFloat", XSharpType.Float);
        }

        internal static string FormatKeyword(string keyword)
        {
            switch (KeywordCase)
            {
                case KeywordCase.Upper:
                    return keyword.ToUpper();
                case KeywordCase.Lower:
                    return keyword.ToLower();
                case KeywordCase.Title:
                    return keyword.Substring(0, 1).ToUpper() + keyword.Substring(1).ToLower();
                case KeywordCase.None:
                default:
                    return keyword;
            }
        }
        internal string Name => _name;
        protected XSharpType(string name)
        {
            _name = FormatKeyword(name);
        }

        internal static XSharpType Create (Meta.Type gentype, Meta.Type[] args)
        {
            var name = gentype.FullName;
            XSharpType baseType = Create(name);
            var resultName = baseType.ToString();
            if (baseType != ArrayBase)
                resultName += "<";
            else
                resultName += " ";
            bool first = true;
            foreach (var arg in args)
            {
                if (!first)
                    resultName += ",";
                var t = Create(arg.FullName);
                resultName += t.ToString();
                first = false;
            }
            if (baseType != ArrayBase)
            {
                resultName += ">";
            }

            var result = Create(resultName);
            return result;
        }

        internal static XSharpType Create(string name)
        {
            var pos = name.IndexOf("`");
            if (pos > 0)
                name = name.Substring(0, pos);
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
                return this == Integer || this == String || this == Logic;
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
            if (!existingTypes.TryGetValue(this, out T type))
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
