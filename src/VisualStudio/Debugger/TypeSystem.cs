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
using XSharp.Settings;
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
        internal static XSharpType UsualType;
        internal static XSharpType Invalid;

        private static readonly Dictionary<XSharpType, ArrayType> s_arrayTypes = new Dictionary<XSharpType, ArrayType>();
        private static readonly Dictionary<XSharpType, ByRefType> s_byRefTypes = new Dictionary<XSharpType, ByRefType>();
        private static readonly Dictionary<String, XSharpType> s_types = new  Dictionary<String, XSharpType>();
        private readonly string _name;

        private static KeywordCase KeywordCase;


        static KeywordCase CurrentKeywordCase()
        {
            try
            {
                return (KeywordCase)Constants.GetSetting(Constants.RegistryKeywordCase, (int)KeywordCase.None);
            }
            catch
            {
                return KeywordCase.None;
            }
            
        }
        static void InitTypes()
        {
            try
            {
                KeywordCase = (KeywordCase)Constants.GetSetting(Constants.RegistryKeywordCase, (int)KeywordCase.None);
            }
            catch
            {
                KeywordCase = KeywordCase.None;
            }


            s_arrayTypes.Clear();
            s_byRefTypes.Clear();
            s_types.Clear();
            XSharpType.Byte = new XSharpType("Byte", true);
            XSharpType.Char = new XSharpType("Char", true);
            XSharpType.DWord = new XSharpType("Dword", true);
            XSharpType.Integer = new XSharpType("Int", true);
            XSharpType.Int64 = new XSharpType("Int64", true);
            XSharpType.Logic = new XSharpType("Logic", true);
            XSharpType.Object = new XSharpType("Object", true);
            XSharpType.Ptr = new XSharpType("Ptr", true);
            XSharpType.Real4 = new XSharpType("Real4", true);
            XSharpType.Real8 = new XSharpType("Real8", true);
            XSharpType.SByte = new XSharpType("Sbyte", true);
            XSharpType.Short = new XSharpType("Short", true);
            XSharpType.String = new XSharpType("String", true);
            XSharpType.UInt64 = new XSharpType("Uint64", true);
            XSharpType.Void = new XSharpType("Void", true);
            XSharpType.Word = new XSharpType("Word", true);

            XSharpType.Array = new XSharpType("Array", true);
            XSharpType.ArrayBase = new XSharpType("Array Of", true);
            XSharpType.Binary = new XSharpType("Binary", true);
            XSharpType.Currency = new XSharpType("Currency", true);
            XSharpType.Date = new XSharpType("Date", true);
            XSharpType.Float = new XSharpType("Float", true);
            XSharpType.FoxArray = new XSharpType("Foxarray", true);
            XSharpType.Invalid = new XSharpType("Invalid");
            XSharpType.Psz = new XSharpType("Psz", true);
            XSharpType.Symbol = new XSharpType("Symbol", true);
            XSharpType.Usual = new XSharpType("Usual", true);
            XSharpType.UsualType = new XSharpType("UsualType", true);
            // types below are sorted by name
            s_types.Add(KnownTypes.SystemBoolean, XSharpType.Logic);
            s_types.Add(KnownTypes.SystemByte, XSharpType.Byte);
            s_types.Add(KnownTypes.SystemChar, XSharpType.Char);
            s_types.Add(KnownTypes.SystemDouble, XSharpType.Real8);
            s_types.Add(KnownTypes.SystemInt16, XSharpType.Short);
            s_types.Add(KnownTypes.SystemInt32, XSharpType.Integer);
            s_types.Add(KnownTypes.SystemInt64, XSharpType.Int64);
            s_types.Add(KnownTypes.SystemIntPtr, XSharpType.Ptr);
            s_types.Add(KnownTypes.SystemObject, XSharpType.Object);
            s_types.Add(KnownTypes.SystemSByte, XSharpType.SByte);
            s_types.Add(KnownTypes.SystemSingle, XSharpType.Real4);
            s_types.Add(KnownTypes.SystemString, XSharpType.String);
            s_types.Add(KnownTypes.SystemUInt16, XSharpType.Word);
            s_types.Add(KnownTypes.SystemUInt32, XSharpType.DWord);
            s_types.Add(KnownTypes.SystemUInt64, XSharpType.UInt64);
            s_types.Add(KnownTypes.SystemVoid, XSharpType.Void);
            s_types.Add(KnownTypes.SystemVoidPtr, XSharpType.Ptr);

            s_types.Add(KnownTypes.VulcanArray, XSharpType.Array);
            s_types.Add(KnownTypes.VulcanPSZ, XSharpType.Psz);
            s_types.Add(KnownTypes.VulcanUsual, XSharpType.Usual);
            s_types.Add(KnownTypes.VulcanUsualType, XSharpType.UsualType);
            s_types.Add(KnownTypes.VulcanSymbol, XSharpType.Symbol);
            s_types.Add(KnownTypes.VulcanDate, XSharpType.Date);
            s_types.Add(KnownTypes.VulcanFloat, XSharpType.Float);

            s_types.Add(KnownTypes.XSharpArray, XSharpType.Array);
            s_types.Add(KnownTypes.XSharpArrayBase, XSharpType.ArrayBase);
            s_types.Add(KnownTypes.XSharpBinary, XSharpType.Binary);
            s_types.Add(KnownTypes.XSharpCurrency, XSharpType.Currency);
            s_types.Add(KnownTypes.XSharpDate, XSharpType.Date);
            s_types.Add(KnownTypes.XSharpFoxArray, XSharpType.FoxArray);
            s_types.Add(KnownTypes.XSharpFloat, XSharpType.Float);
            s_types.Add(KnownTypes.XSharpPSZ, XSharpType.Psz);
            s_types.Add(KnownTypes.XSharpSymbol, XSharpType.Symbol);
            s_types.Add(KnownTypes.XSharpUsual, XSharpType.Usual);
            s_types.Add(KnownTypes.XSharpUsualType, XSharpType.UsualType);
            s_types.Add(KnownTypes.XSharpVODate, XSharpType.Date);
            s_types.Add(KnownTypes.XSharpVOFloat, XSharpType.Float);
        }

        static XSharpType()
        {
            InitTypes();
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
        internal static void CheckCase()
        {
            if (KeywordCase != CurrentKeywordCase())
            {
                InitTypes();
            }
        }
        internal string Name => _name;
        protected XSharpType(string name, bool casesync = false)
        {
            
            if (casesync)
            {
                _name = FormatKeyword(name);
            }
            else
            {
                _name = name;
            }
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
			XSharpType result;
            if (s_types.TryGetValue(name, out result))
                return result;
            result = new XSharpType(name);
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
