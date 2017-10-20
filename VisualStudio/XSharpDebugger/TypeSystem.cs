// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

using System;
using System.Collections.Generic;
using System.Text;

namespace XSharpDebugger
{
    /// <summary>
    /// A type in the Iris type system.  Each unique type should have one and only one instance of
    /// the IrisType class.  This allows us to use reference equality to equate types.
    /// </summary>
    public class IrisType
    {
        public static readonly IrisType Invalid = new IrisType("!invalid");
        public static readonly IrisType Integer = new IrisType("integer");
        public static readonly IrisType String = new IrisType("string");
        public static readonly IrisType Boolean = new IrisType("boolean");
        public static readonly IrisType Void = new IrisType("void");

        private static Dictionary<IrisType, ArrayType> s_arrayTypes = new Dictionary<IrisType, ArrayType>();
        private static Dictionary<IrisType, ByRefType> s_byRefTypes = new Dictionary<IrisType, ByRefType>();
        private readonly string _name;

        protected IrisType(string name)
        {
            _name = name;
        }

        public virtual bool IsArray
        {
            get
            {
                return false;
            }
        }

        public virtual bool IsByRef
        {
            get
            {
                return false;
            }
        }

        public virtual bool IsFunction
        {
            get
            {
                return false;
            }
        }

        public virtual bool IsProcedure
        {
            get
            {
                return false;
            }
        }

        public virtual bool IsMethod
        {
            get
            {
                return false;
            }
        }

        public bool IsPrimitive
        {
            get
            {
                return this == Integer || this == String || this == Boolean;
            }
        }

        public override string ToString()
        {
            return _name;
        }

        public IrisType MakeArrayType()
        {
            return MakeCompoundType(s_arrayTypes, t => ArrayType.InternalCreate(t));
        }

        public IrisType MakeByRefType()
        {
            return MakeCompoundType(s_byRefTypes, t => ByRefType.InternalCreate(t));
        }

        public virtual IrisType GetElementType()
        {
            throw new InvalidOperationException("Type does not have an element type.");
        }

        private IrisType MakeCompoundType<T>(Dictionary<IrisType, T> existingTypes, Func<IrisType, T> factory)
            where T : IrisType
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

    public class Method : IrisType
    {
        private readonly Variable[] _parameters;

        protected Method(string name, Variable[] parameters)
            : base(name)
        {
            _parameters = parameters;
        }

        public override bool IsMethod
        {
            get
            {
                return true;
            }
        }

        public virtual IrisType ReturnType
        {
            get
            {
                return Void;
            }
        }

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
                    builder.Append("; ");

                builder.Append(param);
                first = false;
            }

            builder.Append(')');
            return builder.ToString();
        }
    }

    public class Function : Method
    {
        private readonly IrisType _returnType;

        protected Function(IrisType returnType, Variable[] parameters)
            : base("function", parameters)
        {
            _returnType = returnType;
        }

        public override bool IsFunction
        {
            get
            {
                return true;
            }
        }

        public override IrisType ReturnType
        {
            get
            {
                return _returnType;
            }
        }

        public static Function Create(IrisType returnType, Variable[] parameters)
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

        public override bool IsProcedure
        {
            get
            {
                return true;
            }
        }

        public static Procedure Create(Variable[] parameters)
        {
            return new Procedure(parameters);
        }
    }

    public class ArrayType : IrisType
    {
        private readonly IrisType _elementType;

        protected ArrayType(IrisType elementType)
            : base(string.Format("array of {0}", elementType))
        {
            _elementType = elementType;
        }

        public override bool IsArray
        {
            get
            {
                return true;
            }
        }

        public override IrisType GetElementType()
        {
            return _elementType;
        }

        internal static ArrayType InternalCreate(IrisType elementType)
        {
            return new ArrayType(elementType);
        }
    }

    public class ByRefType : IrisType
    {
        private readonly IrisType _elementType;

        protected ByRefType(IrisType elementType)
            : base(elementType + "&")
        {
            _elementType = elementType;
        }

        public override bool IsByRef
        {
            get
            {
                return true;
            }
        }

        public override IrisType GetElementType()
        {
            return _elementType;
        }

        internal static ByRefType InternalCreate(IrisType elementType)
        {
            return new ByRefType(elementType);
        }
    }
}