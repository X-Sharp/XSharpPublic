//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis
{
    internal partial class ConstantValue
    {
        public virtual IntPtr IntPtrValue { get { throw new InvalidOperationException(); } }


        public static ConstantValue Create(IntPtr value)
        {
            if (value == default(IntPtr))
            {
                return ConstantValueDefault.IntPtr;
            }

            return new ConstantValueIntPtr(value);
        }

        public static ConstantValue CreateVoid()
        {
            return new ConstantValueVoid();
        }
        private partial class ConstantValueDefault : ConstantValueDiscriminated
        {

            public static readonly ConstantValueDefault IntPtr = new ConstantValueDefault(ConstantValueTypeDiscriminator.IntPtr);

            public override IntPtr IntPtrValue
            {
                get
                {
                    return default(IntPtr);
                }
            }
        }
        private sealed class ConstantValueIntPtr : ConstantValue
        {
            private readonly IntPtr _value;

            public ConstantValueIntPtr(IntPtr value)
            {
                _value = value;
            }

            public override ConstantValueTypeDiscriminator Discriminator
            {
                get
                {
                    return ConstantValueTypeDiscriminator.IntPtr;
                }
            }

            internal override SpecialType SpecialType
            {
                get { return SpecialType.System_IntPtr; }
            }

            public override IntPtr IntPtrValue
            {
                get
                {
                    return _value;
                }
            }

            public override int GetHashCode()
            {
                return Hash.Combine(base.GetHashCode(), _value.GetHashCode());
            }

            public override bool Equals(ConstantValue other)
            {
                return base.Equals(other) && _value == other.IntPtrValue;
            }
        }

        private sealed class ConstantValueVoid : ConstantValue
        {
            public ConstantValueVoid()
            {
            }

            public override ConstantValueTypeDiscriminator Discriminator
            {
                get
                {
                    return ConstantValueTypeDiscriminator.Void;
                }
            }

            internal override SpecialType SpecialType
            {
                get { return SpecialType.System_Void; }
            }

            public override int GetHashCode()
            {
                return Hash.Combine(base.GetHashCode(), 0.GetHashCode());
            }

            public override bool Equals(ConstantValue other)
            {
                return base.Equals(other) && other.IsVoid;
            }
        }
    }
}
