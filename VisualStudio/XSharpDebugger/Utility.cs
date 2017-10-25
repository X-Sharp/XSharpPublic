// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.


using Type = Microsoft.VisualStudio.Debugger.Metadata.Type;

namespace XSharpDebugger
{
    internal static class Utility
    {
        /// <summary>
        /// Convert a type from the debugger's type system into Iris's type system
        /// </summary>
        /// <param name="lmrType">LMR Type</param>
        /// <returns>Iris type</returns>
        public static XSharpType GetXSharpTypeForLmrType(Type lmrType)
        {
            if (lmrType.IsPrimitive)
            {
                return XSharpType.Create(lmrType.FullName);
            }
            else if (lmrType.IsArray)
            {

                XSharpType elementType = GetXSharpTypeForLmrType(lmrType.GetElementType());

                return elementType.MakeArrayType();
            }
            else if (lmrType.IsByRef)
            {
                XSharpType elementType = GetXSharpTypeForLmrType(lmrType.GetElementType());

                return elementType.MakeByRefType();
            }
            else if (lmrType.FullName.Equals("System.String"))
            {
                return XSharpType.String;
            }

            // Unknown
            return XSharpType.Create(lmrType.FullName);
        }
    }
}
