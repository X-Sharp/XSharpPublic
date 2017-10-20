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
        public static IrisType GetIrisTypeForLmrType(Type lmrType)
        {
            if (lmrType.IsPrimitive)
            {
                switch (lmrType.FullName)
                {
                    case "System.Int32":
                        return IrisType.Integer;
                    case "System.Boolean":
                        return IrisType.Boolean;
                }
            }
            else if (lmrType.IsArray)
            {
                if (lmrType.GetArrayRank() != 1)
                    return IrisType.Invalid;

                IrisType elementType = GetIrisTypeForLmrType(lmrType.GetElementType());
                if (elementType == IrisType.Invalid)
                    return IrisType.Invalid;

                return elementType.MakeArrayType();
            }
            else if (lmrType.IsByRef)
            {
                IrisType elementType = GetIrisTypeForLmrType(lmrType.GetElementType());
                if (elementType == IrisType.Invalid)
                    return IrisType.Invalid;

                return elementType.MakeByRefType();
            }
            else if (lmrType.FullName.Equals("System.String"))
            {
                return IrisType.String;
            }

            // Unknown
            return IrisType.Invalid;
        }
    }
}
