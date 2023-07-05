//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using Type = Microsoft.VisualStudio.Debugger.Metadata.Type;

namespace XSharpDebugger
{
    internal static class Logger
    {
        internal static void Exception(Exception e, string msg)
        {
            XSharp.Settings.XSettings.Logger.Exception(e, msg);
        }
    }

    internal static class Utility
    {
        /// <summary>
        /// Convert a type from the debugger's type system into X# type system
        /// </summary>
        /// <param name="lmrType">LMR Type</param>
        /// <returns>X# type</returns>
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
            else if (lmrType.IsGenericType)
            {
                var args = lmrType.GetGenericArguments();
                var td = lmrType.GetGenericTypeDefinition();
                return XSharpType.Create(td,args);
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
