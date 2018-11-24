using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;

namespace XSharp.MacroCompiler
{
    using Syntax;

    internal partial class Binder
    {
        static bool TypesMatch(TypeSymbol t1, TypeSymbol t2)
        {
            return t1 == t2;
        }

        internal static TypeSymbol GetNativeTypeFromToken(TokenType kind)
        {
            switch (kind)
            {
                case TokenType.ARRAY:
                case TokenType.CODEBLOCK:
                case TokenType.DATE:
                case TokenType.FLOAT:
                case TokenType.PSZ:
                case TokenType.SYMBOL:
                    // TODO nvk: create special types
                    throw new NotImplementedException();
                case TokenType.USUAL:
                    return Compilation.GetNativeType(NativeType.Usual);
                case TokenType.BYTE:
                    return Compilation.GetNativeType(NativeType.Byte);
                case TokenType.CHAR:
                    return Compilation.GetNativeType(NativeType.Char);
                case TokenType.DATETIME:
                    return Compilation.GetNativeType(NativeType.DateTime);
                case TokenType.DECIMAL:
                    return Compilation.GetNativeType(NativeType.Decimal);
                case TokenType.DWORD:
                    return Compilation.GetNativeType(NativeType.UInt32);
                case TokenType.DYNAMIC:
                    return Compilation.GetNativeType(NativeType.Object); // TODO nvk: special dynamic type?
                case TokenType.INT:
                    return Compilation.GetNativeType(NativeType.Int32);
                case TokenType.INT64:
                    return Compilation.GetNativeType(NativeType.Int64);
                case TokenType.LOGIC:
                    return Compilation.GetNativeType(NativeType.Boolean);
                case TokenType.LONGINT:
                    return Compilation.GetNativeType(NativeType.Int32);
                case TokenType.OBJECT:
                    return Compilation.GetNativeType(NativeType.Object);
                case TokenType.PTR:
                    return Compilation.GetNativeType(NativeType.Unknown); // TODO nvk: PTR type
                case TokenType.REAL4:
                    return Compilation.GetNativeType(NativeType.Single);
                case TokenType.REAL8:
                    return Compilation.GetNativeType(NativeType.Double);
                case TokenType.SHORTINT:
                    return Compilation.GetNativeType(NativeType.Int16);
                case TokenType.STRING:
                    return Compilation.GetNativeType(NativeType.String);
                case TokenType.UINT64:
                    return Compilation.GetNativeType(NativeType.UInt64);
                case TokenType.VOID:
                    return Compilation.GetNativeType(NativeType.Void);
                case TokenType.WORD:
                    return Compilation.GetNativeType(NativeType.UInt16);
                default:
                    throw new NotImplementedException();
            }
        }
    }
}