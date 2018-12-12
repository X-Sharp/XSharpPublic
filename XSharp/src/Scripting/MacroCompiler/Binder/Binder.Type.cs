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
        internal static bool TypesMatch(TypeSymbol t1, TypeSymbol t2)
        {
            return t1 == t2;
        }

        internal static TypeSymbol GetNativeTypeFromToken(TokenType kind)
        {
            switch (kind)
            {
                case TokenType.ARRAY:
                    return Compilation.Get(NativeType.Array);
                case TokenType.CODEBLOCK:
                    return Compilation.Get(NativeType.Codeblock);
                case TokenType.DATE:
                    return Compilation.Get(NativeType.VODate);
                case TokenType.FLOAT:
                    return Compilation.Get(NativeType.VOFloat);
                case TokenType.PSZ:
                    return Compilation.Get(NativeType.Psz);
                case TokenType.SYMBOL:
                    return Compilation.Get(NativeType.Symbol);
                case TokenType.USUAL:
                    return Compilation.Get(NativeType.Usual);
                case TokenType.BYTE:
                    return Compilation.Get(NativeType.Byte);
                case TokenType.CHAR:
                    return Compilation.Get(NativeType.Char);
                case TokenType.DATETIME:
                    return Compilation.Get(NativeType.DateTime);
                case TokenType.DECIMAL:
                    return Compilation.Get(NativeType.Decimal);
                case TokenType.DWORD:
                    return Compilation.Get(NativeType.UInt32);
                case TokenType.DYNAMIC:
                    return Compilation.Get(NativeType.Object); // TODO nvk: special dynamic type?
                case TokenType.INT:
                    return Compilation.Get(NativeType.Int32);
                case TokenType.INT64:
                    return Compilation.Get(NativeType.Int64);
                case TokenType.LOGIC:
                    return Compilation.Get(NativeType.Boolean);
                case TokenType.LONGINT:
                    return Compilation.Get(NativeType.Int32);
                case TokenType.OBJECT:
                    return Compilation.Get(NativeType.Object);
                case TokenType.PTR:
                    return Compilation.Get(NativeType.Ptr);
                case TokenType.REAL4:
                    return Compilation.Get(NativeType.Single);
                case TokenType.REAL8:
                    return Compilation.Get(NativeType.Double);
                case TokenType.SHORTINT:
                    return Compilation.Get(NativeType.Int16);
                case TokenType.STRING:
                    return Compilation.Get(NativeType.String);
                case TokenType.UINT64:
                    return Compilation.Get(NativeType.UInt64);
                case TokenType.VOID:
                    return Compilation.Get(NativeType.Void);
                case TokenType.WORD:
                    return Compilation.Get(NativeType.UInt16);
                default:
                    return null;
            }
        }
    }
}
