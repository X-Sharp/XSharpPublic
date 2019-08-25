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
        internal Symbol BindMemberAccess(ref Expr expr, ref Expr member, BindAffinity affinity)
        {
            if (member is NameExpr MemberName)
            {
                if (affinity == BindAffinity.Invoke)
                {
                    var s = Lookup(expr.Datatype, MemberName.LookupName);
                    if (s == null)
                    {
                        if (Options.Binding.HasFlag(BindOptions.AllowDynamic) && expr.Datatype.IsUsualOrObject())
                        {
                            Convert(ref expr, Compilation.Get(NativeType.Usual) ?? Compilation.Get(NativeType.Object));
                            return new DynamicSymbol(MemberName.LookupName);
                        }
                        else
                            throw Binder.LookupError(expr, MemberName);
                    }
                    return s;
                }
                else
                {
                    Convert(ref expr, Compilation.Get(NativeType.Object));
                    return new DynamicSymbol(MemberName.LookupName);
                }
            }
            else if (member is RuntimeIdExpr)
            {
                Bind(ref member, BindAffinity.Member);
                return new DynamicExprSymbol(member);
            }
            else
                throw member.Error(ErrorCode.NameExpected);
        }
    }
}
