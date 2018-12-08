using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;

namespace XSharp.MacroCompiler
{
    public enum ErrorCode
    {
        NoError,
        NotImplemented,
        Expected,
        Unexpected,
        BadNumArgs,
        UnterminatedString,
        NotSupported,
        UnexpectedValue,
        NoConversion,
        TypeNotFound,
    }

    internal class ErrorString
    {
        static private Dictionary<ErrorCode, string> _errorStrings = new Dictionary<ErrorCode, string>()
        {
            { ErrorCode.NoError, "No error" },
            { ErrorCode.NotImplemented, "Feature not implemented: {0}" },
            { ErrorCode.Expected, "Expected {0}" },
            { ErrorCode.Unexpected, "Unexpected {0}" },
            { ErrorCode.BadNumArgs, "Bad number of arguments (expected {0})" },
            { ErrorCode.UnterminatedString, "Unterminated string" },
            { ErrorCode.NotSupported, "Not supported: {0}" },
            { ErrorCode.UnexpectedValue, "Unexpected value" },
            { ErrorCode.NoConversion, "No conversion from {0} to {1}" },
            { ErrorCode.TypeNotFound, "Type not found: {0}" },
        };

        static internal string Get(ErrorCode e) { return _errorStrings[e]; }

        static internal string Format(ErrorCode e, params object[] args) { return String.Format(_errorStrings[e], args); }

#if DEBUG
        static ErrorString()
        {
            var errors = (ErrorCode[])Enum.GetValues(typeof(ErrorCode));
            foreach (var e in errors)
            {
                string v;
                _errorStrings.TryGetValue(e, out v);
                Debug.Assert(v != null);
            }
        }
#endif
    }

    public class CompileFailure : Exception
    {
        public readonly ErrorCode Code;
        public readonly SourceLocation Location;
        public string DiagnosticMessage
        {
            get
            {
                return Location.Valid ?
                      String.Format("({1},{2}): error XM{0:D4}: {3}", (int)Code, Location.Line, Location.Col, Message)
                    : String.Format("error XM{0:D4}: {1}", (int)Code, Message);
            }
        }
        internal CompileFailure(ErrorCode e, params object[] args): base(ErrorString.Format(e, args)) { Code = e; Location = SourceLocation.None; }
        internal CompileFailure(int offset, ErrorCode e, params object[] args) : base(ErrorString.Format(e, args)) { Code = e; Location = new SourceLocation(offset); }
        internal CompileFailure(SourceLocation loc, ErrorCode e, params object[] args) : base(ErrorString.Format(e, args)) { Code = e; Location = loc; }
        internal CompileFailure(CompileFailure e, string source) : base(e.Message) { Code = e.Code; Location = new SourceLocation(source, e.Location); }
    }

    public static partial class Compilation
    {
        internal static CompileFailure Error(ErrorCode e, params object[] args) { return new CompileFailure(e, args); }
        internal static CompileFailure Error(int offset, ErrorCode e, params object[] args) { return new CompileFailure(offset, e, args); }
        internal static CompileFailure Error(SourceLocation loc, ErrorCode e, params object[] args) { return new CompileFailure(loc, e, args); }
        internal static CompileFailure Error(Syntax.Token t, ErrorCode e, params object[] args) { return new CompileFailure(t.start, e, args); }
    }
}