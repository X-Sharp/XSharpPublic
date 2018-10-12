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
        NotImplemented,
        NotSupported,
        UnexpectedValue,
        NoConversion,
    }

    internal class ErrorString
    {
        static private Dictionary<ErrorCode, string> _errorStrings = new Dictionary<ErrorCode, string>()
        {
            { ErrorCode.NotImplemented, "Feature not implemented: {0}" },
            { ErrorCode.NotSupported, "Not supported: {0}" },
            { ErrorCode.UnexpectedValue, "Unexpected value" },
            { ErrorCode.NoConversion, "No conversion from {0} to {1}" },
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
        public CompileFailure(ErrorCode e, params object[] args): base(ErrorString.Format(e, args)) { Code = e; }
    }
}