using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.LanguageService.Formatting
{
    /// <summary>
    /// This structure with the size of an Int is used to store
    /// single token or double token keywords that are using
    /// for the formatting code
    /// </summary>
    [StructLayout(LayoutKind.Explicit, Size = 4)]
    internal struct XToken : IEquatable<XToken>, IEqualityComparer<XToken>
    {
        [FieldOffset(0)]
        private readonly XKeyword _kw1;
        [FieldOffset(2)]
        private readonly XKeyword _kw2;
        [FieldOffset(0)]
        private int _code;

        /// <summary>
        /// Validate that the code is included in the XKeyword enum
        /// </summary>
        /// <param name="code"></param>
        [Conditional("DEBUG")]
        private static void Validate(int code)
        {
            if (code > short.MaxValue)
            {
                Community.VisualStudio.Toolkit.VS.MessageBox.Show("Code is > Int16.MaxValue");
            }
            var values = Enum.GetValues(typeof(XKeyword));
            foreach (var value in values)
            {
                if ((short) code == (short) value)
                    return;
            }
            Community.VisualStudio.Toolkit.VS.MessageBox.Show($"Code {code} is not in XKeyword Enum");

        }
        internal bool isEmpty => _code == 0;
        internal bool isEnd => _kw2 == XKeyword.End;

        internal XToken(int kw)
        {
            _code = 0;
            Validate(kw);
            _kw1 = XKeyword.None;
            _kw2 = (XKeyword)kw;
        }

        internal XToken(XKeyword kw)
        {
            _code = 0;
            _kw1 = XKeyword.None;
            _kw2 = kw;
        }
        internal XToken(int kw1, int kw2)
        {
            _code = 0;
            Validate(kw1);
            Validate(kw2);
            _kw1 = (XKeyword) kw1;
            _kw2 = (XKeyword) kw2;
        }

        internal XToken(XKeyword kw1, XKeyword kw2)
        {
            _code = 0;
            _kw1 = kw1;
            _kw2 = kw2;
        }

        public bool Equals(XToken other)
        {
            return _code == other._code;
        }

        public bool Equals(XToken x, XToken y)
        {
            return x.Equals(y);
        }

        public int GetHashCode(XToken obj)
        {
            return _code;
        }

        public override string ToString()
        {
            if (_kw1 == XKeyword.None)
                return _kw2.ToString();
            return _kw1.ToString() + " " + _kw2.ToString();
        }
    }
}
