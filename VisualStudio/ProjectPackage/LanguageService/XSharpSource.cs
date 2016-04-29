using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.OLE.Interop;

namespace XSharp.LanguageService
{
    class XSharpSource: Source
    {
        public XSharpSource( XSharpLanguageService service,
                        IVsTextLines textLines,
                        Colorizer colorizer)
            : base(service, textLines, colorizer)
        {

        }
    }
}
