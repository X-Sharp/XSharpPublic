////
//// Copyright (c) XSharp B.V.  All Rights Reserved.  
//// Licensed under the Apache License, Version 2.0.  
//// See License.txt in the project root for license information.
////
//using System;
//using System.Collections.Generic;
//using System.Linq;
//using System.Text;
//using System.Threading.Tasks;
//using Microsoft.VisualStudio;
//using Microsoft.VisualStudio.Package;
//using Microsoft.VisualStudio.TextManager.Interop;
//using Microsoft.VisualStudio.OLE.Interop;

//namespace XSharp.LanguageService
//{
//    class XSharpSource: Source
//    {
//        public XSharpSource( XSharpLanguageService service,
//                        IVsTextLines textLines,
//                        Colorizer colorizer)
//            : base(service, textLines, colorizer)
//        {

//        }
//        public override void BeginParse()
//        {
//            base.BeginParse();
//        }
//        public override ParseRequest BeginParse(int line, int idx, TokenInfo info, ParseReason reason, IVsTextView view, ParseResultHandler callback)
//        {
//            return base.BeginParse(line, idx, info, reason, view, callback);
//        }
//    }
//}
