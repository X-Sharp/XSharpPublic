////
//// Copyright (c) XSharp B.V.  All Rights Reserved.  
//// Licensed under the Apache License, Version 2.0.  
//// See License.txt in the project root for license information.
////
//using Microsoft.VisualStudio.Package;
//using Microsoft.VisualStudio.TextManager.Interop;
//using System;
//using System.Collections.Generic;
//using System.Linq;
//using System.Text;
//using System.Threading.Tasks;

//namespace XSharp.LanguageService
//{
//    internal class XSharpScanner : IScanner
//    {
//        private IVsTextBuffer m_buffer;
//        string m_source;
        

//        public XSharpScanner(IVsTextBuffer buffer)
//        {
//            m_buffer = buffer;
//        }

//        bool IScanner.ScanTokenAndProvideInfoAboutIt(TokenInfo tokenInfo, ref int state)
//        {
//            // The caller will SetSource() to set the line that is to be parsed. 
//            // Then the ScanTokenAndProvideInfoAboutIt method is typically called repeatedly until all tokens are obtained. 
//            // The state value is cached line-by-line by the buffer wich keep a copy of it
//            // That's used to keep the current line-state
//            tokenInfo.Type = TokenType.Unknown;
//            tokenInfo.Color = TokenColor.Text;
//            // True -> Again; False -> Stop
//            return false;
//        }

//        void IScanner.SetSource(string source, int offset)
//        {
//            m_source = source.Substring(offset);
//        }
//    }
//}
