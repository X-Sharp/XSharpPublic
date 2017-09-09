#if SMARTINDENTMEF
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel.Composition;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Text.Tagging;

namespace XSharp.Project
{

    [Export(typeof(ISmartIndentProvider))]
    [Name("Smart Indent")]
    [Order(Before = "default")]
    [ContentType("XSharp")]
    internal class SmartIndentProvider : ISmartIndentProvider
    {
        [Import]
        IBufferTagAggregatorFactoryService aggregator = null;

        public ISmartIndent CreateSmartIndent(ITextView textView)
        {
            return new XSharpSmartIndent(textView, aggregator);
        }

    }
}

#endif