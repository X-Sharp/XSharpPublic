using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LanguageService.CodeAnalysis.XSharp;
using Microsoft.VisualStudio.Text;
using XSharpModel;

namespace XSharp.LanguageService
{
    internal class XSharpSearchLocation
    {
        internal ITextSnapshot Snapshot { get; private set; }
        internal int Position { get; set; }
        internal int LineNumber { get; set; }
        internal string CurrentNamespace { get; set; }
        internal XFile File { get; private set; }
        internal XSourceMemberSymbol Member { get; private set; }
        internal XSharpDialect Dialect
        {
            get
            {
                if (Project != null)
                    return Project.Dialect;
                return XSharpDialect.Core;
            }
        }
        internal XProject Project
        {
            get
            {
                if (File != null)
                    return File.Project;
                return null;
            }
        }
        internal XSharpSearchLocation(XSourceMemberSymbol member, ITextSnapshot snapshot)
        {
            Member = member;
            Snapshot = snapshot;
            File = Member.File;
        }

    }
}
