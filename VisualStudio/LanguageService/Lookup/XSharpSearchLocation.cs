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
    /// <summary>
    /// This type contains the location where a search has started. It is immutable
    /// If you want to change the line number or Position you will have to use the With() method to update these
    /// </summary>
    internal class XSharpSearchLocation
    {
        internal ITextSnapshot Snapshot { get; private set; }
        internal int Position { get; private set; }
        internal int LineNumber { get; private set; }
        internal string CurrentNamespace { get; private set; }
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
        internal XSharpSearchLocation(XSourceMemberSymbol member, ITextSnapshot snapshot,
            int lineNumber = 0, int position = 0, string currentNs= "")
        {
            Member = member;
            Snapshot = snapshot;
            File = Member.File;
            LineNumber = lineNumber;
            Position = position;
            CurrentNamespace = currentNs;
        }

        internal XSharpSearchLocation With( int newLine, int newPos)
        {
            var clone = (XSharpSearchLocation)this.MemberwiseClone();
            clone.LineNumber = newLine;
            clone.Position = newPos;
            return clone;
        }
        internal XSharpSearchLocation With(string currentNs)
        {
            var clone = (XSharpSearchLocation)this.MemberwiseClone();
            clone.CurrentNamespace = currentNs;
            return clone;
        }
        internal XSharpSearchLocation With(XSourceMemberSymbol member)
        {
            var clone = (XSharpSearchLocation)this.MemberwiseClone();
            clone.Member = member;
            return clone;
        }
        internal IXTypeSymbol FindType(string name)
        {
            var usings = File.Usings.ToList();
            if (!string.IsNullOrEmpty(CurrentNamespace))
                usings.Add(CurrentNamespace);
            return Project.FindType(name, usings);
        }
    }
}
