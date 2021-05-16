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
        internal List<string> Usings { get; private set; }
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
            if (member != null)
            {
                File = Member.File;
            }
            LineNumber = lineNumber;
            Position = position;
            CurrentNamespace = currentNs;
            Usings = GetUsings();
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
            clone.Usings = clone.GetUsings();
            return clone;
        }
        internal XSharpSearchLocation With(XSourceMemberSymbol member)
        {
            var clone = (XSharpSearchLocation)this.MemberwiseClone();
            clone.Member = member;
            clone.Usings = clone.GetUsings();
            return clone;
        }
        internal IXTypeSymbol FindType(string name)
        {
            var usings = Usings.ToList();
             return Project.FindType(name, usings);
        }

        private List<string> GetUsings()
        {
            IXSymbol scope;
            scope = Member;
            var scopes = new List<string>();
            scopes.AddRange(File.Usings);
            while (scope != null)
            {
                string ns = "";
                if (scope is XSourceTypeSymbol && !XSourceTypeSymbol.IsGlobalType(scope))
                {
                    ns = scope.FullName;
                }
                if (scope.Kind == Kind.Namespace)
                {
                    ns = scope.FullName;
                }
                if (ns?.Length > 0)
                {
                    var elements = ns.Split(".".ToCharArray());
                    ns = "";
                    for (int i = 0; i < elements.Length; i++)
                    {
                        if (i > 0)
                            ns += "." + elements[i];
                        else
                            ns = elements[0];
                        if (!scopes.Contains(ns))
                        {
                            scopes.Add(ns);
                        }
                    }
                }
                scope = scope.Parent;
            }
            if (!string.IsNullOrEmpty(CurrentNamespace) && ! scopes.Contains(CurrentNamespace))
                scopes.Add(CurrentNamespace);

            return scopes;
        }

    }
}
