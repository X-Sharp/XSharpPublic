//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System.Collections.Generic;
using System.Linq;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Microsoft.CodeAnalysis
{
    public abstract partial class Compilation
    {

        internal Location AdjustMissingStartLocation(Location location)
        {
            // When there is no Start function and there are no other files with
            // a function, global etc then we point to the first file in the tree
            if (location.SourceTree is CSharpSyntaxTree cstree && cstree.Generated)
            {
                var tree = this.SyntaxTrees.First();
                var root = tree.GetRoot();
                XSharpToken token = null;
                var xnode = root.XNode;
                if (xnode is XSharpParserRuleContext context)
                    token = (XSharpToken)context.Start;
                else if (xnode is XSharpToken tok)
                    token = tok;
                if (token != null)
                {
                    location = new XSharpSourceLocation(token, tree);
                }
            }
            return location;
        }

        internal IEnumerable<Cci.IWin32Resource> AdjustWin32VersionAndManifestResources(IEnumerable<Cci.IWin32Resource> resources, DiagnosticBag diagnostics)
        {
            const int RC_RT_MANIFEST = 24;
            const int RC_RT_VERSION = 16;
            bool createVersion = true;
            bool createManifest = resources.Where((r) => r.TypeId == RC_RT_MANIFEST).Count() == 0;
            var options = (CSharpCompilationOptions)Options;
            if (options.NoWin32Manifest)
            {
                createManifest = false; 
            }
            if (options.UseNativeVersion)
            {
                createVersion = resources.Where((r) => r.TypeId == RC_RT_VERSION).Count() == 0;
            }
            if (createVersion || createManifest)
            {
                var defVersion = CreateDefaultWin32Resources(createVersion, !createManifest, null, null);
                var defList = MakeWin32ResourceList(defVersion, diagnostics);
                if (defList?.Count() > 0)
                {
                    var reslist = new List<Cci.IWin32Resource>();
                    if (createVersion)
                        reslist.AddRange(resources.Where((r) => r.TypeId != RC_RT_VERSION)); // 
                    else
                        reslist.AddRange(resources); 
                    reslist.AddRange(defList);
                    resources = reslist;
                }
            }
            return resources;
        }
    }
}
