// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp;

namespace Microsoft.CodeAnalysis
{
    public abstract partial class Compilation
    {
        internal IEnumerable<Cci.IWin32Resource> AdjustWin32VersionAndManifestResources(IEnumerable<Cci.IWin32Resource> resources, DiagnosticBag diagnostics)
        {
            var hasVersion = resources.Where( (r) => r.TypeId == 16).Count() > 0;
            var createManifest = resources.Where((r) => r.TypeId == 24).Count() == 0;
            var opts = (CSharpCompilationOptions)Options;
            var createVersion = !opts.UseNativeVersion || !hasVersion;
            if (createVersion || createManifest)
            {
                var defVersion = CreateDefaultWin32Resources(createVersion, !createManifest, null, null);
                var defList = MakeWin32ResourceList(defVersion, diagnostics);
                if (defList?.Count() > 0)
                {
                    var reslist = new List<Cci.IWin32Resource>();
                    if (createVersion)
                        reslist.AddRange(resources.Where((r) => r.TypeId != 16 )); // RC_RT_VERSION
                    else
                        reslist.AddRange(resources); // RC_RT_VERSION
                    reslist.AddRange(defList);
                    resources = reslist;
                }
            }
            return resources;
        }
    }
}
