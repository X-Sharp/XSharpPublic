//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Shell;
using XSharpModel;

namespace XSharp.LanguageService.RoslynIntegration
{
    internal interface IXSharpEditorDebugPipeline
    {
        Task InitializeAsync(AsyncPackage package, CancellationToken cancellationToken);
        void OnBeforeDebugLaunch(string projectFileName);
        void OnProjectParseOptionsChanged(string projectFileName, XParseOptions parseOptions);
        void Terminate();
    }
}
