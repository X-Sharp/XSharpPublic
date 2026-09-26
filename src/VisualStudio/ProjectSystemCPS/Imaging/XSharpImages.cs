//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using Microsoft.VisualStudio.ProjectSystem;

namespace XSharp.ProjectSystem.Imaging
{
    /// <summary>
    /// Image monikers of XSharp.ProjectSystemCPS.imagemanifest.
    /// </summary>
    internal static class XSharpImages
    {
        public static readonly Guid ImageCatalogGuid = new Guid("D9769BF1-DF1F-48B6-9642-949FB27BF46D");

        public static readonly ProjectImageMoniker Project = new ProjectImageMoniker(ImageCatalogGuid, 1);
        public static readonly ProjectImageMoniker Document = new ProjectImageMoniker(ImageCatalogGuid, 2);
    }
}
