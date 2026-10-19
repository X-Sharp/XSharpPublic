//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.ProjectSystem;

using System;

namespace XSharp.Imaging
{
    /// <summary>
    /// Image monikers of XSharp.ProjectSystemCPS.imagemanifest.
    /// </summary>
    public static class XSharpImages
    {
        public static readonly Guid ImageCatalogGuid = new Guid("D9769BF1-DF1F-48B6-9642-949FB27BF46D");

        public static readonly ProjectImageMoniker Project = new ProjectImageMoniker(ImageCatalogGuid, 1);
        public static readonly ProjectImageMoniker Document = new ProjectImageMoniker(ImageCatalogGuid, 2);
        public static readonly ImageMoniker ProjectImg = new ImageMoniker() { Guid = ImageCatalogGuid, Id = 1 };
        public static readonly ImageMoniker DocumentImg = new ImageMoniker() { Guid = ImageCatalogGuid, Id = 2 };

    }
}
