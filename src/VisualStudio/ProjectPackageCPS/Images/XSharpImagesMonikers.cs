//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using Microsoft.VisualStudio.Imaging.Interop;

namespace XSharp.ProjectSystem
{
    public static class XSharpImagesMonikers
    {
        private static readonly Guid ManifestGuid = new Guid("6c1120ab-b5cc-4593-9413-ad506eedcec7");

        private const int ProjectIcon = 1;
        private const int ItemIcon = 2;

        public static ImageMoniker ProjectImage
        {
            get
            {
                return new ImageMoniker { Guid = ManifestGuid, Id = ProjectIcon };
            }
        }

        public static ImageMoniker FileImage
        {
            get
            {
                return new ImageMoniker { Guid = ManifestGuid, Id = ItemIcon };
            }
        }
    }
}
