//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Imaging.Interop;
using XSharp.ProjectImages;

namespace XSharp.Project
{
    /// <summary>
    /// Image monikers for the images in the XSharp.ProjectImages project (replaces the old project imagelist).
    /// </summary>
    internal static class XSharpImageMonikers
    {
        internal static ImageMoniker Create(int id)
        {
            return new ImageMoniker { Guid = XSharpImageIds.ImageGuid, Id = id };
        }

        internal static ImageMoniker Project => Create(XSharpImageIds.Project);
        internal static ImageMoniker Source => Create(XSharpImageIds.Source);
        internal static ImageMoniker Form => Create(XSharpImageIds.Form);
        internal static ImageMoniker Server => Create(XSharpImageIds.Server);
        internal static ImageMoniker FieldSpec => Create(XSharpImageIds.FieldSpec);
        internal static ImageMoniker Menu => Create(XSharpImageIds.Menu);
        internal static ImageMoniker VO => Create(XSharpImageIds.VO);
        internal static ImageMoniker SQLTable => Create(XSharpImageIds.SQLTable);
        internal static ImageMoniker Test => Create(XSharpImageIds.Test);
        internal static ImageMoniker Report => Create(XSharpImageIds.Report);
        internal static ImageMoniker Reference => Create(XSharpImageIds.Reference);
        internal static ImageMoniker DanglingReference => Create(XSharpImageIds.DanglingReference);
        internal static ImageMoniker TabOrder => Create(XSharpImageIds.TabOrder);
        internal static ImageMoniker FoxPro => Create(XSharpImageIds.FoxPro);
        internal static ImageMoniker NuGet => Create(XSharpImageIds.NuGet);
        internal static ImageMoniker Header => Create(XSharpImageIds.Header);
        internal static ImageMoniker IncludeFolder => Create(XSharpImageIds.IncludeFolder);
        internal static ImageMoniker ReferenceGroup => Create(XSharpImageIds.ReferenceGroup);
        internal static ImageMoniker MissingFile => Create(XSharpImageIds.MissingFile);
    }
}
