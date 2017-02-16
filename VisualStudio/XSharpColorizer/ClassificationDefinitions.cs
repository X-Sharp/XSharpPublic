//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace XSharpColorizer
{
    internal static class ClassificationTypes
    {

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(ColorizerConstants.XSharpRegionStartFormat)]
        internal static ClassificationTypeDefinition XSharpRegionStartFormat = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(ColorizerConstants.XSharpRegionStopFormat)]
        internal static ClassificationTypeDefinition XSharpRegionStopFormat = null;


    }
}
