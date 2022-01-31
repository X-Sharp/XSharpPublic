﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;
using XSharpModel;

namespace XSharp.LanguageService
{
    internal static class ClassificationTypes
    {

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(ColorizerConstants.XSharpRegionStartFormat)]
        internal static ClassificationTypeDefinition XSharpRegionStartFormat = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(ColorizerConstants.XSharpRegionStopFormat)]
        internal static ClassificationTypeDefinition XSharpRegionStopFormat = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(ColorizerConstants.XSharpKwOpenFormat)]
        internal static ClassificationTypeDefinition XSharpKwOpenFormat = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(ColorizerConstants.XSharpKwCloseFormat)]
        internal static ClassificationTypeDefinition XSharpKwCloseFormat = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(ColorizerConstants.XSharpTextEndTextFormat)]
        internal static ClassificationTypeDefinition XSharpTextEndTextFormat = null;
    }
}
