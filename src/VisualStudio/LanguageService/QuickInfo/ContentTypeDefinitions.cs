#pragma warning disable 649
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.ComponentModel.Composition;

using Microsoft.VisualStudio.Utilities;

namespace XSharp.LanguageService
{
    internal static class ContentTypeDefinitions
    {
        /// <summary>
        /// Exports the XSharp content type
        /// </summary>
        [Export]
        [Name(XSharpConstants.LanguageName)]
        [BaseDefinition("code")]
        [BaseDefinition("projection")]
        internal static ContentTypeDefinition XSharpContentType;

        /// <summary>
        /// Exports the XSharp file extension mappings
        /// </summary>
        [Export]
        [FileExtension(".prg")]
        [ContentType(XSharpConstants.LanguageName)]
        internal static FileExtensionToContentTypeDefinition PrgFileExtension;

        [Export]
        [FileExtension(".xs")]
        [ContentType(XSharpConstants.LanguageName)]
        internal static FileExtensionToContentTypeDefinition XsFileExtension;

        [Export]
        [FileExtension(".ppo")]
        [ContentType(XSharpConstants.LanguageName)]
        internal static FileExtensionToContentTypeDefinition PpoFileExtension;

        [Export]
        [FileExtension(".vh")]
        [ContentType(XSharpConstants.LanguageName)]
        internal static FileExtensionToContentTypeDefinition VhFileExtension;

        [Export]
        [FileExtension(".xh")]
        [ContentType(XSharpConstants.LanguageName)]
        internal static FileExtensionToContentTypeDefinition XhFileExtension;

        [Export]
        [FileExtension(".ch")]
        [ContentType(XSharpConstants.LanguageName)]
        internal static FileExtensionToContentTypeDefinition ChFileExtension;
    }
}
