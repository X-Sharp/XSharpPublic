// Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

using System;
using Microsoft.VisualStudio.ProjectSystem;
namespace XSharp.ProjectSystem
{
    /// <summary>
    ///     Provides common well-known C# project property pages.
    /// </summary>
    internal static class XSharpProjectDesignerPage
    {
        public static readonly ProjectDesignerPageMetadata General        = new ProjectDesignerPageMetadata(new Guid(XSharpConstants.GeneralPropertiesPage), pageOrder: 0, hasConfigurationCondition: false);
        public static readonly ProjectDesignerPageMetadata Language       = new ProjectDesignerPageMetadata(new Guid(XSharpConstants.LanguagePropertiesPage), pageOrder: 1, hasConfigurationCondition: false);
        public static readonly ProjectDesignerPageMetadata Dialect        = new ProjectDesignerPageMetadata(new Guid(XSharpConstants.DialectPropertiesPage), pageOrder: 2, hasConfigurationCondition: false);
        public static readonly ProjectDesignerPageMetadata Build          = new ProjectDesignerPageMetadata(new Guid(XSharpConstants.BuildPropertiesPage), pageOrder: 3, hasConfigurationCondition: true);
        public static readonly ProjectDesignerPageMetadata BuildEvents    = new ProjectDesignerPageMetadata(new Guid(XSharpConstants.BuildEventsPropertiesPage), pageOrder: 4, hasConfigurationCondition: true);
        public static readonly ProjectDesignerPageMetadata Debug          = new ProjectDesignerPageMetadata(new Guid(XSharpConstants.DebugPropertiesPage), pageOrder: 5, hasConfigurationCondition: true);
    }
}
