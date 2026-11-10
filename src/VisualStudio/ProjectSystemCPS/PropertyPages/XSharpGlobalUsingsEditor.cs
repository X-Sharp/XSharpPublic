//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.Immutable;
using System.ComponentModel.Composition;

using Microsoft.VisualStudio.ProjectSystem.VS.PropertyPages.Designer;

namespace XSharp.ProjectSystem.Properties
{
    /// <summary>
    /// Renders the "Manage Global Usings" button in the property page.
    /// </summary>
    [Export(typeof(IPropertyEditor))]
    [ExportMetadata("Name", EditorName)]
    internal sealed class XSharpGlobalUsingsEditor : XPropertyEditorBase
    {
        internal const string EditorName = "XSharpGlobalUsingsEditor";
#pragma warning disable VSTHRD010 // Invoke single-threaded types on Main thread
        public XSharpGlobalUsingsEditor()
            : base("UnconfiguredXSharpGlobalUsingsTemplate", "ConfiguredXSharpGlobalUsingsTemplate")
        {
        }

        /// <summary>The editor owns no MSBuild property; it edits the Using items directly.</summary>
        public override bool IsPseudoProperty => true;

        public override object DefaultValue => null;

        public override bool IsChangedByEvaluation(
            string unevaluatedValue, object evaluatedValue,
            ImmutableDictionary<string, string> editorMetadata)
        {
            return false;
        }
    }
}
