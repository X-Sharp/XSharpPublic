//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Immutable;
using System.Threading;
using System.Windows;

using Microsoft.VisualStudio.ProjectSystem.VS.PropertyPages.Designer;
using Microsoft.VisualStudio.Shell;

namespace XSharp.ProjectSystem.Properties
{
    /// <summary>
    /// Base class for X# property editors, modelled on the project system's own
    /// <c>PropertyEditorBase</c>, which is internal and therefore not usable here.
    /// </summary>
    /// <remarks>
    /// Templates are resolved lazily and on the UI thread only, matching the behaviour
    /// expected by the property page designer.
    /// </remarks>
    internal abstract class XPropertyEditorBase : IPropertyEditor
    {
        private static readonly Lazy<ResourceDictionary> LazyResources =
            new Lazy<ResourceDictionary>(() =>
            {
                if (!UriParser.IsKnownScheme("pack"))
                {
                    UriParser.Register(
                        new GenericUriParser(GenericUriParserOptions.GenericAuthority), "pack", -1);
                }
                return new ResourceDictionary
                {
                    Source = new Uri(
                        "pack://application:,,,/XSharp.ProjectSystemCPS;component/Properties/Resources/PropertyTemplates.xaml",
                        UriKind.RelativeOrAbsolute)
                };
            }, LazyThreadSafetyMode.ExecutionAndPublication);

        private readonly Lazy<DataTemplate> _lazyPropertyDataTemplate;
        private readonly Lazy<DataTemplate> _lazyUnconfiguredDataTemplate;
        private readonly Lazy<DataTemplate> _lazyConfiguredDataTemplate;

        protected XPropertyEditorBase(
            string unconfiguredDataTemplateName,
            string configuredDataTemplateName,
            string propertyDataTemplateName = null)
        {
#pragma warning disable VSTHRD010 // Invoke single-threaded types on Main thread
            _lazyPropertyDataTemplate = CreateLazyTemplate(propertyDataTemplateName);
            _lazyUnconfiguredDataTemplate = CreateLazyTemplate(unconfiguredDataTemplateName);
            _lazyConfiguredDataTemplate = CreateLazyTemplate(configuredDataTemplateName);
#pragma warning restore VSTHRD010 // Invoke single-threaded types on Main thread
        }

        public DataTemplate PropertyDataTemplate
        {
            get
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                return _lazyPropertyDataTemplate?.Value;
            }
        }

        public DataTemplate UnconfiguredDataTemplate
        {
            get
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                return _lazyUnconfiguredDataTemplate?.Value;
            }
        }

        public DataTemplate ConfiguredDataTemplate
        {
            get
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                return _lazyConfiguredDataTemplate?.Value;
            }
        }

        public virtual bool ShowUnevaluatedValue => false;

        public virtual bool IsPseudoProperty => false;

        public abstract object DefaultValue { get; }

        public virtual bool ShouldShowDescription(int valueCount) => true;

        public abstract bool IsChangedByEvaluation(
            string unevaluatedValue, object evaluatedValue,
            ImmutableDictionary<string, string> editorMetadata);

        private static Lazy<DataTemplate> CreateLazyTemplate(string templateName)
        {
            if (templateName == null)
                return null;

            return new Lazy<DataTemplate>(() =>
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                return (DataTemplate)LazyResources.Value[templateName];
            });
        }
    }
}
