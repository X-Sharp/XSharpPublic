//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.Build.Execution;
using Microsoft.Build.Framework;
using Microsoft.Build.Framework.XamlTypes;
using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.Properties;
using Microsoft.VisualStudio.Threading;

namespace XSharp.ProjectSystem.PropertyPages
{
    /// <summary>
    /// Persistence "XSharpDefineConstants" of the "Conditional compilation symbols" field (DefineConstants in
    /// BuildPropertyPage.XSharp.xaml): the project file, with the value converted like C# does.
    /// </summary>
    /// <remarks>
    /// The managed Build page edits DefineConstants with a MultiStringSelector, whose value is a list of name/value
    /// pairs ("MYSYM=False"). For C# the managed DefineConstantsValueProvider (persistence
    /// "ProjectFileWithInterception", AppliesTo CSharp | FSharp) converts it: the list shows only the symbols of the
    /// project file after "$(DefineConstants);", and a change is written as "$(DefineConstants);MYSYM", so the
    /// implicit symbols (DEBUG, TRACE) are kept. For X# nothing converted it: VS wrote "MYSYM=False" into the project
    /// file and DEBUG and TRACE were lost (verified in VS). That value provider cannot be used for X#: its interfaces
    /// are in Microsoft.VisualStudio.ProjectSystem.Managed.dll, which has no package and a version per VS release.
    /// So this provider does the same with the public CPS API: it wraps the project file provider and converts only
    /// DefineConstants.
    /// </remarks>
    // The property pages import the unnamed contracts and select the provider by the "Name" metadata
    // (PropertyPagesDataModelProvider); the named exports are for imports by persistence name.
    [Export(PersistenceName, typeof(IProjectPropertiesProvider))]
    [Export(typeof(IProjectPropertiesProvider))]
    [Export(PersistenceName, typeof(IProjectInstancePropertiesProvider))]
    [Export(typeof(IProjectInstancePropertiesProvider))]
    [ExportMetadata("Name", PersistenceName)]
    [AppliesTo(XSharpCapabilities.XSharpCps)]
    internal sealed class XSharpDefineConstantsPropertiesProvider : IProjectPropertiesProvider, IProjectInstancePropertiesProvider
    {
        public const string PersistenceName = "XSharpDefineConstants";
        private const string ProjectFileContract = "Microsoft.VisualStudio.ProjectSystem.ProjectFile";

        private readonly IProjectPropertiesProvider provider;
        private readonly IProjectInstancePropertiesProvider instanceProvider;
        private readonly UnconfiguredProject project;

        [ImportingConstructor]
        public XSharpDefineConstantsPropertiesProvider(
            [Import(ProjectFileContract)] IProjectPropertiesProvider provider,
            [Import(ProjectFileContract)] IProjectInstancePropertiesProvider instanceProvider,
            UnconfiguredProject project)
        {
            this.provider = provider;
            this.instanceProvider = instanceProvider;
            this.project = project;
        }

        public string DefaultProjectPath => project.FullPath;

        public event AsyncEventHandler<ProjectPropertyChangedEventArgs> ProjectPropertyChanged
        {
            add => provider.ProjectPropertyChanged += value;
            remove => provider.ProjectPropertyChanged -= value;
        }

        public event AsyncEventHandler<ProjectPropertyChangedEventArgs> ProjectPropertyChangedOnWriter
        {
            add => provider.ProjectPropertyChangedOnWriter += value;
            remove => provider.ProjectPropertyChangedOnWriter -= value;
        }

        public event AsyncEventHandler<ProjectPropertyChangedEventArgs> ProjectPropertyChanging
        {
            add => provider.ProjectPropertyChanging += value;
            remove => provider.ProjectPropertyChanging -= value;
        }

        // The property pages read and write through GetProperties; the rest is passed on unchanged
        public IProjectProperties GetProperties(string file, string itemType, string item) =>
            new DefineConstantsProperties(provider.GetProperties(file, itemType, item));

        public IProjectProperties GetCommonProperties() => provider.GetCommonProperties();
        public IProjectProperties GetItemProperties(string itemType, string item) => provider.GetItemProperties(itemType, item);
        public IProjectProperties GetItemTypeProperties(string itemType) => provider.GetItemTypeProperties(itemType);
        public IProjectProperties GetCommonProperties(ProjectInstance projectInstance) => instanceProvider.GetCommonProperties(projectInstance);
        public IProjectProperties GetItemTypeProperties(ProjectInstance projectInstance, string itemType) => instanceProvider.GetItemTypeProperties(projectInstance, itemType);
        public IProjectProperties GetItemProperties(ProjectInstance projectInstance, string itemType, string itemName) => instanceProvider.GetItemProperties(projectInstance, itemType, itemName);
        public IProjectProperties GetItemProperties(ITaskItem taskItem) => instanceProvider.GetItemProperties(taskItem);

        /// <summary>
        /// The project file properties; DefineConstants is converted between the MultiStringSelector list and
        /// "$(DefineConstants);SYMBOL;..." (DefineConstantsValueProvider of the managed project system).
        /// </summary>
        private sealed class DefineConstantsProperties : IProjectProperties, IRuleAwareProjectProperties
        {
            private const string DefineConstants = nameof(DefineConstants);
            private const string RecursivePrefix = "$(DefineConstants)";

            private readonly IProjectProperties properties;

            public DefineConstantsProperties(IProjectProperties properties)
            {
                this.properties = properties;
            }

            private static bool IsDefineConstants(string propertyName) =>
                string.Equals(propertyName, DefineConstants, StringComparison.OrdinalIgnoreCase);

            public IProjectPropertiesContext Context => properties.Context;
            public string FileFullPath => properties.FileFullPath;
            public PropertyKind PropertyKind => properties.PropertyKind;

            public Task DeleteDirectPropertiesAsync() => properties.DeleteDirectPropertiesAsync();
            public Task DeletePropertyAsync(string propertyName, IReadOnlyDictionary<string, string> dimensionalConditions = null) =>
                properties.DeletePropertyAsync(propertyName, dimensionalConditions);
            public Task<IEnumerable<string>> GetDirectPropertyNamesAsync() => properties.GetDirectPropertyNamesAsync();
            public Task<IEnumerable<string>> GetPropertyNamesAsync() => properties.GetPropertyNamesAsync();
            public Task<string> GetEvaluatedPropertyValueAsync(string propertyName) => properties.GetEvaluatedPropertyValueAsync(propertyName);
            public Task<bool> IsValueInheritedAsync(string propertyName) => properties.IsValueInheritedAsync(propertyName);

            public async Task<string> GetUnevaluatedPropertyValueAsync(string propertyName)
            {
                var value = await properties.GetUnevaluatedPropertyValueAsync(propertyName);
                if (!IsDefineConstants(propertyName))
                    return value;
                // Only the symbols of the project file itself; the imported ones (DEBUG, TRACE) are shown as the
                // evaluated preview
                if (value == null || await properties.IsValueInheritedAsync(propertyName))
                    return string.Empty;
                return KeyValuePairListEncoding.Format(ParseOwnSymbols(value).Select(symbol => (symbol, bool.FalseString)));
            }

            public async Task SetPropertyValueAsync(string propertyName, string unevaluatedPropertyValue, IReadOnlyDictionary<string, string> dimensionalConditions = null)
            {
                if (!IsDefineConstants(propertyName))
                {
                    await properties.SetPropertyValueAsync(propertyName, unevaluatedPropertyValue, dimensionalConditions);
                    return;
                }
                // Exactly the symbols of the list, after the inherited ones. (The managed provider also leaves out
                // symbols of an outer "$(DefineConstants);..." definition; a symbol defined twice does no harm.)
                var symbols = KeyValuePairListEncoding.Parse(unevaluatedPropertyValue)
                    .Select(pair => pair.Name.Trim())
                    .Where(symbol => symbol.Length > 0)
                    .Distinct(StringComparer.Ordinal)
                    .ToList();
                if (symbols.Count == 0)
                {
                    await properties.DeletePropertyAsync(propertyName, dimensionalConditions);
                    return;
                }
                await properties.SetPropertyValueAsync(propertyName, RecursivePrefix + ";" + string.Join(";", symbols), dimensionalConditions);
            }

            /// <summary>
            /// The symbols after "$(DefineConstants);". A value without that prefix (written by hand or by MPFproj)
            /// is taken as a whole, so its symbols are shown and kept.
            /// </summary>
            private static IEnumerable<string> ParseOwnSymbols(string value)
            {
                value = value?.Trim() ?? string.Empty;
                if (value.StartsWith(RecursivePrefix, StringComparison.OrdinalIgnoreCase))
                    value = value.Substring(RecursivePrefix.Length);
                return value.Split(new[] { ';', ',' }, StringSplitOptions.RemoveEmptyEntries)
                    .Select(symbol => symbol.Trim())
                    .Where(symbol => symbol.Length > 0);
            }

            public void SetRuleContext(Rule rule)
            {
                (properties as IRuleAwareProjectProperties)?.SetRuleContext(rule);
            }
        }
    }
}
