// Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

using Microsoft.Build.Framework.XamlTypes;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.LanguageServices;
using Microsoft.VisualStudio.LanguageServices.Implementation.ProjectSystem;
using XSharpModel;
namespace Microsoft.VisualStudio.ProjectSystem.Properties
{
    internal class XSharpStartupObjectsEnumGenerator : IDynamicEnumValuesGenerator
    {
        public bool AllowCustomValues => true;
        private readonly Workspace _workspace;
        private readonly UnconfiguredProject _unconfiguredProject;
        private readonly bool _includeEmptyValue;
        private readonly bool _searchForEntryPointsInFormsOnly;

        public XSharpStartupObjectsEnumGenerator(Workspace workspace, UnconfiguredProject project, bool includeEmptyValue, bool searchForEntryPointsInFormsOnly)
        {
            _workspace = workspace;
            _unconfiguredProject = project;
            _includeEmptyValue = includeEmptyValue;
            _searchForEntryPointsInFormsOnly = searchForEntryPointsInFormsOnly;
        }

        public async Task<ICollection<IEnumValue>> GetListedValuesAsync()
        {
            var objects = XDatabase.GetStartupClasses(_unconfiguredProject.FullPath);
            await Task.Yield();
            List<IEnumValue> enumValues = new();
            if (_includeEmptyValue)
            {
                enumValues.Add(new PageEnumValue(new EnumValue { Name = string.Empty, DisplayName = VSResources.StartupObjectNotSet }));
            }
            foreach (var obj in objects)
            {
                enumValues.Add(new PageEnumValue(new EnumValue { Name = obj, DisplayName = obj}));
            }
            return enumValues;
        }

        public Task<IEnumValue?> TryCreateEnumValueAsync(string userSuppliedValue)
        {
            var value = new PageEnumValue(new EnumValue { Name = userSuppliedValue, DisplayName = userSuppliedValue });
            return Task.FromResult<IEnumValue?>(value);
        }
    }
}
