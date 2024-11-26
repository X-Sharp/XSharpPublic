//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using Microsoft.Build.Framework.XamlTypes;
using Microsoft.CodeAnalysis;
using XSharpModel;
namespace Microsoft.VisualStudio.ProjectSystem.Properties
{
    internal class XSharpStartupObjectsEnumGenerator : IDynamicEnumValuesGenerator
    {
        public bool AllowCustomValues => true;
        private readonly UnconfiguredProject _unconfiguredProject;
        private readonly bool _includeEmptyValue;

        public XSharpStartupObjectsEnumGenerator(Workspace workspace, UnconfiguredProject project, bool includeEmptyValue, bool searchForEntryPointsInFormsOnly)
        {
            _unconfiguredProject = project;
            _includeEmptyValue = includeEmptyValue;
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
