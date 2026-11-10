//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using Microsoft.VisualStudio.Shell;

namespace XSharp.ProjectSystem.Selection
{
    /// <summary>
    /// Registers a project selector for a project type:
    /// <code>
    /// [$RootKey$\Projects\{projectType}]      "ProjectSelector" = {selector}
    /// [$RootKey$\ProjectSelectors\{selector}] "Package"         = {package that registers the selector}
    /// </code>
    /// VS loads the package when it needs the selector; the package registers the selector object with
    /// IVsRegisterProjectSelector during its initialization.
    /// </summary>
    [AttributeUsage(AttributeTargets.Class, AllowMultiple = true, Inherited = true)]
    internal sealed class ProvideProjectSelectorAttribute : RegistrationAttribute
    {
        private readonly Guid projectTypeGuid;
        private readonly Guid selectorGuid;

        public ProvideProjectSelectorAttribute(string projectTypeGuid, string selectorGuid)
        {
            this.projectTypeGuid = new Guid(projectTypeGuid);
            this.selectorGuid = new Guid(selectorGuid);
        }

        private string ProjectTypeKey => "Projects\\" + projectTypeGuid.ToString("B");
        private string SelectorKey => "ProjectSelectors\\" + selectorGuid.ToString("B");

        public override void Register(RegistrationContext context)
        {
            var projectKey = context.CreateKey(ProjectTypeKey);
            try
            {
                projectKey.SetValue("ProjectSelector", selectorGuid.ToString("B"));
            }
            finally
            {
                projectKey.Close();
            }
            var selectorKey = context.CreateKey(SelectorKey);
            try
            {
                selectorKey.SetValue("Package", context.ComponentType.GUID.ToString("B"));
            }
            finally
            {
                selectorKey.Close();
            }
        }

        public override void Unregister(RegistrationContext context)
        {
            context.RemoveValue(ProjectTypeKey, "ProjectSelector");
            context.RemoveKey(SelectorKey);
        }
    }
}
