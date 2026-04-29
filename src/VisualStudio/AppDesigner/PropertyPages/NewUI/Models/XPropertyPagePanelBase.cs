//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.IO;
using XSharp.Settings;
using XSharpModel;

namespace XSharp.Project
{
    /// <summary>
    /// Abstract base class that provides the shared business logic used by both
    /// WinForms and XAML/WPF property page panel implementations.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This class is a pure Plain-Old-Object (POJO) — it does not inherit from
    /// <see cref="System.Windows.Forms.Control"/> or any WPF type, so it can safely be
    /// used in both the WinForms hierarchy (<see cref="XPropertyPagePanel"/> subclasses)
    /// and the XAML hierarchy (<see cref="XPropertyPageXamlHost"/> subclasses via the
    /// ViewModel layer).
    /// </para>
    /// <para>
    /// Concrete panel classes inherit either:
    /// <list type="bullet">
    ///   <item>
    ///     <see cref="XPropertyPagePanel"/> (WinForms path) — which adds the UI control
    ///     infrastructure and also implements <see cref="IPropertyPagePanel"/>.
    ///   </item>
    ///   <item>
    ///     <see cref="XPropertyPageViewModel"/> (XAML path) — which adds
    ///     <see cref="System.ComponentModel.INotifyPropertyChanged"/> for WPF data binding.
    ///   </item>
    /// </list>
    /// </para>
    /// </remarks>
    public abstract class XPropertyPagePanelBase
    {
        // =========================================================================================
        // Member Variables
        // =========================================================================================

        /// <summary>The parent property page that owns this panel.</summary>
        protected XPropertyPage parentPropertyPage;

        /// <summary>Whether the panel has unsaved changes.</summary>
        public bool isDirty;

        /// <summary>Converter for the XSharp Dialect enumeration.</summary>
        protected DialectConverter dialectConverter;

        /// <summary>Converter for the project OutputType enumeration.</summary>
        protected OutputTypeConverter outputTypeConverter;

        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XPropertyPagePanelBase"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">
        /// The <see cref="XPropertyPage"/> that owns this panel.
        /// May be <see langword="null"/> during designer-time initialization.
        /// </param>
        protected XPropertyPagePanelBase(XPropertyPage parentPropertyPage)
        {
            this.parentPropertyPage = parentPropertyPage;
            this.isDirty = false;
            this.dialectConverter = new DialectConverter();
            this.outputTypeConverter = new OutputTypeConverter();
        }

        // =========================================================================================
        // Properties
        // =========================================================================================

        /// <summary>
        /// Gets a value indicating whether the current project is an SDK-style project
        /// (i.e., the MSBuild project file has a top-level <c>Sdk</c> attribute such as
        /// <c>&lt;Project Sdk="XSharp.Net/..."&gt;</c>).
        /// </summary>
        /// <value>
        /// <see langword="true"/> for SDK/Core projects; <see langword="false"/> for
        /// traditional .NET Framework projects.
        /// </value>
        protected bool IsSdkProject
            => !string.IsNullOrEmpty(parentPropertyPage?.ProjectMgr?.BuildProject.Xml.Sdk);

        /// <summary>
        /// Gets a value indicating whether the current project targets more than one
        /// target framework simultaneously (multi-targeting).
        /// </summary>
        /// <remarks>
        /// Multi-targeting is signalled by the presence of either the
        /// <c>TargetFrameworks</c> or the XSharp-specific <c>XTargetFrameworks</c>
        /// MSBuild property.
        /// </remarks>
        protected bool IsMultiTargetingProject
            => parentPropertyPage?.ProjectMgr?.GetProjectProperty(XSharpProjectFileConstants.TargetFrameworks) != null
            || parentPropertyPage?.ProjectMgr?.GetProjectProperty(XSharpProjectFileConstants.XTargetFrameworks) != null;

        // =========================================================================================
        // Protected Methods — Project property access
        // =========================================================================================

        /// <summary>
        /// Gets a raw string property value from the MSBuild project.
        /// </summary>
        /// <param name="propertyName">The MSBuild property name (e.g., "AssemblyName").</param>
        /// <returns>
        /// The property value string, or <see langword="null"/> if the project manager
        /// is not yet available.
        /// </returns>
        protected string GetProjectProperty(string propertyName)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            return parentPropertyPage?.ProjectMgr?.GetProjectProperty(propertyName);
        }

        /// <summary>
        /// Sets a raw string property value in the MSBuild project and marks the
        /// panel dirty.
        /// </summary>
        /// <param name="propertyName">The MSBuild property name.</param>
        /// <param name="value">The value to write.</param>
        protected void SetProjectProperty(string propertyName, string value)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (parentPropertyPage?.ProjectMgr == null)
                return;

            parentPropertyPage.ProjectMgr.SetProjectProperty(propertyName, value);
            NotifyDirty();
        }

        /// <summary>
        /// Gets the full target-framework moniker for the current project
        /// (e.g., <c>.NETFramework,Version=v4.8</c> or <c>.NETCoreApp,Version=v6.0</c>).
        /// </summary>
        /// <returns>
        /// The moniker string, or <see langword="null"/> if not available.
        /// </returns>
        protected string GetTargetFrameworkMoniker()
            => parentPropertyPage?.ProjectMgr?.TargetFrameworkMoniker.FullName;

        // =========================================================================================
        // Protected Methods — Dirty flag
        // =========================================================================================

        /// <summary>
        /// Marks the panel as having unsaved changes and propagates the dirty state to
        /// the owning <see cref="XPropertyPage"/> so that the VS property sheet enables
        /// the <em>Apply</em> button.
        /// </summary>
        protected void NotifyDirty()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            isDirty = true;
            if (parentPropertyPage != null)
                parentPropertyPage.IsDirty = true;
        }

        // =========================================================================================
        // Protected Methods — Converters
        // =========================================================================================

        /// <summary>
        /// Converts a raw MSBuild dialect value to its display name using the
        /// <see cref="DialectConverter"/>.
        /// </summary>
        /// <param name="rawValue">The raw string stored in the MSBuild property.</param>
        /// <returns>The localized display name for the dialect.</returns>
        protected string ConvertDialectToDisplay(string rawValue)
        {
            try
            {
                var dialect = (Dialect)dialectConverter.ConvertFrom(rawValue);
                return (string)dialectConverter.ConvertTo(dialect, typeof(string));
            }
            catch
            {
                return rawValue;
            }
        }

        /// <summary>
        /// Converts a display-name dialect string back to its raw MSBuild value using the
        /// <see cref="DialectConverter"/>.
        /// </summary>
        /// <param name="displayValue">The display name shown in the UI.</param>
        /// <returns>The raw value suitable for writing to the MSBuild property.</returns>
        protected string ConvertDialectToRaw(string displayValue)
        {
            try
            {
                var dialect = (Dialect)dialectConverter.ConvertFrom(displayValue);
                return dialect.ToString();
            }
            catch
            {
                return displayValue;
            }
        }

        /// <summary>
        /// Converts a raw MSBuild output-type value to its display name using the
        /// <see cref="OutputTypeConverter"/>.
        /// </summary>
        /// <param name="rawValue">The raw string stored in the MSBuild property.</param>
        /// <returns>The localized display name for the output type.</returns>
        protected string ConvertOutputTypeToDisplay(string rawValue)
        {
            try
            {
                var outputType = (OutputType)outputTypeConverter.ConvertFrom(rawValue);
                return (string)outputTypeConverter.ConvertTo(outputType, typeof(string));
            }
            catch
            {
                return rawValue;
            }
        }

        /// <summary>
        /// Converts a display-name output-type string back to its raw MSBuild value using
        /// the <see cref="OutputTypeConverter"/>.
        /// </summary>
        /// <param name="displayValue">The display name shown in the UI.</param>
        /// <returns>The raw value suitable for writing to the MSBuild property.</returns>
        protected string ConvertOutputTypeToRaw(string displayValue)
        {
            try
            {
                var outputType = (OutputType)outputTypeConverter.ConvertFrom(displayValue);
                return outputType.ToString();
            }
            catch
            {
                return displayValue;
            }
        }

        // =========================================================================================
        // Protected Methods — Database / project queries
        // =========================================================================================

        /// <summary>
        /// Retrieves the list of startup-eligible class names from the XSharp code model
        /// database for the current project.
        /// </summary>
        /// <returns>
        /// A list of fully-qualified class names that contain a <c>Start</c> procedure,
        /// or an empty list if the project file is not available.
        /// </returns>
        protected List<string> GetStartupClasses()
        {
            var projectFile = parentPropertyPage?.ProjectMgr?.ProjectFile;
            if (string.IsNullOrEmpty(projectFile))
                return new List<string>();

            return XDatabase.GetStartupClasses(Path.GetFileName(projectFile));
        }

        /// <summary>
        /// Determines whether the current project contains any RC (Win32 resource) files.
        /// When RC files are present the application-icon field should be disabled because
        /// the icon is controlled by the resource script.
        /// </summary>
        /// <returns>
        /// <see langword="true"/> if the project has RC files; otherwise
        /// <see langword="false"/>.
        /// </returns>
        protected bool HasRCFiles()
        {
            var projectFile = parentPropertyPage?.ProjectMgr?.ProjectFile;
            if (string.IsNullOrEmpty(projectFile))
                return false;

            return XDatabase.HasRCFiles(projectFile);
        }

        /// <summary>
        /// Asks VS source-control whether the project file may be edited.
        /// </summary>
        /// <returns>
        /// <see langword="true"/> if editing is allowed (file is checked out or not
        /// under source control); <see langword="false"/> if the file is read-only or
        /// the check-out was refused.
        /// </returns>
        protected bool CanEditProjectFile()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            return parentPropertyPage?.ProjectMgr?.QueryEditProjectFile(false) ?? false;
        }

        /// <summary>
        /// Normalizes a property value (e.g., trims leading/trailing whitespace) by
        /// delegating to the parent page's <see cref="XPropertyPage.Normalize"/> method.
        /// </summary>
        /// <param name="propertyName">The name of the MSBuild property being normalized.</param>
        /// <param name="value">The raw value entered by the user.</param>
        /// <returns>The normalized value.</returns>
        protected string Normalize(string propertyName, string value)
            => parentPropertyPage?.Normalize(propertyName, value) ?? value;

        // =========================================================================================
        // Protected Methods — Reset-to-default support
        // =========================================================================================

        /// <summary>
        /// Returns <see langword="true"/> when the MSBuild property <paramref name="propertyName"/>
        /// is explicitly set in the project file itself (i.e., not inherited from an imported
        /// <c>.props</c> / <c>.targets</c> file and not absent).
        /// </summary>
        /// <remarks>
        /// This is used to drive the enabled/disabled state of the reset (↺) button next to
        /// each setting: the button is only enabled when the user has an explicit override in
        /// the project file.
        /// </remarks>
        /// <param name="propertyName">The MSBuild property name (e.g., <c>"AssemblyName"</c>).</param>
        /// <returns>
        /// <see langword="true"/> if the property exists in the project file and is not imported;
        /// <see langword="false"/> if it is absent or comes from an imported file.
        /// </returns>
        protected bool IsPropertyOverridden(string propertyName)
        {
            var project = parentPropertyPage?.ProjectMgr?.BuildProject;
            if (project == null)
                return false;
            // Check XML API (covers properties written via Xml.PropertyGroups path)
            foreach (var propGroup in project.Xml.PropertyGroups)
            {
                foreach (var prop in propGroup.Properties)
                {
                    if (string.Equals(prop.Name, propertyName, System.StringComparison.OrdinalIgnoreCase)
                        && string.IsNullOrEmpty(prop.Condition))
                        return true;
                }
            }
            // Check evaluation API (covers properties written via buildProject.SetProperty)
            var evalProp = project.GetProperty(propertyName);
            if (evalProp != null && !evalProp.IsImported)
                return true;
            return false;
        }

        /// <summary>
        /// Removes the MSBuild property <paramref name="propertyName"/> from the project file
        /// so that the SDK-supplied default value takes effect, then reloads all property
        /// values into the ViewModel by calling <see cref="XPropertyPagePanel.BindProperties"/>.
        /// </summary>
        /// <remarks>
        /// After removal the project is marked dirty so VS knows there are unsaved changes.
        /// </remarks>
        /// <param name="propertyName">The MSBuild property name to remove.</param>
        protected void ResetProperty(string propertyName)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var project = parentPropertyPage?.ProjectMgr?.BuildProject;
            if (project == null)
                return;

            // Remove via the evaluation API first (before XML mutation invalidates the object).
            // This clears the in-memory value written by buildProject.SetProperty.
            var evalProp = project.GetProperty(propertyName);
            if (evalProp != null && !evalProp.IsImported)
                project.RemoveProperty(evalProp);

            // Also remove any entries in the project XML directly.
            var groupsToDelete = new System.Collections.Generic.List<Microsoft.Build.Construction.ProjectPropertyGroupElement>();
            foreach (var propGroup in project.Xml.PropertyGroups)
            {
                var propsToDelete = new System.Collections.Generic.List<Microsoft.Build.Construction.ProjectPropertyElement>();
                foreach (var prop in propGroup.Properties)
                {
                    if (string.Equals(prop.Name, propertyName, System.StringComparison.OrdinalIgnoreCase)
                        && string.IsNullOrEmpty(prop.Condition))
                        propsToDelete.Add(prop);
                }
                foreach (var prop in propsToDelete)
                    propGroup.RemoveChild(prop);
                if (propGroup.Count == 0 && string.IsNullOrEmpty(propGroup.Condition))
                    groupsToDelete.Add(propGroup);
            }
            foreach (var g in groupsToDelete)
                project.Xml.RemoveChild(g);
        }
    }
}
