//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Windows.Forms;

namespace XSharp.Project
{
    /// <summary>
    /// Common abstraction for property page panel implementations.
    /// Both the WinForms and the XAML/WPF backends implement this interface,
    /// allowing <see cref="Microsoft.VisualStudio.Project.XPropertyPage"/> to work
    /// uniformly with either technology.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Visual Studio hosts property pages through the COM <c>IPropertyPage2</c> interface.
    /// That host is WinForms-based, so every panel — regardless of whether its content is
    /// WinForms or WPF — must ultimately present a <see cref="Control"/> whose
    /// <c>HWND</c> can be re-parented into the VS property sheet frame.
    /// </para>
    /// <para>
    /// WinForms panels satisfy this naturally (they <em>are</em> controls).
    /// WPF panels wrap their content in a <c>System.Windows.Forms.Integration.ElementHost</c>
    /// inside a <see cref="UserControl"/>, so the same contract applies.
    /// </para>
    /// <para>
    /// Call sequence imposed by <see cref="Microsoft.VisualStudio.Project.XPropertyPage"/>:
    /// <list type="number">
    ///   <item><c>HookupEvents()</c> — wire up change-notification handlers</item>
    ///   <item><c>Control.CreateControl()</c> — force HWND creation</item>
    ///   <item><c>BindProperties()</c> — load project values into the UI</item>
    ///   <item><c>ApplyChanges()</c> — (on Apply) save UI values back to the project</item>
    ///   <item><c>Dispose()</c> — (on Deactivate) release resources</item>
    /// </list>
    /// </para>
    /// </remarks>
    public interface IPropertyPagePanel : IDisposable
    {
        // =========================================================================================
        // Properties
        // =========================================================================================

        /// <summary>
        /// Gets the WinForms <see cref="Control"/> that Visual Studio will host inside its
        /// property sheet frame.  This provides the <c>HWND</c>, size, visibility, and
        /// bounds that the frame needs to manage layout.
        /// </summary>
        /// <remarks>
        /// For WinForms panels this is the panel itself (<c>return this</c>).
        /// For XAML panels this is the <c>UserControl</c> that wraps the
        /// <c>ElementHost</c>.
        /// </remarks>
        Control Control { get; }

        /// <summary>
        /// Gets or sets a value indicating whether this panel contains unsaved changes.
        /// </summary>
        /// <remarks>
        /// Setting this to <see langword="true"/> propagates the dirty state to the owning
        /// <see cref="Microsoft.VisualStudio.Project.XPropertyPage"/>, which in turn notifies
        /// the VS property sheet so the <em>Apply</em> button is enabled.
        /// </remarks>
        bool IsDirty { get; set; }

        // =========================================================================================
        // Methods
        // =========================================================================================

        /// <summary>
        /// Wires up all change-notification event handlers on the panel's controls.
        /// Called once by <see cref="Microsoft.VisualStudio.Project.XPropertyPage"/>
        /// immediately after the panel is created, before the panel is shown.
        /// </summary>
        void HookupEvents();

        /// <summary>
        /// Loads all project property values from the MSBuild project and updates
        /// the panel's UI controls to reflect them.
        /// Called after the panel is activated and whenever the project's active
        /// configuration changes.
        /// </summary>
        void BindProperties();

        /// <summary>
        /// Reads all current UI control values and saves them back to the MSBuild project.
        /// Called when the user clicks <em>Apply</em> or <em>OK</em> in the property sheet.
        /// </summary>
        void ApplyChanges();
    }
}
