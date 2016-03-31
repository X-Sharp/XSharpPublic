using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Runtime.Versioning;
using System.Windows.Forms;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Project;
using EnvDTE;
using EnvDTE80;
using System.ComponentModel;


namespace XSharp.Project
{
    /// <summary>
    /// This class implements general property page for the project type.
    /// </summary>
    [ComVisible(true)]
    [Guid("E994C210-9D6D-4CF4-A061-EBBEA2BC626B")]
    [ClassInterface(ClassInterfaceType.AutoDual)]
    public class XSharpBuildPropertyPage : XSharpSettingsPage
    {
        #region Fields
        private bool warningAsErrors;
        #endregion Fields

        #region Constructors
        /// <summary>
        /// Explicitly defined default constructor.
        /// </summary>
        public XSharpBuildPropertyPage()
        {
            this.Name = Resources.GetString(Resources.BuildCaption);
        }
        #endregion

        [ResourcesCategoryAttribute(Resources.WarningCaption)]
        [LocDisplayName(Resources.WarningCaptionExpanded)]
        [ResourcesDescriptionAttribute(Resources.WarningDescription)]
        /// <summary>
        /// Gets or sets Assembly Name.
        /// </summary>
        /// <remarks>IsDirty flag was switched to true.</remarks>
        public bool WarningAsErrors
        {
            get { return this.warningAsErrors; }
            set { this.warningAsErrors = value; this.IsDirty = true; }
        }

        #region Overriden Implementation
        /// <summary>
        /// Returns class FullName property value.
        /// </summary>
        public override string GetClassName()
        {
            return this.GetType().FullName;
        }

        /// <summary>
        /// Bind properties.
        /// </summary>
        protected override void BindProperties()
        {
            if (this.ProjectMgr == null)
            {
                return;
            }

            string value = this.ProjectMgr.GetProjectProperty("WarningAsErrors", true);
            if ( value != null )
                this.warningAsErrors = (value.ToLower() == "true");

        }

        /// <summary>
        /// Apply Changes on project node.
        /// </summary>
        /// <returns>E_INVALIDARG if internal ProjectMgr is null, otherwise applies changes and return S_OK.</returns>
        protected override int ApplyChanges()
        {
            if (this.ProjectMgr == null)
            {
                return VSConstants.E_INVALIDARG;
            }

            this.ProjectMgr.SetProjectProperty("WarningAsErrors", this.warningAsErrors.ToString().ToLower());

            this.IsDirty = false;

            return VSConstants.S_OK;
        }

        #endregion
    }
}


