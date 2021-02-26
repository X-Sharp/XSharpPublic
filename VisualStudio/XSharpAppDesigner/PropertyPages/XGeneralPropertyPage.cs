//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

namespace XSharp.Project
{
    using System;
    using System.Linq;
    using System.Globalization;
    using System.Runtime.InteropServices;
    using System.Windows.Forms;
    using Microsoft.VisualStudio;
    using Microsoft.VisualStudio.Package;
    using Microsoft.VisualStudio.Project;
    using Microsoft.VisualStudio.Shell;

    /// <summary>
    /// Property page for the build events.
    /// </summary>
    [ComVisible(true)]
    [Guid(XSharpConstants.GeneralPropertiesPage)]
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProvideObject(typeof(XSharpGeneralPropertyPage))]
    public class XSharpGeneralPropertyPage : XPropertyPage
    {
        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpBuildEventsPropertyPage"/> class.
        /// </summary>
        OutputTypeConverter converterOutPut;
        DialectConverter converterDialect;
        FrameworkNameConverter converterFramework;
        public XSharpGeneralPropertyPage()
        {
            this.PageName = "Application";
            this.PerConfig = false;
            converterDialect = new DialectConverter();
            converterFramework = new FrameworkNameConverter();
            converterOutPut = new OutputTypeConverter();
        }

        // =========================================================================================
        // Methods
        // =========================================================================================

        /// <summary>
        /// Gets a project property.
        /// </summary>
        /// <param name="propertyName">The name of the property to get.</param>
        /// <returns>
        /// Value of the property, or null if the property is unset or inconsistent across configurations.
        /// </returns>
        public override string GetProperty(string propertyName)
        {
            string value = base.GetProperty(propertyName);

            if (propertyName == XProjectFileConstants.OutputType)
            {
                var outputType = (OutputType) converterOutPut.ConvertFrom(value);
                value = (string)converterOutPut.ConvertTo(outputType, typeof(System.String));
            }
            else if (propertyName == "Dialect")
            {
                var dialect = (Dialect)converterDialect.ConvertFrom(value);
                value = (string) converterDialect.ConvertTo(dialect, typeof(System.String));
            }
            else if (propertyName == "TargetFrameworkVersion")
            {
                if (!value.StartsWith(".NETFramework"))
                    value = ".NETFramework,Version =" + value;
                value = converterFramework.ConvertFrom(value).ToString();
            }
            return value;
        }

        /// <summary>
        /// Sets a project property.
        /// </summary>
        /// <param name="propertyName">Name of the property to set.</param>
        /// <param name="value">Value of the property.</param>
        public override void SetProperty(string propertyName, string value)
        {
            if (propertyName == ProjectFileConstants.OutputType)
            {
                var output = (OutputType)converterOutPut.ConvertFrom(value);
                value = output.ToString();

            }
            else if (propertyName == "Dialect")
            {
                var dialect = (Dialect)converterDialect.ConvertFrom(value);
                value = dialect.ToString();
            }
            else if (propertyName == "TargetFrameworkVersion")
            {
                value = value.ToLower();
                var pos = value.IndexOf("version=");
                if (pos > 0)
                {
                    value = value.Substring(pos + "version=".Length);
                }
            }
            base.SetProperty(propertyName, value);
        }

        /// <summary>
        /// Creates the controls that constitute the property page. This should be safe to re-entrancy.
        /// </summary>
        /// <returns>The newly created main control that hosts the property page.</returns>
        protected override XPropertyPagePanel CreatePropertyPagePanel()
        {
            return new XGeneralPropertyPagePanel(this);
        }
    }
}
