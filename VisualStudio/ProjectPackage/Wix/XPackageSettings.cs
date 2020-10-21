//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System;
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.Security.AccessControl;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.Win32;

namespace XSharp.Project
{

    /// <summary>
    /// Helper class for setting and retrieving registry settings for the package. All machine
    /// settings are cached on first use, so only one registry read is performed.
    /// </summary>
    public class XPackageSettings
    {
        // =========================================================================================
        // Member Variables
        // =========================================================================================

        private string devEnvPath;
        private string visualStudioRegistryRoot;

        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XPackageSettings"/> class.
        /// </summary>
        /// <param name="serviceProvider">The <see cref="IServiceProvider"/> to use.</param>
        public XPackageSettings(IServiceProvider serviceProvider)
        {
            XHelperMethods.VerifyNonNullArgument(serviceProvider, "serviceProvider");

            if (serviceProvider != null)
            {
                // get the Visual Studio registry root
                ILocalRegistry3 localRegistry = XHelperMethods.GetService<ILocalRegistry3, SLocalRegistry>(serviceProvider);
                ErrorHandler.ThrowOnFailure(localRegistry.GetLocalRegistryRoot(out this.visualStudioRegistryRoot));
            }
        }

        // =========================================================================================
        // Properties
        // =========================================================================================

        /// <summary>
        /// Gets the absolute path to the devenv.exe that we're currently running in.
        /// </summary>
        /// <value>The absolute path to the devenv.exe that we're currently running in.</value>
        [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Env")]
        public virtual string DevEnvPath
        {
            get
            {
                if (this.devEnvPath == null && this.visualStudioRegistryRoot != null)
                {
                    string regPath = XHelperMethods.RegistryPathCombine(this.visualStudioRegistryRoot, @"Setup\VS");
                    using (RegistryKey regKey = Registry.LocalMachine.OpenSubKey(regPath, RegistryKeyPermissionCheck.ReadSubTree, RegistryRights.ReadKey))
                    {
                        this.devEnvPath = regKey.GetValue("EnvironmentPath", String.Empty) as string;
                    }
                }

                return this.devEnvPath;
            }
        }


    }
}
