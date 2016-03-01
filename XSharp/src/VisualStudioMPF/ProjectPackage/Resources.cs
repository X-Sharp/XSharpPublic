/********************************************************************************************

Copyright (c) Microsoft Corporation 
All rights reserved. 

Microsoft Public License: 

This license governs use of the accompanying software. If you use the software, you 
accept this license. If you do not accept the license, do not use the software. 

1. Definitions 
The terms "reproduce," "reproduction," "derivative works," and "distribution" have the 
same meaning here as under U.S. copyright law. 
A "contribution" is the original software, or any additions or changes to the software. 
A "contributor" is any person that distributes its contribution under this license. 
"Licensed patents" are a contributor's patent claims that read directly on its contribution. 

2. Grant of Rights 
(A) Copyright Grant- Subject to the terms of this license, including the license conditions 
and limitations in section 3, each contributor grants you a non-exclusive, worldwide, 
royalty-free copyright license to reproduce its contribution, prepare derivative works of 
its contribution, and distribute its contribution or any derivative works that you create. 
(B) Patent Grant- Subject to the terms of this license, including the license conditions 
and limitations in section 3, each contributor grants you a non-exclusive, worldwide, 
royalty-free license under its licensed patents to make, have made, use, sell, offer for 
sale, import, and/or otherwise dispose of its contribution in the software or derivative 
works of the contribution in the software. 

3. Conditions and Limitations 
(A) No Trademark License- This license does not grant you rights to use any contributors' 
name, logo, or trademarks. 
(B) If you bring a patent claim against any contributor over patents that you claim are 
infringed by the software, your patent license from such contributor to the software ends 
automatically. 
(C) If you distribute any portion of the software, you must retain all copyright, patent, 
trademark, and attribution notices that are present in the software. 
(D) If you distribute any portion of the software in source code form, you may do so only 
under this license by including a complete copy of this license with your distribution. 
If you distribute any portion of the software in compiled or object code form, you may only 
do so under a license that complies with this license. 
(E) The software is licensed "as-is." You bear the risk of using it. The contributors give 
no express warranties, guarantees or conditions. You may have additional consumer rights 
under your local laws which this license cannot change. To the extent permitted under your 
local laws, the contributors exclude the implied warranties of merchantability, fitness for 
a particular purpose and non-infringement.

********************************************************************************************/

using System;
using System.Reflection;
using System.Globalization;
using System.Resources;
using System.Threading;

namespace XSharp.Project
{
    /// <summary>
    /// This class represent resource storage and management functionality.
    /// </summary>
    internal sealed class Resources
    {
        #region Constants
        internal const string Application = "Application";
        internal const string ApplicationCaption = "ApplicationCaption";
        internal const string GeneralCaption = "GeneralCaption";
        internal const string AssemblyName = "AssemblyName";
        internal const string AssemblyNameDescription = "AssemblyNameDescription";
        internal const string OutputType = "OutputType";
        internal const string OutputTypeDescription = "OutputTypeDescription";
        internal const string DefaultNamespace = "DefaultNamespace";
        internal const string DefaultNamespaceDescription = "DefaultNamespaceDescription";
        internal const string StartupObject = "StartupObject";
        internal const string StartupObjectDescription = "StartupObjectDescription";
        internal const string ApplicationIcon = "ApplicationIcon";
        internal const string ApplicationIconDescription = "ApplicationIconDescription";
        internal const string Project = "Project";
        internal const string ProjectFile = "ProjectFile";
        internal const string ProjectFileDescription = "ProjectFileDescription";
        internal const string ProjectFolder = "ProjectFolder";
        internal const string ProjectFolderDescription = "ProjectFolderDescription";
        internal const string OutputFile = "OutputFile";
        internal const string OutputFileDescription = "OutputFileDescription";
        internal const string TargetFrameworkMoniker = "TargetFrameworkMoniker";
        internal const string TargetFrameworkMonikerDescription = "TargetFrameworkMonikerDescription";
        internal const string NestedProjectFileAssemblyFilter = "NestedProjectFileAssemblyFilter";
        //internal const string MsgFailedToLoadTemplateFile = "Failed to add template file to project";
        #endregion Constants

        #region Fields
        private static Resources loader;
        private ResourceManager resourceManager;
        private static Object internalSyncObjectInstance;
        #endregion Fields

        #region Constructors
        /// <summary>
        /// Internal explicitly defined default constructor.
        /// </summary>
        internal Resources()
        {
            resourceManager = new System.Resources.ResourceManager("XSharp.Project.Resources",
                Assembly.GetExecutingAssembly());
        }
        #endregion Constructors

        #region Properties
        /// <summary>
        /// Gets the internal sync. object.
        /// </summary>
        private static Object InternalSyncObject
        {
            get
            {
                if(internalSyncObjectInstance == null)
                {
                    Object o = new Object();
                    Interlocked.CompareExchange(ref internalSyncObjectInstance, o, null);
                }
                return internalSyncObjectInstance;
            }
        }
        /// <summary>
        /// Gets information about a specific culture.
        /// </summary>
        private static CultureInfo Culture
        {
            get { return null/*use ResourceManager default, CultureInfo.CurrentUICulture*/; }
        }

        /// <summary>
        /// Gets convenient access to culture-specific resources at runtime.
        /// </summary>
        public static ResourceManager ResourceManager
        {
            get
            {
                return GetLoader().resourceManager;
            }
        }
        #endregion Properties

        #region Public Implementation
        /// <summary>
        /// Provide access to resource string value.
        /// </summary>
        /// <param name="name">Received string name.</param>
        /// <param name="args">Arguments for the String.Format method.</param>
        /// <returns>Returns resources string value or null if error occured.</returns>
        public static string GetString(string name, params object[] args)
        {
            Resources resourcesInstance = GetLoader();
            if(resourcesInstance == null)
            {
                return null;
            }
            string res = resourcesInstance.resourceManager.GetString(name, Resources.Culture);

            if(args != null && args.Length > 0)
            {
                return String.Format(CultureInfo.CurrentCulture, res, args);
            }
            else
            {
                return res;
            }
        }
        /// <summary>
        /// Provide access to resource string value.
        /// </summary>
        /// <param name="name">Received string name.</param>
        /// <returns>Returns resources string value or null if error occured.</returns>
        public static string GetString(string name)
        {
            Resources resourcesInstance = GetLoader();

            if(resourcesInstance == null)
            {
                return null;
            }
            return resourcesInstance.resourceManager.GetString(name, Resources.Culture);
        }

        /// <summary>
        /// Provide access to resource object value.
        /// </summary>
        /// <param name="name">Received object name.</param>
        /// <returns>Returns resources object value or null if error occured.</returns>
        public static object GetObject(string name)
        {
            Resources resourcesInstance = GetLoader();

            if(resourcesInstance == null)
            {
                return null;
            }
            return resourcesInstance.resourceManager.GetObject(name, Resources.Culture);
        }
        #endregion Methods

        #region Private Implementation
        private static Resources GetLoader()
        {
            if(loader == null)
            {
                lock(InternalSyncObject)
                {
                    if(loader == null)
                    {
                        loader = new Resources();
                    }
                }
            }
            return loader;
        }
        #endregion
    }
}
