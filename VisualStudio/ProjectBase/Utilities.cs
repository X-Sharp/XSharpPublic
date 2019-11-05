/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation.
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A
 * copy of the license can be found in the License.txt file at the root of this distribution.
 *
 * You must not remove this notice, or any other, from this software.
 *
 * ***************************************************************************/
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Drawing;
using System.Globalization;
using System.IO;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using System.Security.Policy;
using System.Text;
using System.Text.RegularExpressions;
using System.Windows.Forms;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.Win32;
using IServiceProvider = System.IServiceProvider;
using MSBuild = Microsoft.Build.Evaluation;
using VSRegistry = Microsoft.VisualStudio.Shell.VSRegistry;

namespace Microsoft.VisualStudio.Project
{
    public static class Utilities
    {
        private const string defaultMSBuildVersion = "4.0";

        /// <summary>
        /// Look in the registry under the current hive for the path
        /// of MSBuild
        /// </summary>
        /// <returns></returns>
        [CLSCompliant(false)]
        [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "Ms")]
        [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "msbuild")]
        public static string GetMsBuildPath(IServiceProvider serviceProvider)
        {
            return GetMsBuildPath(serviceProvider, defaultMSBuildVersion);
        }

        /// <summary>
        /// Search the registry for the tools path for MSBuild.
        /// </summary>
        /// <param name="serviceProvider">The service provider.</param>
        /// <param name="version">Msbuild version.</param>
        /// <returns>The msbuild tools path</returns>
        [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "Ms")]
        public static string GetMsBuildPath(IServiceProvider serviceProvider, string version)
        {
            string msBuildPath = null;
            using(RegistryKey root = VSRegistry.RegistryRoot(serviceProvider, __VsLocalRegistryType.RegType_Configuration, false))
            {
                // Get the value from the registry
                using(RegistryKey vsKey = root.OpenSubKey("MSBuild", false))
                {
                    msBuildPath = (string)vsKey.GetValue("MSBuildBinPath", null);
                }
            }
			if (!string.IsNullOrEmpty(msBuildPath))
            {
                return msBuildPath;
            }

            // The path to MSBuild was not found in the VisualStudio's registry hive, so try to
            // find it in the new MSBuild hive.
			string registryPath = string.Format(CultureInfo.InvariantCulture, "Software\\Microsoft\\MSBuild\\ToolsVersions\\{0}", version);
            using(Microsoft.Win32.RegistryKey msbuildKey = Microsoft.Win32.Registry.LocalMachine.OpenSubKey(registryPath, false))
            {
                msBuildPath = (string)msbuildKey.GetValue("MSBuildToolsPath", null);
            }
			if (string.IsNullOrEmpty(msBuildPath))
            {
                string error = SR.GetString(SR.ErrorMsBuildRegistration, CultureInfo.CurrentUICulture);
                throw new FileLoadException(error);
            }
            return msBuildPath;
        }

        /// <summary>
        /// Is Visual Studio in design mode.
        /// </summary>
        /// <param name="serviceProvider">The service provider.</param>
        /// <returns>true if visual studio is in design mode</returns>
        public static bool IsVisualStudioInDesignMode(IServiceProvider site)
        {
            Utilities.ArgumentNotNull("site", site);

            IVsMonitorSelection selectionMonitor = site.GetService(typeof(IVsMonitorSelection)) as IVsMonitorSelection;
            Assumes.Present(selectionMonitor);
            uint cookie = 0;
            int active = 0;
            Guid designContext = VSConstants.UICONTEXT_DesignMode;
            ErrorHandler.ThrowOnFailure(selectionMonitor.GetCmdUIContextCookie(ref designContext, out cookie));
            ErrorHandler.ThrowOnFailure(selectionMonitor.IsCmdUIContextActive(cookie, out active));
            return active != 0;
        }

        /// <include file='doc\VsShellUtilities.uex' path='docs/doc[@for="Utilities.IsInAutomationFunction"]/*' />
        /// <devdoc>
        /// Is an extensibility object executing an automation function.
        /// </devdoc>
        /// <param name="serviceProvider">The service provider.</param>
        /// <returns>true if the extensibility object is executing an automation function.</returns>
        public static bool IsInAutomationFunction(IServiceProvider serviceProvider)
        {
            Utilities.ArgumentNotNull("serviceProvider", serviceProvider);

            IVsExtensibility3 extensibility = serviceProvider.GetService(typeof(EnvDTE.IVsExtensibility)) as IVsExtensibility3;

            if(extensibility == null)
            {
                throw new InvalidOperationException();
            }
            int inAutomation = 0;
            ErrorHandler.ThrowOnFailure(extensibility.IsInAutomationFunction(out inAutomation));
            return inAutomation != 0;
        }


        /// <summary>
        /// Creates a semicolon delimited list of strings. This can be used to provide the properties for VSHPROPID_CfgPropertyPagesCLSIDList, VSHPROPID_PropertyPagesCLSIDList, VSHPROPID_PriorityPropertyPagesCLSIDList
        /// </summary>
        /// <param name="guids">An array of Guids.</param>
        /// <returns>A semicolon delimited string, or null</returns>
        [CLSCompliant(false)]
        public static string CreateSemicolonDelimitedListOfStringFromGuids(Guid[] guids)
        {
            if(guids == null || guids.Length == 0)
            {
                return String.Empty;
            }

            // Create a StringBuilder with a pre-allocated buffer big enough for the
            // final string. 39 is the length of a GUID in the "B" form plus the final ';'
            StringBuilder stringList = new StringBuilder(39 * guids.Length);
            for(int i = 0; i < guids.Length; i++)
            {
                stringList.Append(guids[i].ToString("B"));
                stringList.Append(";");
            }

            return stringList.ToString().TrimEnd(';');
        }

        private static char[] curlyBraces = new char[] { '{', '}' };
        /// <summary>
        /// Take list of guids as a single string and generate an array of Guids from it
        /// </summary>
        /// <param name="guidList">Semi-colon separated list of Guids</param>
        /// <returns>Array of Guids</returns>
        [CLSCompliant(false)]
        public static Guid[] GuidsArrayFromSemicolonDelimitedStringOfGuids(string guidList)
        {
            if (guidList == null)
            {
                return null;
            }

            List<Guid> guids = new List<Guid>();
            string[] guidsStrings = guidList.Split(';');
            foreach (string guid in guidsStrings)
            {
                if (!String.IsNullOrEmpty(guid))
                    guids.Add(new Guid(guid.Trim(curlyBraces)));
            }

            return guids.ToArray();
        }

        internal static bool GuidEquals(string x, string y) {
            Guid gx, gy;
            return Guid.TryParse(x, out gx) && Guid.TryParse(y, out gy) && gx == gy;
        }

        internal static bool GuidEquals(Guid x, string y) {
            Guid gy;
            return Guid.TryParse(y, out gy) && x == gy;
        }

        internal static void CheckNotNull(object value, string message = null) {
            if(value == null) {
                throw new InvalidOperationException(message);
            }
        }

        internal static void ArgumentNotNull(string name, object value) {
            if(value == null) {
                throw new ArgumentNullException(name);
            }
        }
        internal static void ArgumentNotNullOrEmpty(string name, string value) {
            if(String.IsNullOrEmpty(value)) {
                throw new ArgumentNullException(name);
            }
        }
        /// <summary>
        /// Validates a file path by validating all file parts. If the
        /// the file name is invalid it throws an exception if the project is in automation. Otherwise it shows a dialog box with the error message.
        /// </summary>
        /// <param name="serviceProvider">The service provider</param>
        /// <param name="filePath">A full path to a file name</param>
        /// <exception cref="InvalidOperationException">In case of failure an InvalidOperationException is thrown.</exception>
        public static void ValidateFileName(IServiceProvider serviceProvider, string filePath)
        {
            string errorMessage = String.Empty;
            if(String.IsNullOrEmpty(filePath))
            {
                errorMessage = SR.GetString(SR.ErrorInvalidFileName, CultureInfo.CurrentUICulture);
            }
            else if(filePath.Length > NativeMethods.MAX_PATH)
            {
                errorMessage = String.Format(CultureInfo.CurrentCulture, SR.GetString(SR.PathTooLong, CultureInfo.CurrentUICulture), filePath);
            }
            else if(ContainsInvalidFileNameChars(filePath))
            {
                errorMessage = SR.GetString(SR.ErrorInvalidFileName, CultureInfo.CurrentUICulture);
            }

            if(errorMessage.Length == 0)
            {
                string fileName = Path.GetFileName(filePath);
                if(String.IsNullOrEmpty(fileName) || IsFileNameInvalid(fileName))
                {
                    errorMessage = SR.GetString(SR.ErrorInvalidFileName, CultureInfo.CurrentUICulture);
                }
                else
                {
                    string fileNameWithoutExtension = Path.GetFileNameWithoutExtension(fileName);

                    // If there is no filename or it starts with a leading dot issue an error message and quit.
                    if(String.IsNullOrEmpty(fileNameWithoutExtension) || fileNameWithoutExtension[0] == '.')
                    {
                        errorMessage = SR.GetString(SR.FileNameCannotContainALeadingPeriod, CultureInfo.CurrentUICulture);
                    }
                }
            }

            if(errorMessage.Length > 0)
            {
                // If it is not called from an automation method show a dialog box.
                if(!Utilities.IsInAutomationFunction(serviceProvider))
                {
                    string title = null;
                    OLEMSGICON icon = OLEMSGICON.OLEMSGICON_CRITICAL;
                    OLEMSGBUTTON buttons = OLEMSGBUTTON.OLEMSGBUTTON_OK;
                    OLEMSGDEFBUTTON defaultButton = OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST;
                    VsShellUtilities.ShowMessageBox(serviceProvider, title, errorMessage, icon, buttons, defaultButton);
                }
                else
                {
                    throw new InvalidOperationException(errorMessage);
                }
            }

        }

        /// <summary>
        /// Creates a CALPOLESTR from a list of strings
        /// It is the responsibility of the caller to release this memory.
        /// </summary>
        /// <param name="guids"></param>
        /// <returns>A CALPOLESTR that was created from the the list of strings.</returns>
        [CLSCompliant(false)]
        [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "CALPOLESTR")]
        public static CALPOLESTR CreateCALPOLESTR(IList<string> strings)
        {
            CALPOLESTR calpolStr = new CALPOLESTR();

            if(strings != null)
            {
                // Demand unmanaged permissions in order to access unmanaged memory.
                new SecurityPermission(SecurityPermissionFlag.UnmanagedCode).Demand();

                calpolStr.cElems = (uint)strings.Count;

                int size = Marshal.SizeOf(typeof(IntPtr));

                calpolStr.pElems = Marshal.AllocCoTaskMem(strings.Count * size);

                IntPtr ptr = calpolStr.pElems;

                foreach(string aString in strings)
                {
                    IntPtr tempPtr = Marshal.StringToCoTaskMemUni(aString);
                    Marshal.WriteIntPtr(ptr, tempPtr);
                    ptr = new IntPtr(ptr.ToInt64() + size);
                }
            }

            return calpolStr;
        }

        /// <summary>
        /// Creates a CADWORD from a list of tagVsSccFilesFlags. Memory is allocated for the elems.
        /// It is the responsibility of the caller to release this memory.
        /// </summary>
        /// <param name="guids"></param>
        /// <returns>A CADWORD created from the list of tagVsSccFilesFlags.</returns>
        [CLSCompliant(false)]
        [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "CADWORD")]
        public static CADWORD CreateCADWORD(IList<tagVsSccFilesFlags> flags)
        {
            CADWORD cadWord = new CADWORD();

            if(flags != null)
            {
                // Demand unmanaged permissions in order to access unmanaged memory.
                new SecurityPermission(SecurityPermissionFlag.UnmanagedCode).Demand();

                cadWord.cElems = (uint)flags.Count;

                int size = Marshal.SizeOf(typeof(UInt32));

                cadWord.pElems = Marshal.AllocCoTaskMem(flags.Count * size);

                IntPtr ptr = cadWord.pElems;

                foreach(tagVsSccFilesFlags flag in flags)
                {
                    Marshal.WriteInt32(ptr, (int)flag);
                    ptr = new IntPtr(ptr.ToInt64() + size);
                }
            }

            return cadWord;
        }

        /// <summary>
        /// Splits a bitmap from a Stream into an ImageList
        /// </summary>
        /// <param name="imageStream">A Stream representing a Bitmap</param>
        /// <returns>An ImageList object representing the images from the given stream</returns>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2000:Dispose objects before losing scope")]
        public static ImageList GetImageList(Stream imageStream)
        {
            ImageList ilist = new ImageList();

            if(imageStream == null)
            {
				Debug.Fail("ImageStream was null.");
                return ilist;
            }
            ilist.ColorDepth = ColorDepth.Depth24Bit;
            ilist.ImageSize = new Size(16, 16);
            Bitmap bitmap = new Bitmap(imageStream);
            ilist.Images.AddStrip(bitmap);
            ilist.TransparentColor = Color.Magenta;
            return ilist;
        }

        /// <summary>
        /// Splits a bitmap from a pointer to an ImageList
        /// </summary>
        /// <param name="imageListAsPointer">A pointer to a bitmap of images to split</param>
        /// <returns>An ImageList object representing the images from the given stream</returns>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2000:Dispose objects before losing scope")]
        public static ImageList GetImageList(object imageListAsPointer)
        {
            ImageList images = null;

            IntPtr intPtr = new IntPtr((int)imageListAsPointer);
            HandleRef hImageList = new HandleRef(null, intPtr);
            int count = UnsafeNativeMethods.ImageList_GetImageCount(hImageList);

            if(count > 0)
            {
                // Create a bitmap big enough to hold all the images
                Bitmap b = new Bitmap(16 * count, 16);
                Graphics g = Graphics.FromImage(b);

                // Loop through and extract each image from the imagelist into our own bitmap
                IntPtr hDC = IntPtr.Zero;
                try
                {
                    hDC = g.GetHdc();
                    HandleRef handleRefDC = new HandleRef(null, hDC);
                    for(int i = 0; i < count; i++)
                    {
                        UnsafeNativeMethods.ImageList_Draw(hImageList, i, handleRefDC, i * 16, 0, NativeMethods.ILD_NORMAL);
                    }
                }
                finally
                {
                    if(g != null && hDC != IntPtr.Zero)
                    {
                        g.ReleaseHdc(hDC);
                    }
                }

                // Create a new imagelist based on our stolen images
                images = new ImageList();
                images.ColorDepth = ColorDepth.Depth24Bit;
                images.ImageSize = new Size(16, 16);
                images.Images.AddStrip(b);
            }
            return images;
        }

        /// <summary>
        /// Gets the active configuration name.
        /// </summary>
        /// <param name="automationObject">The automation object.</param>
        /// <returns>The name of the active configuration.</returns>
        internal static string GetActiveConfigurationName(EnvDTE.Project automationObject)
        {
            Utilities.ArgumentNotNull("automationObject", automationObject);

            string currentConfigName = string.Empty;
            if (automationObject.ConfigurationManager != null)
            {
                try
                {
                    EnvDTE.Configuration activeConfig = automationObject.ConfigurationManager.ActiveConfiguration;
                    if (activeConfig != null)
                    {
                        currentConfigName = activeConfig.ConfigurationName;
                    }
                }
                catch (COMException ex)
                {
                    if (System.Diagnostics.Debugger.IsAttached)
                        Debug.WriteLine("Failed to get active configuration because of {0}", ex);
                }
            }
            return currentConfigName;

        }

		/// <summary>
		/// Gets the active platform name.
		/// </summary>
		/// <param name="automationObject">The automation object.</param>
		/// <returns>The name of the active platform.</returns>
		internal static string GetActivePlatformName(EnvDTE.Project automationObject)
		{
            if (automationObject == null)
			{
				throw new ArgumentNullException("automationObject");
			}

			string currentPlatformName = string.Empty;
            ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            if (automationObject.ConfigurationManager != null)
			{
                try
                {

					EnvDTE.Configuration activeConfig = automationObject.ConfigurationManager.ActiveConfiguration;
					if (activeConfig != null)
					{
						currentPlatformName = activeConfig.PlatformName;
					}
                }
                catch (COMException ex)
                {
                    if (System.Diagnostics.Debugger.IsAttached)
                        Debug.WriteLine("Failed to get active platform because of {0}", ex);
                }
			}

			return currentPlatformName;
		}


        /// <summary>
        /// Verifies that two objects represent the same instance of a COM object.
        /// This essentially compares the IUnkown pointers of the 2 objects.
        /// This is needed in scenario where aggregation is involved.
        /// </summary>
        /// <param name="obj1">Can be an object, interface or IntPtr</param>
        /// <param name="obj2">Can be an object, interface or IntPtr</param>
        /// <returns>True if the 2 items represent the same thing</returns>
        [SuppressMessage("Microsoft.Naming", "CA1720:IdentifiersShouldNotContainTypeNames", MessageId = "obj")]
        public static bool IsSameComObject(object obj1, object obj2)
        {
            bool isSame = false;
            IntPtr unknown1 = IntPtr.Zero;
            IntPtr unknown2 = IntPtr.Zero;
            try
            {
                // If we have 2 null, then they are not COM objects and as such "it's not the same COM object"
                if(obj1 != null && obj2 != null)
                {
                    unknown1 = QueryInterfaceIUnknown(obj1);
                    unknown2 = QueryInterfaceIUnknown(obj2);

                    isSame = IntPtr.Equals(unknown1, unknown2);
                }
            }
            finally
            {
                if(unknown1 != IntPtr.Zero)
                {
                    Marshal.Release(unknown1);
                }

                if(unknown2 != IntPtr.Zero)
                {
                    Marshal.Release(unknown2);
                }

            }

            return isSame;
        }

        /// <summary>
        /// Retrieve the IUnknown for the managed or COM object passed in.
        /// </summary>
        /// <param name="objToQuery">Managed or COM object.</param>
        /// <returns>Pointer to the IUnknown interface of the object.</returns>
        internal static IntPtr QueryInterfaceIUnknown(object objToQuery)
        {
            bool releaseIt = false;
            IntPtr unknown = IntPtr.Zero;
            IntPtr result;
            try
            {
                if(objToQuery is IntPtr)
                {
                    unknown = (IntPtr)objToQuery;
                }
                else
                {
                    // This is a managed object (or RCW)
                    unknown = Marshal.GetIUnknownForObject(objToQuery);
                    releaseIt = true;
                }

                // We might already have an IUnknown, but if this is an aggregated
                // object, it may not be THE IUnknown until we QI for it.
                Guid IID_IUnknown = VSConstants.IID_IUnknown;
                ErrorHandler.ThrowOnFailure(Marshal.QueryInterface(unknown, ref IID_IUnknown, out result));
            }
            finally
            {
                if(releaseIt && unknown != IntPtr.Zero)
                {
                    Marshal.Release(unknown);
                }

            }

            return result;
        }

        /// <summary>
        /// Returns true if thename that can represent a path, absolut or relative, or a file name contains invalid filename characters.
        /// </summary>
        /// <param name="name">File name</param>
        /// <returns>true if file name is invalid</returns>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1062:Validate arguments of public methods", MessageId = "0",
            Justification = "The name is validated.")]
        public static bool ContainsInvalidFileNameChars(string name)
		{
            if (String.IsNullOrEmpty(name))
			{
                return true;
            }

            try
			{
                if (Path.IsPathRooted(name) && !name.StartsWith(@"\\", StringComparison.Ordinal))
				{
                    string root = Path.GetPathRoot(name);
                    name = name.Substring(root.Length);
                }
            }
                // The Path methods used by ContainsInvalidFileNameChars return argument exception if the filePath contains invalid characters.
            catch (ArgumentException)
			{
                return true;
            }

            Microsoft.VisualStudio.Shell.Url uri = new Microsoft.VisualStudio.Shell.Url(name);

            // This might be confusing bur Url.IsFile means that the uri represented by the name is either absolut or relative.
            if (uri.IsFile)
			{
                string[] segments = uri.Segments;
                if (segments != null && segments.Length > 0)
				{
                    foreach (string segment in segments)
					{
                        if (IsFilePartInValid(segment))
						{
                            return true;
                        }
                    }

                    // Now the last segment should be specially taken care, since that cannot be all dots or spaces.
                    string lastSegment = segments[segments.Length - 1];
                    string filePart = Path.GetFileNameWithoutExtension(lastSegment);
                    // if the file is only an extension (.fob) then it's ok, otherwise we need to do the special checks.
                    if (filePart.Length != 0 && (IsFileNameAllGivenCharacter('.', filePart) || IsFileNameAllGivenCharacter(' ', filePart)))
					{
                        return true;
                    }
                }
            }
			else
			{
                // The assumption here is that we got a file name.
                string filePart = Path.GetFileNameWithoutExtension(name);
                if (IsFileNameAllGivenCharacter('.', filePart) || IsFileNameAllGivenCharacter(' ', filePart))
				{
                    return true;
                }


                return IsFilePartInValid(name);
            }

            return false;
        }

        /// Checks if a file name is valid.
        /// </devdoc>
        /// <param name="fileName">The name of the file</param>
        /// <returns>True if the file is valid.</returns>
        public static bool IsFileNameInvalid(string fileName)
        {
            if(String.IsNullOrEmpty(fileName))
            {
                return true;
            }

            if(IsFileNameAllGivenCharacter('.', fileName) || IsFileNameAllGivenCharacter(' ', fileName))
            {
                return true;
            }


            return IsFilePartInValid(fileName);

        }

        /// <summary>
        /// Helper method to call a converter explicitely to convert to an enum type
        /// </summary>
        /// <typeparam name="T">THe enum to convert to</typeparam>
        /// <typeparam name="V">The converter that will be created</typeparam>
        /// <param name="value">The enum value to be converted to</param>
        /// <param name="typeToConvert">The type to convert</param>
        /// <param name="culture">The culture to use to read the localized strings</param>
        /// <returns></returns>
        [CLSCompliant(false)]
        public static object ConvertToType<T>(T value, Type typeToConvert, CultureInfo culture)
            where T : struct
        {
            EnumConverter converter = GetEnumConverter<T>();
            if(converter == null)
            {
                return null;
            }
            if(converter.CanConvertTo(typeToConvert))
            {
                return converter.ConvertTo(null, culture, value, typeToConvert);
            }

            return null;
        }

        /// <summary>
        /// Helper method for converting from a string to an enum using a converter.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="value"></param>
        /// <param name="culture">The culture to use to read the localized strings</param>
        /// <returns></returns>
        [CLSCompliant(false)]
        public static Nullable<T> ConvertFromType<T>(string value, CultureInfo culture)
            where T : struct
        {
            Nullable<T> returnValue = new Nullable<T>();

            returnValue = returnValue.GetValueOrDefault();

            if(value == null)
            {
                return returnValue;
            }

            EnumConverter converter = GetEnumConverter<T>();
            if(converter == null)
            {
                return returnValue;
            }

            if(converter.CanConvertFrom(value.GetType()))
            {
                object converted = converter.ConvertFrom(null, culture, value);

                if(converted != null && (converted is T))
                {
                    returnValue = (T)converted;
                }
            }

            return returnValue;
        }


        /// <summary>
        /// Sets a string value from an enum
        /// </summary>
        /// <typeparam name="T">The enum type</typeparam>
        /// <param name="enumValue">The value of teh enum.</param>
        /// <returns></returns>
        [CLSCompliant(false)]
        public static string SetStringValueFromConvertedEnum<T>(T enumValue, CultureInfo culture)
            where T : struct
        {
            string convertToType = ConvertToType<T>(enumValue, typeof(string), culture) as string;

            if(convertToType == null)
            {
                return String.Empty;
            }

            return convertToType;
        }

		/// <summary>
		/// Get the default global properties for a new project instance.
		/// </summary>
		/// <param name="provider">The service provider.</param>
		/// <returns></returns>
		private static IDictionary<string, string> GetProjectDefaultGlobalProperties(IServiceProvider provider)
		{
			Dictionary<string, string> properties = new Dictionary<string, string>();
			string solutionDirectory = null;
			string solutionFile = null;
			string userOptionsFile = null;
			string installDir = null;

			if (provider != null)
			{
				IVsSolution solution = provider.GetService(typeof(SVsSolution)) as IVsSolution;
				if (solution != null)
				{
					// We do not want to throw. If we cannot set the solution related constants we set them to empty string.
					solution.GetSolutionInfo(out solutionDirectory, out solutionFile, out userOptionsFile);
				}

				// DevEnvDir property
				IVsShell shell = provider.GetService(typeof(SVsShell)) as IVsShell;
				if (shell != null)
				{
					object installDirAsObject = null;
					// We do not want to throw. If we cannot set the solution related constants we set them to empty string.
					shell.GetProperty((int)__VSSPROPID.VSSPROPID_InstallDirectory, out installDirAsObject);
					installDir = ((string)installDirAsObject);
				}
			}

			if (solutionDirectory == null)
			{
				solutionDirectory = String.Empty;
			}

			if (solutionFile == null)
			{
				solutionFile = String.Empty;
			}

			string solutionFileName = (solutionFile.Length == 0) ? String.Empty : Path.GetFileName(solutionFile);
			string solutionName = (solutionFile.Length == 0) ? String.Empty : Path.GetFileNameWithoutExtension(solutionFile);
			string solutionExtension = (solutionFile.Length == 0 || !Path.HasExtension(solutionFile)) ? String.Empty : Path.GetExtension(solutionFile);

			properties.Add(GlobalProperty.SolutionDir.ToString(), solutionDirectory);
			properties.Add(GlobalProperty.SolutionPath.ToString(), solutionFile);
			properties.Add(GlobalProperty.SolutionFileName.ToString(), solutionFileName);
			properties.Add(GlobalProperty.SolutionName.ToString(), solutionName);
			properties.Add(GlobalProperty.SolutionExt.ToString(), solutionExtension);

			// Other misc properties
			properties.Add(GlobalProperty.BuildingInsideVisualStudio.ToString(), "true");

			if (String.IsNullOrEmpty(installDir))
			{
				installDir = String.Empty;
			}
			else
			{
				// Ensure that we have traimnling backslash as this is done for the langproj macros too.
				if (installDir[installDir.Length - 1] != Path.DirectorySeparatorChar)
				{
					installDir += Path.DirectorySeparatorChar;
				}
			}

			properties.Add(GlobalProperty.DevEnvDir.ToString(), installDir);

			return properties;
		}

        /// <summary>
        /// Initializes the in memory project. Sets BuildEnabled on the project to true.
        /// </summary>
        /// <param name="engine">The build engine to use to create a build project.</param>
        /// <param name="fullProjectPath">The full path of the project.</param>
        /// <returns>A loaded msbuild project.</returns>
        internal static MSBuild.Project InitializeMsBuildProject(MSBuild.ProjectCollection buildEngine, string fullProjectPath)
        {
            Utilities.ArgumentNotNullOrEmpty("fullProjectPath", fullProjectPath);

            // Call GetFullPath to expand any relative path passed into this method.
            fullProjectPath = Path.GetFullPath(fullProjectPath);


            // Check if the project already has been loaded with the fullProjectPath. If yes return the build project associated to it.
            List<MSBuild.Project> loadedProject = new List<MSBuild.Project>(buildEngine.GetLoadedProjects(fullProjectPath));
            MSBuild.Project buildProject = loadedProject != null && loadedProject.Count > 0 && loadedProject[0] != null ? loadedProject[0] : null;

            if(buildProject == null)
            {
               buildProject = buildEngine.LoadProject(fullProjectPath);
            }

            return buildProject;
        }

        /// <summary>
        /// Loads a project file for the file. If the build project exists and it was loaded with a different file then it is unloaded first.
        /// </summary>
        /// <param name="engine">The build engine to use to create a build project.</param>
        /// <param name="fullProjectPath">The full path of the project.</param>
        /// <param name="exitingBuildProject">An Existing build project that will be reloaded.</param>
        /// <returns>A loaded msbuild project.</returns>
        internal static MSBuild.Project ReinitializeMsBuildProject(MSBuild.ProjectCollection buildEngine, string fullProjectPath, MSBuild.Project exitingBuildProject)
        {
            // If we have a build project that has been loaded with another file unload it.
            try
            {
                if(exitingBuildProject != null && exitingBuildProject.ProjectCollection != null && !NativeMethods.IsSamePath(exitingBuildProject.FullPath, fullProjectPath))
                {
                    buildEngine.UnloadProject(exitingBuildProject);
                }
            }
            // We  catch Invalid operation exception because if the project was unloaded while we touch the ParentEngine the msbuild API throws.
            // Is there a way to figure out that a project was unloaded?
            catch(InvalidOperationException)
            {
            }

            return Utilities.InitializeMsBuildProject(buildEngine, fullProjectPath);
        }

        /// <summary>
        /// Initialize the build engine. Sets the build enabled property to true. The engine is initialzed if the passed in engine is null or does not have its bin path set.
        /// </summary>
        /// <param name="engine">An instance of MSBuild.ProjectCollection build engine, that will be checked if initialized.</param>
        /// <param name="engine">The service provider.</param>
        /// <returns>The buildengine to use.</returns>
        internal static MSBuild.ProjectCollection InitializeMsBuildEngine(MSBuild.ProjectCollection existingEngine, IServiceProvider serviceProvider)
        {
            if(serviceProvider == null)
            {
                throw new ArgumentNullException("serviceProvider");
            }

            if(existingEngine == null)
            {
                MSBuild.ProjectCollection buildEngine = MSBuild.ProjectCollection.GlobalProjectCollection;
                return buildEngine;
            }

            return existingEngine;
        }

        /// <summary>
        /// Get the outer T implementation
        /// </summary>
        internal static T GetOuterAs<T>(object o)
            where T : class
        {
            T hierarchy = null;

            // The hierarchy of a node is its project node hierarchy.
            IntPtr projectUnknown = Marshal.GetIUnknownForObject(o);

            try
            {
                hierarchy = (T)Marshal.GetTypedObjectForIUnknown(projectUnknown, typeof(T));
            }
            finally
            {
                if (projectUnknown != IntPtr.Zero)
                {
                    Marshal.Release(projectUnknown);
                }
            }

            return hierarchy;
        }

        /// <summary>
        /// Gets an instance of an EnumConverter for enums that have PropertyPageTypeConverter attribute
        /// </summary>
        /// <typeparam name="T">The type to search for the PropertyPageTypeConverter attribute.</typeparam>
        /// <returns>An instance of an enum converter, or null if none found.</returns>
        private static EnumConverter GetEnumConverter<T>()
            where T : struct
        {
            object[] attributes = typeof(T).GetCustomAttributes(typeof(PropertyPageTypeConverterAttribute), true);

            // There should be only one PropertyPageTypeConverterAttribute defined on T
            if(attributes != null && attributes.Length == 1)
            {

                Debug.Assert(attributes[0] is PropertyPageTypeConverterAttribute, "The returned attribute must be an attribute is PropertyPageTypeConverterAttribute");
                PropertyPageTypeConverterAttribute converterAttribute = (PropertyPageTypeConverterAttribute)attributes[0];

                if(converterAttribute.ConverterType.IsSubclassOf(typeof(EnumConverter)))
                {
                    return Activator.CreateInstance(converterAttribute.ConverterType) as EnumConverter;
                }
            }

            return null;
        }

        /// <summary>>
        /// Checks if the file name is all the given character.
        /// </summary>
        private static bool IsFileNameAllGivenCharacter(char c, string fileName)
        {
            // A valid file name cannot be all "c" .
            int charFound = 0;
            for(charFound = 0; charFound < fileName.Length && fileName[charFound] == c; ++charFound) ;
            if(charFound >= fileName.Length)
            {
                return true;
            }

            return false;
        }

        private const string _reservedName = "(\\b(nul|con|aux|prn)\\b)|(\\b((com|lpt)[0-9])\\b)";
        private const string _invalidChars = "([\\/:*?\"<>|#%])";
        private const string _regexToUseForFileName = _reservedName + "|" + _invalidChars;
        private static Regex _unsafeFileNameCharactersRegex = new Regex(_regexToUseForFileName, RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);
        private static Regex _unsafeCharactersRegex = new Regex(_invalidChars, RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);
        /// <summary>
        /// Checks whether a file part contains valid characters. The file part can be any part of a non rooted path.
        /// </summary>
        /// <param name="filePart"></param>
        /// <returns></returns>
        private static bool IsFilePartInValid(string filePart)
        {
            if(String.IsNullOrEmpty(filePart))
            {
                return true;
            }
            String fileNameToVerify = filePart;

            // Define a regular expression that covers all characters that are not in the safe character sets.
            // It is compiled for performance.

            // The filePart might still be a file and extension. If it is like that then we must check them separately, since different rules apply
            string extension = String.Empty;
            try
            {
                extension = Path.GetExtension(filePart);
            }
            // We catch the ArgumentException because we want this method to return true if the filename is not valid. FilePart could be for example #�&%"�&"% and that would throw ArgumentException on GetExtension
            catch(ArgumentException)
            {
                return true;
            }

            if(!String.IsNullOrEmpty(extension))
            {
                // Check the extension first
                bool isMatch = _unsafeCharactersRegex.IsMatch(extension);
                if(isMatch)
                {
                    return isMatch;
                }

                // We want to verify here everything but the extension.
                // We cannot use GetFileNameWithoutExtension because it might be that for example (..\\filename.txt) is passed in asnd that should fail, since that is not a valid filename.
                fileNameToVerify = filePart.Substring(0, filePart.Length - extension.Length);

                if(String.IsNullOrEmpty(fileNameToVerify))
                {
                    return true;
                }
            }

            // We verify CLOCK$ outside the regex since for some reason the regex is not matching the clock\\$ added.
            if(String.Compare(fileNameToVerify, "CLOCK$", StringComparison.OrdinalIgnoreCase) == 0)
            {
                return true;
            }

            return _unsafeFileNameCharactersRegex.IsMatch(fileNameToVerify);
        }

        /// <summary>
        /// Copy a directory recursively to the specified non-existing directory
        /// </summary>
        /// <param name="source">Directory to copy from</param>
        /// <param name="target">Directory to copy to</param>
        public static void RecursivelyCopyDirectory(string source, string target)
        {
            // Make sure it doesn't already exist
            if(Directory.Exists(target))
                throw new ArgumentException(String.Format(CultureInfo.CurrentCulture, SR.GetString(SR.FileOrFolderAlreadyExists, CultureInfo.CurrentUICulture), target));

            Directory.CreateDirectory(target);
            DirectoryInfo directory = new DirectoryInfo(source);

            // Copy files
            foreach(FileInfo file in directory.GetFiles())
            {
                file.CopyTo(Path.Combine(target, file.Name));
            }

            // Now recurse to child directories
            foreach(DirectoryInfo child in directory.GetDirectories())
            {
                RecursivelyCopyDirectory(child.FullName, Path.Combine(target, child.Name));
            }
        }

        /// <summary>
        /// Canonicalizes a file name, including:
        ///  - determines the full path to the file
        ///  - casts to upper case
        /// Canonicalizing a file name makes it possible to compare file names using simple simple string comparison.
        ///
        /// Note: this method does not handle shared drives and UNC drives.
        /// </summary>
        /// <param name="anyFileName">A file name, which can be relative/absolute and contain lower-case/upper-case characters.</param>
        /// <returns>Canonicalized file name.</returns>
        internal static string CanonicalizeFileName(string anyFileName)
        {
            // Get absolute path
            // Note: this will not handle UNC paths
            FileInfo fileInfo = new FileInfo(anyFileName);
            string fullPath = fileInfo.FullName;

            // Cast to upper-case
            fullPath = fullPath.ToUpperInvariant();

            return fullPath;
        }


        /// <summary>
        /// Determines if a file is a template.
        /// </summary>
        /// <param name="fileName">The file to check whether it is a template file</param>
        /// <returns>true if the file is a template file</returns>
        internal static bool IsTemplateFile(string fileName)
        {
            if(String.IsNullOrEmpty(fileName))
            {
                return false;
            }

            string extension = Path.GetExtension(fileName);
            return (String.Compare(extension, ".vstemplate", StringComparison.OrdinalIgnoreCase) == 0 || string.Compare(extension, ".vsz", StringComparison.OrdinalIgnoreCase) == 0);
        }
        /// <summary>
        /// Save dirty files
        /// </summary>
        /// <returns>Whether succeeded</returns>
        public static bool SaveDirtyFiles() {
            var rdt = ServiceProvider.GlobalProvider.GetService(typeof(SVsRunningDocumentTable)) as IVsRunningDocumentTable;
            if (rdt != null) {
                // Consider using (uint)(__VSRDTSAVEOPTIONS.RDTSAVEOPT_SaveIfDirty | __VSRDTSAVEOPTIONS.RDTSAVEOPT_PromptSave)
                // when VS settings include prompt for save on build
                var saveOpt = (uint)__VSRDTSAVEOPTIONS.RDTSAVEOPT_SaveIfDirty;
                var hr = rdt.SaveDocuments(saveOpt, null, VSConstants.VSITEMID_NIL, VSConstants.VSCOOKIE_NIL);
                if (hr == VSConstants.E_ABORT) {
                    return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Retrives the configuration and the platform using the IVsSolutionBuildManager2 interface.
        /// </summary>
        /// <param name="serviceProvider">A service provider.</param>
        /// <param name="hierarchy">The hierarchy whose configuration is requested.  This method calls into
        /// native code and may be called on a background thread, so make sure the IVsHierarchy passed is
        /// safe to use for that sort of interop.</param>
        /// <param name="configuration">The name of the active configuration.</param>
        /// <param name="platform">The name of the platform.</param>
        /// <returns>true if successfull.</returns>
		/// <summary>
		/// Retrives the configuration and the platform using the IVsSolutionBuildManager2 interface.
		/// </summary>
		/// <param name="serviceProvider">A service provider.</param>
		/// <param name="hierarchy">The hierrachy whose configuration is requested.</param>
		/// <param name="configuration">The name of the active configuration.</param>
		/// <param name="platform">The name of the platform.</param>
		/// <returns>true if successfull.</returns>
		internal static bool TryGetActiveConfigurationAndPlatform(System.IServiceProvider serviceProvider, IVsHierarchy hierarchy, out ConfigCanonicalName configCanonicalName)
		{
			if (serviceProvider == null)
			{
				throw new ArgumentNullException("serviceProvider");
			}

			if (hierarchy == null)
			{
				throw new ArgumentNullException("hierarchy");
			}

			IVsSolutionBuildManager2 solutionBuildManager = serviceProvider.GetService(typeof(SVsSolutionBuildManager)) as IVsSolutionBuildManager2;

			if (solutionBuildManager == null)
			{
				configCanonicalName = new ConfigCanonicalName();
				return false;
			}

			IVsProjectCfg[] activeConfigs = new IVsProjectCfg[1];
			ErrorHandler.ThrowOnFailure(solutionBuildManager.FindActiveProjectCfg(IntPtr.Zero, IntPtr.Zero, hierarchy, activeConfigs));

			IVsProjectCfg activeCfg = activeConfigs[0];

			// Can it be that the activeCfg is null?
			System.Diagnostics.Debug.Assert(activeCfg != null, "Cannot find the active configuration");

			string canonicalName;
			ErrorHandler.ThrowOnFailure(activeCfg.get_CanonicalName(out canonicalName));
			configCanonicalName = new ConfigCanonicalName(canonicalName);
			return true;
		}

        /// <summary>
        /// Determines whether the shell is in command line mode.
        /// </summary>
        /// <param name="serviceProvider">A reference to a Service Provider.</param>
        /// <returns>true if the shell is in command line mode. false otherwise.</returns>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
        internal static bool IsShellInCommandLineMode(System.IServiceProvider serviceProvider)
        {
            Utilities.ArgumentNotNull("serviceProvider", serviceProvider);

            IVsShell shell = serviceProvider.GetService(typeof(SVsShell)) as IVsShell;
            if(shell == null)
            {
                throw new InvalidOperationException();
            }

            object isInCommandLineModeAsObject;
            ErrorHandler.ThrowOnFailure(shell.GetProperty((int)__VSSPROPID.VSSPROPID_IsInCommandLineMode, out isInCommandLineModeAsObject));

            return ((bool)isInCommandLineModeAsObject);
        }

		/// <summary>
		/// Saves the dialog state in the solution
		/// </summary>
		/// <param name="serviceProvider">A reference to a Service Provider.</param>
		/// <param name="projectLoadSecurityDialogState">The dialog state</param>
		internal static void SaveDialogStateInSolution(IServiceProvider serviceProvider, _ProjectLoadSecurityDialogState projectLoadSecurityDialogState)
		{
			if (serviceProvider == null)
			{
				throw new ArgumentNullException("serviceProvider");
			}

			IVsSolution solution = serviceProvider.GetService(typeof(SVsSolution)) as IVsSolution;

			if (solution == null)
			{
				throw new InvalidOperationException();
			}

			ErrorHandler.ThrowOnFailure(solution.SetProperty((int)__VSPROPID2.VSPROPID_ProjectLoadSecurityDialogState, projectLoadSecurityDialogState));
		}
        internal static bool DeleteFileSafe(string fileName)
        {
            try
            {
                if (System.IO.File.Exists(fileName))
                {
                    System.IO.File.SetAttributes(fileName, FileAttributes.Normal);
                    System.IO.File.Delete(fileName);

                }
            }
            catch (Exception e)
            {
                Debug.WriteLine(e.Message);
                return false;
            }
            return true;
        }
	}
}
