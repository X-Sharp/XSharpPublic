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
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.Runtime.InteropServices;

namespace Microsoft.VisualStudio.Project.Automation
{
    [SuppressMessage("Microsoft.Interoperability", "CA1405:ComVisibleTypeBaseTypesShouldBeComVisible")]
    [CLSCompliant(false), ComVisible(true)]
    public class OAComReference : OAReferenceBase<ComReferenceNode>
    {
        private System.Reflection.Assembly assembly = null;
        private bool tryLoad = false;
        internal OAComReference(ComReferenceNode comReference) :
            base(comReference)
        {
        }

        #region Reference override
        public override string Culture
        {
            get
            {
                int locale = 0;
                try
                {
                    locale = int.Parse(BaseReferenceNode.LCID, CultureInfo.InvariantCulture);
                }
                catch(System.FormatException)
                {
                    // Do Nothing
                }
				if(0 == locale)
				{
                    return "0";
				}
				CultureInfo culture = new CultureInfo(locale);
                return culture.Name;
            }
        }
      public override string Description
      {
         get
         {
            return BaseReferenceNode.Description;
         }
      }
        public override string Identity
        {
            get
            {
                return string.Format(CultureInfo.InvariantCulture, "{0}\\{1}\\{2}\\{3}", BaseReferenceNode.TypeGuid.ToString("B").ToUpper(), this.Version, BaseReferenceNode.LCID, BaseReferenceNode.WrapperTool);
            }
        }
        public override int MajorVersion
        {
            get { return BaseReferenceNode.MajorVersionNumber; }
        }
        public override int MinorVersion
        {
            get { return BaseReferenceNode.MinorVersionNumber; }
        }
        public override string Name
        {
            //get { return BaseReferenceNode.Caption; }
            get
            {
                // this needs to return the name as defined in the assembly
                // Otherwise the form editor will not be able to load a saved activeX control
                // the safest thing to do is to load the assembly and retrieve its name
                if (assembly == null  && ! tryLoad)
                {
                    try
                    {
                        string path = this.Path;
                        tryLoad = true;
                        if (System.IO.File.Exists(path))
                        {
                            assembly = System.Reflection.Assembly.LoadFrom(path);
                        }
                    }
                    catch (Exception)
                    {
                        assembly = null;
                    }
                }
                if (assembly != null)
                {
                    return assembly.GetName().Name;
                }
                return System.IO.Path.GetFileNameWithoutExtension(this.Path); }
        }

        public override VSLangProj.prjReferenceType Type
        {
            get
            {
                return VSLangProj.prjReferenceType.prjReferenceTypeActiveX;
            }
        }
        public override string Version
        {
            get
            {
                Version version = new Version(BaseReferenceNode.MajorVersionNumber, BaseReferenceNode.MinorVersionNumber);
                return version.ToString();
            }
        }
        #endregion
    }
}
