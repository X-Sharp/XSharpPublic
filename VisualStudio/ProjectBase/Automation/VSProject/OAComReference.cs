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
using System.Reflection;

namespace Microsoft.VisualStudio.Project.Automation
{
    [SuppressMessage("Microsoft.Interoperability", "CA1405:ComVisibleTypeBaseTypesShouldBeComVisible")]
    [CLSCompliant(false), ComVisible(true)]
    public class OAComReference : OAReferenceBase<ComReferenceNode>
    {
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

        public override bool SpecificVersion
        {
            get
            {
                var data = this.BaseReferenceNode.ItemNode.GetMetadata(ProjectFileConstants.SpecificVersion);
                if (String.IsNullOrEmpty(data))
                    return false;
                return string.Compare(data, "True", true) == 0;

            }
        }

        public override bool Isolated
        {
            get
            {
                var data = this.BaseReferenceNode.ItemNode.GetMetadata(ProjectFileConstants.Isolated);
                if (String.IsNullOrEmpty(data))
                {
                    return false;
                }
                else
                {
                    return string.Compare(data, "True", true) == 0;
                }
            }

        }
        public override string RuntimeVersion
        {
            get
            {
                return "";
            }
        }
        #endregion
    }
}
