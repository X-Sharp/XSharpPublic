//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio.Shell.Flavor;
using Microsoft.VisualStudio.Shell.Interop;


namespace XSharp.Project.WPF
{
    [ComVisible(true)]
    [ClassInterface(ClassInterfaceType.None)]
    [Guid(XSharpConstants.WPFFlavor)]
    public class XSharpWPFFlavor : FlavoredProjectBase
    {
        public XSharpWPFFlavor(IServiceProvider site)
        {
            this.serviceProvider = site;
        }

        protected override Guid GetGuidProperty(uint itemId, int propId)
        {
            if (propId == (int)__VSHPROPID2.VSHPROPID_AddItemTemplatesGuid)
                return typeof(XSharpWPFProjectFactory).GUID;
            if (propId == (int)__VSHPROPID.VSHPROPID_PreferredLanguageSID)
                return new Guid(Constants.XSharpLanguageString);

            return base.GetGuidProperty(itemId, propId);
        }

        protected override int GetProperty(uint itemId, int propId, out object property)
        {
            return base.GetProperty(itemId, propId, out property);
        }
    }
}

