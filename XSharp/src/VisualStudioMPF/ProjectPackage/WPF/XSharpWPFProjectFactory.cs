//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio.Shell.Flavor;

namespace XSharp.Project.WPF
{
    [Guid(XSharpConstants.WPFProjectFactory)]
    public class XSharpWPFProjectFactory : FlavoredProjectFactoryBase
    {
        private IServiceProvider _site;

        public XSharpWPFProjectFactory(IServiceProvider site)
        {
            _site = site;
        }

        /// <summary>
        /// Create an instance of our project. The initialization will be done later
        /// when VS calls InitalizeForOuter on it.
        /// </summary>
        /// <param name="outerProjectIUnknown">This is only useful if someone else is subtyping us</param>
        /// <returns>An uninitialized instance of our project</returns>
        protected override object PreCreateForOuter(IntPtr outerProjectIUnknown)
        {
            return new XSharpWPFFlavor(_site);
        }
    }
}

