//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// The source code in this file enables the "new" Add reference dialog
// The entry point is AddProjectReference

using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
using Microsoft.VisualStudio.Shell;
using System.Globalization;
#pragma warning disable VSTHRD010
namespace XSharp.Project
{
    public partial class XSharpProjectNode
    {

        public override int AddProjectReference()
        {
            ThreadHelper.ThrowIfNotOnUIThread("AddProjectReference");
            var referenceManager = this.GetService(typeof(SVsReferenceManager)) as IVsReferenceManager;
            if (referenceManager != null)
            {
                string title = string.Format(CultureInfo.CurrentCulture, "Reference Manager - {0}", new object[1]
                {
                    Path.GetFileNameWithoutExtension(Url)
                });

                var contextGuids = new[] {
                          VSConstants.AssemblyReferenceProvider_Guid,
                          VSConstants.ProjectReferenceProvider_Guid,
                          //VSConstants.SharedProjectReferenceProvider_Guid,
                          VSConstants.ComReferenceProvider_Guid,
                          VSConstants.FileReferenceProvider_Guid,
                    };
                referenceManager.ShowReferenceManager(
                      this,
                      title,
                      "VS.AddReference",
                      contextGuids.First(),
                      fForceShowDefaultProvider: false
                      );
                return VSConstants.S_OK;
            }
            else
            {
                return VSConstants.E_NOINTERFACE;
            }
        }

        #region IVsReferenceManagerUser Members

        void IVsReferenceManagerUser.ChangeReferences(uint operation, IVsReferenceProviderContext changedContext)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var op = (__VSREFERENCECHANGEOPERATION)operation;
            __VSREFERENCECHANGEOPERATIONRESULT result;

            try
            {
                if (op == __VSREFERENCECHANGEOPERATION.VSREFERENCECHANGEOPERATION_ADD)
                {
                    result = this.AddReferences(changedContext);
                }
                else
                {
                    result = this.RemoveReferences(changedContext);
                }
            }
            catch (InvalidOperationException e)
            {
                Debug.Fail(e.ToString());
                result = __VSREFERENCECHANGEOPERATIONRESULT.VSREFERENCECHANGEOPERATIONRESULT_DENY;
            }

            if (result == __VSREFERENCECHANGEOPERATIONRESULT.VSREFERENCECHANGEOPERATIONRESULT_DENY)
            {
                throw new InvalidOperationException();
            }
        }

        Array IVsReferenceManagerUser.GetProviderContexts()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var mgr = this.GetService(typeof(SVsReferenceManager)) as IVsReferenceManager;
            return this.GetProviderContexts(mgr).ToArray();
        }

        #endregion
        #region IvsReferenceManagerUser implementation
        protected virtual IEnumerable<IVsReferenceProviderContext> GetProviderContexts(IVsReferenceManager mgr)
        {
            var ctxt = CreateAssemblyReferenceProviderContext(mgr);
            if (ctxt != null)
            {
                yield return ctxt;
            }
            ctxt = CreateProjectReferenceProviderContext(mgr);
            if (ctxt != null)
            {
                yield return ctxt;
            }

            //CreateSharedProjectReferenceProviderContext(referenceManager),
            ctxt = CreateCOMReferenceProviderContext(mgr);
            if (ctxt != null)
            {
                yield return ctxt;
            }

            ctxt = CreateFileReferenceProviderContext(mgr);
            if (ctxt != null)
            {
                yield return ctxt;
            }

            //CreatePlatformReferenceProviderContext(referenceManager),

        }
        private IVsReferenceProviderContext CreateAssemblyReferenceProviderContext(IVsReferenceManager mgr)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var context = mgr.CreateProviderContext(VSConstants.AssemblyReferenceProvider_Guid) as IVsAssemblyReferenceProviderContext;
            context.AssemblySearchPaths = this.GetProjectProperty("AssemblySearchPaths");
            context.TargetFrameworkMoniker = this.TargetFrameworkMoniker.ToString();
            var referenceContainer = this.GetReferenceContainer();
            var references = referenceContainer
                 .EnumReferences()
                 .OfType<AssemblyReferenceNode>();
            foreach (var reference in references)
            {
                var newReference = context.CreateReference() as IVsAssemblyReference;
                newReference.FullPath = reference.Url ?? reference.AssemblyName.ToString();
                newReference.Name = reference.AssemblyName.Name;
                context.AddReference(newReference);
            }

            return context;
        }

        private IVsReferenceProviderContext CreateCOMReferenceProviderContext(IVsReferenceManager mgr)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var context = mgr.CreateProviderContext(VSConstants.ComReferenceProvider_Guid) as IVsComReferenceProviderContext;

            var referenceContainer = this.GetReferenceContainer();
            var references = referenceContainer
                 .EnumReferences()
                 .OfType<ComReferenceNode>();
            foreach (var reference in references)
            {
                var newReference = context.CreateReference() as IVsComReference;
                newReference.MajorVersion = (ushort)reference.MajorVersionNumber;
                newReference.MinorVersion = (ushort)reference.MinorVersionNumber;
                newReference.Guid = reference.TypeGuid;
                newReference.FullPath = reference.Url;
                context.AddReference(newReference);
            }

            return context;
        }


        private IVsReferenceProviderContext CreateProjectReferenceProviderContext(IVsReferenceManager mgr)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var context = mgr.CreateProviderContext(VSConstants.ProjectReferenceProvider_Guid) as IVsProjectReferenceProviderContext;
            context.CurrentProject = this;

            var referenceContainer = this.GetReferenceContainer();
            var references = referenceContainer
                 .EnumReferences()
                 .OfType<ProjectReferenceNode>();
            foreach (var reference in references)
            {
                var newReference = context.CreateReference() as IVsProjectReference;
                newReference.FullPath = reference.Url;
                newReference.Identity = reference.ReferencedProjectGuid.ToString("B");
                newReference.ReferenceSpecification= reference.ReferencedProjectGuid.ToString();
                newReference.AlreadyReferenced = true;
                context.AddReference(newReference);
            }

            return context;
        }
        //private IVsReferenceProviderContext CreateSharedProjectReferenceProviderContext(IVsReferenceManager mgr)
        //{
        //    ThreadHelper.ThrowIfNotOnUIThread();
        //    var context = mgr.CreateProviderContext(VSConstants.SharedProjectReferenceProvider_Guid) as IVsSharedProjectReferenceProviderContext;

        //    //var referenceContainer = this.GetReferenceContainer();
        //    //var references = referenceContainer
        //    //     .EnumReferences()
        //    //     .OfType<ProjectReferenceNode>();
        //    //foreach (var reference in references)
        //    //{
        //    //    var newReference = context.CreateReference() as IVsProjectReference;
        //    //    newReference.Identity = reference.ReferencedProjectGuid.ToString("B");
        //    //    newReference.AlreadyReferenced = true;
        //    //    context.AddReference(newReference);
        //    //}

        //    return context as IVsReferenceProviderContext;
        //}
        private IVsReferenceProviderContext CreateFileReferenceProviderContext(IVsReferenceManager mgr)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var context = mgr.CreateProviderContext(VSConstants.FileReferenceProvider_Guid) as IVsFileReferenceProviderContext;

            context.BrowseFilter = AddReferenceExtensions.Replace('|', '\0') + "\0";
            var referenceContainer = this.GetReferenceContainer();
            var references = referenceContainer
                .EnumReferences()
                .Where(n => !(n is AssemblyReferenceNode) && !(n is ProjectReferenceNode));
            foreach (var reference in references)
            {
                var newReference = (IVsFileReference)context.CreateReference();
                newReference.FullPath = reference.Url;
                newReference.AlreadyReferenced = true;
                context.AddReference(newReference);
            }
            return context;
        }


        //private IVsReferenceProviderContext CreatePlatformReferenceProviderContext(IVsReferenceManager mgr)
        //{
        //    IVsPlatformReferenceProviderContext2 vsPlatformReferenceProviderContext = mgr.CreateProviderContext(VSConstants.PlatformReferenceProvider_Guid) as IVsPlatformReferenceProviderContext2;
        //    foreach (SdkReferenceNode current in GetReferenceContainer().EnumReferences().OfType<SdkReferenceNode>())
        //    {
        //        IVsPlatformReference vsPlatformReference = vsPlatformReferenceProviderContext.CreateReference() as IVsPlatformReference;
        //        vsPlatformReference.SDKIdentity = current.Caption;
        //        vsPlatformReference.IsSDK = true;
        //        vsPlatformReference.AlreadyReferenced = true;
        //        vsPlatformReference.FullPath = current.Url;
        //        vsPlatformReferenceProviderContext.AddReference(vsPlatformReference);
        //    }
        //    if (IsImmersive)
        //    {
        //        vsPlatformReferenceProviderContext.TargetFrameworkMoniker = base.TargetFrameworkMoniker.ToString();
        //        vsPlatformReferenceProviderContext.TargetPlatformIdentifier = "UAP";
        //        vsPlatformReferenceProviderContext.TargetPlatformVersion = "10.0.14393.0";
        //        vsPlatformReferenceProviderContext.IsImplicitlyReferenced = true;
        //        vsPlatformReferenceProviderContext.SDKDirectoryRoot = GetSDKRootDirectory();
        //        vsPlatformReferenceProviderContext.SDKExtensionDirectoryRoot = GetUWPSDKRootDirectory();
        //        vsPlatformReferenceProviderContext.SDKFilterKeywords = "WindowsAppContainer WindowsXAML CSharp Managed";
        //        vsPlatformReferenceProviderContext.SDKRegistryRoot = "Software\\Microsoft\\Microsoft SDKs";
        //    }
        //    else
        //    {
        //        vsPlatformReferenceProviderContext.TargetFrameworkMoniker = base.TargetFrameworkMoniker.ToString();
        //        vsPlatformReferenceProviderContext.TargetPlatformIdentifier = "Windows";
        //        vsPlatformReferenceProviderContext.TargetPlatformVersion = "8.0";
        //        vsPlatformReferenceProviderContext.IsImplicitlyReferenced = true;
        //        vsPlatformReferenceProviderContext.VisualStudioVersion = "11.0";
        //        vsPlatformReferenceProviderContext.SDKDirectoryRoot = GetSDKRootDirectory();
        //        vsPlatformReferenceProviderContext.SDKFilterKeywords = "WindowsAppContainer Managed";
        //    }
        //    return vsPlatformReferenceProviderContext;
        //}

        private __VSREFERENCECHANGEOPERATIONRESULT AddReferences(IVsReferenceProviderContext context)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var addedReferences = this.GetAddedReferences(context);

            var referenceContainer = this.GetReferenceContainer();
            foreach (var selectorData in addedReferences)
            {
                referenceContainer.AddReferenceFromSelectorData(selectorData);
            }

            return __VSREFERENCECHANGEOPERATIONRESULT.VSREFERENCECHANGEOPERATIONRESULT_ALLOW;
        }

        protected virtual IEnumerable<VSCOMPONENTSELECTORDATA> GetAddedReferences(IVsReferenceProviderContext context)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (context.ProviderGuid == VSConstants.ProjectReferenceProvider_Guid)
            {
                return GetAddedProjectReferences(context as IVsProjectReferenceProviderContext);
            }
            else if (context.ProviderGuid == VSConstants.FileReferenceProvider_Guid)
            {
                return GetAddedFileReferences(context as IVsFileReferenceProviderContext);
            }
            else if (context.ProviderGuid == VSConstants.AssemblyReferenceProvider_Guid)
            {
                return GetAddedAssemblyReferences(context as IVsAssemblyReferenceProviderContext);
            }
            else if (context.ProviderGuid == VSConstants.ComReferenceProvider_Guid)
            {
                return GetAddedCOMReferences(context as IVsComReferenceProviderContext);
            }
            //else if (context.ProviderGuid == VSConstants.PlatformReferenceProvider_Guid)
            //{
            //    return GetAddedPlatformReferences(context as IVsPlatformReferenceProviderContext);
            //}
            return Enumerable.Empty<VSCOMPONENTSELECTORDATA>();
        }

        private __VSREFERENCECHANGEOPERATIONRESULT RemoveReferences(IVsReferenceProviderContext context)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            var removedReferences = this.GetRemovedReferences(context);

            foreach (var refNode in removedReferences)
            {
                refNode.Remove(true); // delete from storage
            }

            return __VSREFERENCECHANGEOPERATIONRESULT.VSREFERENCECHANGEOPERATIONRESULT_ALLOW;
        }

        protected virtual IEnumerable<ReferenceNode> GetRemovedReferences(IVsReferenceProviderContext context)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var removedReferences = Enumerable.Empty<ReferenceNode>();

            if (context.ProviderGuid == VSConstants.ProjectReferenceProvider_Guid)
            {
                removedReferences = GetRemovedProjectReferences(context as IVsProjectReferenceProviderContext);
            }
            else if (context.ProviderGuid == VSConstants.FileReferenceProvider_Guid)
            {
                removedReferences = GetRemovedFileReferences(context as IVsFileReferenceProviderContext);
            }
            else if (context.ProviderGuid == VSConstants.AssemblyReferenceProvider_Guid)
            {
                removedReferences = GetRemovedAssemblyReferences(context as IVsAssemblyReferenceProviderContext);
            }
            else if (context.ProviderGuid == VSConstants.ComReferenceProvider_Guid)
            {
                removedReferences = GetRemovedCOMReferences(context as IVsComReferenceProviderContext);
            }
            //else if (context.ProviderGuid == VSConstants.PlatformReferenceProvider_Guid)
            //{
            //    removedReferences = GetRemovedPlatformReferences(context as IVsPlatformReferenceProviderContext);
            //}
            return removedReferences;
        }

        private IEnumerable<VSCOMPONENTSELECTORDATA> GetAddedProjectReferences(IVsProjectReferenceProviderContext context)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var selectedReferences = context
                 .References
                 .OfType<IVsProjectReference>()
                 .Select(reference => new VSCOMPONENTSELECTORDATA()
                 {
                     type = VSCOMPONENTTYPE.VSCOMPONENTTYPE_Project,
                     bstrTitle = reference.Name,
                     bstrFile = new FileInfo(reference.FullPath).Directory.FullName,
                     bstrProjRef = reference.ReferenceSpecification,
                 });

            return selectedReferences;
        }

        private IEnumerable<VSCOMPONENTSELECTORDATA> GetAddedAssemblyReferences(IVsAssemblyReferenceProviderContext context)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var selectedReferences = context
                 .References
                 .OfType<IVsAssemblyReference>()
                 .Select(reference => new VSCOMPONENTSELECTORDATA()
                 {
                     type = VSCOMPONENTTYPE.VSCOMPONENTTYPE_ComPlus,
                     bstrTitle = reference.Name,
                     bstrFile = reference.FullPath,
                 });

            return selectedReferences;
        }
        private IEnumerable<VSCOMPONENTSELECTORDATA> GetAddedCOMReferences(IVsComReferenceProviderContext context)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var selectedReferences = context
                 .References
                 .OfType<IVsComReference>()
                 .Select(reference => new VSCOMPONENTSELECTORDATA()
                 {
                     type = VSCOMPONENTTYPE.VSCOMPONENTTYPE_Com2,
                     bstrTitle = reference.Name,
                     bstrFile = reference.FullPath,
                     wFileMajorVersion = reference.MajorVersion,
                     wFileMinorVersion = reference.MinorVersion,
                     guidTypeLibrary = reference.Guid
                 });

            return selectedReferences;
        }


        //private IEnumerable<VSCOMPONENTSELECTORDATA> GetAddedPlatformReferences(IVsPlatformReferenceProviderContext context)
        //{
        //    return context.References.OfType<IVsPlatformReference>().Select(delegate (IVsPlatformReference reference)
        //    {
        //        VSCOMPONENTSELECTORDATA result = default(VSCOMPONENTSELECTORDATA);
        //        result.type = VSCOMPONENTTYPE.VSCOMPONENTTYPE_Custom;
        //        result.bstrTitle = reference.Name;
        //        result.bstrFile = reference.FullPath;
        //        return result;
        //    });
        //}


        private IEnumerable<ReferenceNode> GetRemovedProjectReferences(IVsProjectReferenceProviderContext context)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var selectedReferences = context
                 .References
                 .OfType<IVsProjectReference>()
                 .Select(asmRef => new Guid(asmRef.Identity));

            var referenceContainer = this.GetReferenceContainer();
            var references = referenceContainer
                 .EnumReferences()
                 .OfType<ProjectReferenceNode>()
                 .Where(refNode => selectedReferences.Contains(refNode.ReferencedProjectGuid));

            return references;
        }

        private IEnumerable<VSCOMPONENTSELECTORDATA> GetAddedFileReferences(IVsFileReferenceProviderContext context)
        {
            var selectedReferences = context
                 .References
                 .OfType<IVsFileReference>()
                 .Select(reference => new VSCOMPONENTSELECTORDATA()
                 {
                     type = VSCOMPONENTTYPE.VSCOMPONENTTYPE_File,
                     bstrFile = reference.FullPath,
                 });

            return selectedReferences;
        }

        private IEnumerable<ReferenceNode> GetRemovedFileReferences(IVsFileReferenceProviderContext context)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var selectedReferences = context
                 .References
                 .OfType<IVsFileReference>()
                 .Select(fileRef => fileRef.FullPath);

            var referenceContainer = this.GetReferenceContainer();
            var references = referenceContainer
                 .EnumReferences()
                 .OfType<ReferenceNode>()
                 .Where(refNode => selectedReferences.Contains(refNode.Url));

            return references;
        }


        private IEnumerable<ReferenceNode> GetRemovedAssemblyReferences(IVsAssemblyReferenceProviderContext context)
        {
            IEnumerable<string> selectedReferences = from asmRef in context.References.OfType<IVsAssemblyReference>()
                                                     select asmRef.FullPath;
            return from refNode in GetReferenceContainer().EnumReferences().OfType<AssemblyReferenceNode>()
                   where selectedReferences.Contains(refNode.Url)
                   select refNode;
        }

        private IEnumerable<ReferenceNode> GetRemovedCOMReferences(IVsComReferenceProviderContext context)
        {
            IEnumerable<Guid> selectedReferences = from comRef in context.References.OfType<IVsComReference>()
                                                   select comRef.Guid;
            return from refNode in GetReferenceContainer().EnumReferences().OfType<ComReferenceNode>()
                   where selectedReferences.Contains(refNode.TypeGuid)
                   select refNode;
        }

        //private IEnumerable<ReferenceNode> GetRemovedPlatformReferences(IVsPlatformReferenceProviderContext context)
        //{
        //    IEnumerable<string> selectedReferences = from platformRef in context.References.OfType<IVsPlatformReference>()
        //                                             select platformRef.FullPath;
        //    return from refNode in GetReferenceContainer().EnumReferences().OfType<SdkReferenceNode>()
        //           where selectedReferences.Contains(refNode.Url)
        //           select refNode;
        //}


        protected virtual string AddReferenceExtensions
        {
            get
            {
                return "Assembly files|*.dll|All Files (*.*)|*.*";
            }
        }
  
        #endregion
    }
}
