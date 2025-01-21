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
using System.IO;
using Microsoft.VisualStudio.Shell;
#pragma warning disable VSTHRD010
namespace XSharp.Project
{
    internal class ProjectNodeReferenceManager : IVsReferenceManagerUser
    {
        readonly XSharpProjectNode projectNode;
        internal ProjectNodeReferenceManager (XSharpProjectNode projectNode)
        {
            this.projectNode = projectNode;
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
                throw;
            }

            if (result == __VSREFERENCECHANGEOPERATIONRESULT.VSREFERENCECHANGEOPERATIONRESULT_DENY)
            {
                throw new InvalidOperationException();
            }
        }

        Array IVsReferenceManagerUser.GetProviderContexts()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var mgr = projectNode.GetService(typeof(SVsReferenceManager)) as IVsReferenceManager;
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

            //ctxt = CreateSharedProjectReferenceProviderContext(mgr);
            //if (ctxt != null)
            //{
            //    yield return ctxt;
            //}

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
            context.AssemblySearchPaths = projectNode.GetProjectProperty("AssemblySearchPaths");
            context.TargetFrameworkMoniker = projectNode.TargetFrameworkMoniker.ToString();
            var referenceContainer = projectNode.GetReferenceContainer();
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

            var referenceContainer = projectNode.GetReferenceContainer();
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
            context.CurrentProject = projectNode;

            var referenceContainer = projectNode.GetReferenceContainer();
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
        private IVsReferenceProviderContext CreateFileReferenceProviderContext(IVsReferenceManager mgr)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var context = mgr.CreateProviderContext(VSConstants.FileReferenceProvider_Guid) as IVsFileReferenceProviderContext;

            context.BrowseFilter = AddReferenceExtensions.Replace('|', '\0') + "\0";
            var referenceContainer = projectNode.GetReferenceContainer();
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


        private __VSREFERENCECHANGEOPERATIONRESULT AddReferences(IVsReferenceProviderContext context)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var addedReferences = this.GetAddedReferences(context);

            var referenceContainer = projectNode.GetReferenceContainer();
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
                removedReferences = GetRemovedProjectReferences(context);
            }
            else if (context.ProviderGuid == VSConstants.FileReferenceProvider_Guid)
            {
                removedReferences = GetRemovedFileReferences(context);
            }
            else if (context.ProviderGuid == VSConstants.AssemblyReferenceProvider_Guid)
            {
                removedReferences = GetRemovedAssemblyReferences(context);
            }
            else if (context.ProviderGuid == VSConstants.ComReferenceProvider_Guid)
            {
                removedReferences = GetRemovedCOMReferences(context);
            }
            return removedReferences;
        }

        private IEnumerable<VSCOMPONENTSELECTORDATA> GetAddedProjectReferences(IVsReferenceProviderContext context)
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

        private IEnumerable<VSCOMPONENTSELECTORDATA> GetAddedAssemblyReferences(IVsReferenceProviderContext context)
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
        private IEnumerable<VSCOMPONENTSELECTORDATA> GetAddedCOMReferences(IVsReferenceProviderContext context)
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

        private IEnumerable<ReferenceNode> GetRemovedProjectReferences(IVsReferenceProviderContext context)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var selectedReferences = context
                 .References
                 .OfType<IVsProjectReference>()
                 .Select(asmRef => new Guid(asmRef.Identity));

            var referenceContainer = projectNode.GetReferenceContainer();
            var references = referenceContainer
                 .EnumReferences()
                 .OfType<ProjectReferenceNode>()
                 .Where(refNode => selectedReferences.Contains(refNode.ReferencedProjectGuid));

            return references;
        }

        private IEnumerable<VSCOMPONENTSELECTORDATA> GetAddedFileReferences(IVsReferenceProviderContext context)
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

        private IEnumerable<ReferenceNode> GetRemovedFileReferences(IVsReferenceProviderContext context)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var selectedReferences = context
                 .References
                 .OfType<IVsFileReference>()
                 .Select(fileRef => fileRef.FullPath);

            var referenceContainer = projectNode.GetReferenceContainer();
            var references = referenceContainer
                 .EnumReferences()
                 .OfType<ReferenceNode>()
                 .Where(refNode => selectedReferences.Contains(refNode.Url));

            return references;
        }


        private IEnumerable<ReferenceNode> GetRemovedAssemblyReferences(IVsReferenceProviderContext context)
        {
            IEnumerable<string> selectedReferences = from asmRef in context.References.OfType<IVsAssemblyReference>()
                                                     select asmRef.FullPath;
            return from refNode in projectNode.GetReferenceContainer().EnumReferences().OfType<AssemblyReferenceNode>()
                   where selectedReferences.Contains(refNode.Url)
                   select refNode;
        }

        private IEnumerable<ReferenceNode> GetRemovedCOMReferences(IVsReferenceProviderContext context)
        {
            IEnumerable<Guid> selectedReferences = from comRef in context.References.OfType<IVsComReference>()
                                                   select comRef.Guid;
            return from refNode in projectNode.GetReferenceContainer().EnumReferences().OfType<ComReferenceNode>()
                   where selectedReferences.Contains(refNode.TypeGuid)
                   select refNode;
        }

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
