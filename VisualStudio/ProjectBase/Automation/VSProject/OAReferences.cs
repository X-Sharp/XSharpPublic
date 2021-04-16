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
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using VSLangProj;
using ErrorHandler = Microsoft.VisualStudio.ErrorHandler;

namespace Microsoft.VisualStudio.Project.Automation
{
    /// <summary>
    /// Represents the automation object for the equivalent ReferenceContainerNode object
    /// </summary>
    [SuppressMessage("Microsoft.Naming", "CA1710:IdentifiersShouldHaveCorrectSuffix")]
    [CLSCompliant(false), ComVisible(true)]
    public class OAReferences : ConnectionPointContainer,
                                IEventSource<_dispReferencesEvents>,
                                References,
                                ReferencesEvents
    {
        private readonly ProjectNode _project;
        private ReferenceContainerNode _container;

        /// <summary>
        /// Creates a new automation references object.  If the project type doesn't
        /// support references containerNode is null.
        /// </summary>
        /// <param name="containerNode"></param>
        /// <param name="project"></param>
        internal OAReferences(ReferenceContainerNode containerNode, ProjectNode project)
        {
            _container = containerNode;
            _project = project;

            AddEventSource<_dispReferencesEvents>(this as IEventSource<_dispReferencesEvents>);
            if (_container != null) {
                _container.OnChildAdded += new EventHandler<HierarchyNodeEventArgs>(OnReferenceAdded);
                _container.OnChildRemoved += new EventHandler<HierarchyNodeEventArgs>(OnReferenceRemoved);
            }
        }

        #region Private Members
        private Reference AddFromSelectorData(VSCOMPONENTSELECTORDATA selector, string wrapperTool = null)
        {
            if (_container == null) {
                return null;
            }
            ThreadHelper.ThrowIfNotOnUIThread();

            ReferenceNode refNode = _container.AddReferenceFromSelectorData(selector, wrapperTool);
            if(null == refNode)
            {
                return null;
            }

            return refNode.Object as Reference;
        }

        private Reference FindByName(string stringIndex)
        {
            foreach(Reference refNode in this)
            {
                if (0 == String.Compare(refNode.Name, stringIndex, StringComparison.Ordinal))
                {
                    return refNode;
                }
            }
            return null;
        }
        #endregion

        #region References Members

        public Reference Add(string bstrPath)
        {
            if (String.IsNullOrEmpty(bstrPath))
            {
                return null;
            }
            ThreadHelper.ThrowIfNotOnUIThread();

            VSCOMPONENTSELECTORDATA selector = new VSCOMPONENTSELECTORDATA();
            selector.type = VSCOMPONENTTYPE.VSCOMPONENTTYPE_File;
            selector.bstrFile = bstrPath;

            return AddFromSelectorData(selector);
        }

        public Reference AddActiveX(string bstrTypeLibGuid, int lMajorVer, int lMinorVer, int lLocaleId, string bstrWrapperTool)
        {
            VSCOMPONENTSELECTORDATA selector = new VSCOMPONENTSELECTORDATA();
            selector.type = VSCOMPONENTTYPE.VSCOMPONENTTYPE_Com2;
            selector.guidTypeLibrary = new Guid(bstrTypeLibGuid);
            selector.lcidTypeLibrary = (uint)lLocaleId;
            selector.wTypeLibraryMajorVersion = (ushort)lMajorVer;
            selector.wTypeLibraryMinorVersion = (ushort)lMinorVer;
            ThreadHelper.ThrowIfNotOnUIThread();

            return AddFromSelectorData(selector, bstrWrapperTool);
        }

        public Reference AddProject(EnvDTE.Project project)
        {
            if (null == project || _container == null)
            {
                return null;
            }
            ThreadHelper.ThrowIfNotOnUIThread();
            // Get the solution.
            IVsSolution solution = _container.ProjectMgr.Site.GetService(typeof(SVsSolution)) as IVsSolution;
            if(null == solution)
            {
                return null;
            }

            // Get the hierarchy for this project.
            IVsHierarchy projectHierarchy;
            ErrorHandler.ThrowOnFailure(solution.GetProjectOfUniqueName(project.UniqueName, out projectHierarchy));

            // Create the selector data.
            VSCOMPONENTSELECTORDATA selector = new VSCOMPONENTSELECTORDATA();
            selector.type = VSCOMPONENTTYPE.VSCOMPONENTTYPE_Project;

            // Get the project reference string.
            ErrorHandler.ThrowOnFailure(solution.GetProjrefOfProject(projectHierarchy, out selector.bstrProjRef));

            selector.bstrTitle = project.Name;
            selector.bstrFile = System.IO.Path.GetDirectoryName(project.FullName);

            return AddFromSelectorData(selector);
        }

        public EnvDTE.Project ContainingProject
        {
            get
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                return _project.GetAutomationObject() as EnvDTE.Project;
            }
        }

        public int Count
        {
            get
            {
                if (_container == null)
                {
                    return 0;
                }
                return _container.EnumReferences().Count;
            }
        }

        public EnvDTE.DTE DTE
        {
            get
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                return _project.Site.GetService(typeof(EnvDTE.DTE)) as EnvDTE.DTE;
            }
        }

        public Reference Find(string bstrIdentity)
        {
            if (String.IsNullOrEmpty(bstrIdentity))
            {
                return null;
            }
            foreach(Reference refNode in this)
            {
                if(null != refNode)
                {
                    if (0 == String.Compare(bstrIdentity, refNode.Identity, StringComparison.Ordinal))
                    {
                        return refNode;
                    }
                }
            }
            return null;
        }

        public IEnumerator GetEnumerator()
        {
            if (_container == null) {
                return new List<Reference>().GetEnumerator();
            }

            List<Reference> references = new List<Reference>();
            IEnumerator baseEnum = _container.EnumReferences().GetEnumerator();
            if(null == baseEnum)
            {
                return references.GetEnumerator();
            }
            while(baseEnum.MoveNext())
            {
                ReferenceNode refNode = baseEnum.Current as ReferenceNode;
                if(null == refNode)
                {
                    continue;
                }
                Reference reference = refNode.Object as Reference;
                if(null != reference)
                {
                    references.Add(reference);
                }
            }
            return references.GetEnumerator();
        }

        public Reference Item(object index)
        {
            Utilities.ArgumentNotNull("index", index);

            string stringIndex = index as string;
            if(null != stringIndex)
            {
                return FindByName(stringIndex);
            }
            // Note that this cast will throw if the index is not convertible to int.
            int intIndex = (int)index;
            IList<ReferenceNode> refs = _container.EnumReferences();
            if(null == refs)
            {
                throw new ArgumentOutOfRangeException();
            }
            if((intIndex <= 0) || (intIndex > refs.Count))
            {
                throw new ArgumentOutOfRangeException();
            }
            // Let the implementation of IList<> throw in case of index not correct.
            return refs[intIndex - 1].Object as Reference;
        }

        public object Parent
        {
            get
            {
                if (_container == null)
                {
                    return _project.Object;
                }
                return _container.Parent.Object;
            }
        }

        #endregion

        #region _dispReferencesEvents_Event Members
        public event _dispReferencesEvents_ReferenceAddedEventHandler ReferenceAdded;
        public event _dispReferencesEvents_ReferenceChangedEventHandler ReferenceChanged;
        public event _dispReferencesEvents_ReferenceRemovedEventHandler ReferenceRemoved;
        #endregion

        #region Callbacks for the HierarchyNode events
        private void OnReferenceAdded(object sender, HierarchyNodeEventArgs args)
        {
            // Validate the parameters.
            if((_container != sender as ReferenceContainerNode) ||
                (null == args) || (null == args.Child))
            {
                return;
            }

            // Check if there is any sink for this event.
            if(null == ReferenceAdded)
            {
                return;
            }

            // Check that the removed item implements the Reference interface.
            Reference reference = args.Child.Object as Reference;
            if(null != reference)
            {
                ReferenceAdded(reference);
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode",
            Justification="Support for this has not yet been added")]
        private void OnReferenceChanged(object sender, HierarchyNodeEventArgs args)
        {
            // Validate the parameters.
            if ((_container != sender as ReferenceContainerNode) ||
                (null == args) || (null == args.Child))
            {
                return;
            }

            // Check if there is any sink for this event.
            if (null == ReferenceChanged)
            {
                return;
            }

            // Check that the removed item implements the Reference interface.
            Reference reference = args.Child.Object as Reference;
            if (null != reference)
            {
                ReferenceChanged(reference);
            }
        }

        private void OnReferenceRemoved(object sender, HierarchyNodeEventArgs args)
        {
            // Validate the parameters.
            if((_container != sender as ReferenceContainerNode) ||
                (null == args) || (null == args.Child))
            {
                return;
            }

            // Check if there is any sink for this event.
            if(null == ReferenceRemoved)
            {
                return;
            }

            // Check that the removed item implements the Reference interface.
            Reference reference = args.Child.Object as Reference;
            if(null != reference)
            {
                ReferenceRemoved(reference);
            }
        }
        #endregion

        #region IEventSource<_dispReferencesEvents> Members
        void IEventSource<_dispReferencesEvents>.OnSinkAdded(_dispReferencesEvents sink)
        {
            ReferenceAdded += new _dispReferencesEvents_ReferenceAddedEventHandler(sink.ReferenceAdded);
            ReferenceChanged += new _dispReferencesEvents_ReferenceChangedEventHandler(sink.ReferenceChanged);
            ReferenceRemoved += new _dispReferencesEvents_ReferenceRemovedEventHandler(sink.ReferenceRemoved);
        }

        void IEventSource<_dispReferencesEvents>.OnSinkRemoved(_dispReferencesEvents sink)
        {
            ReferenceAdded -= new _dispReferencesEvents_ReferenceAddedEventHandler(sink.ReferenceAdded);
            ReferenceChanged -= new _dispReferencesEvents_ReferenceChangedEventHandler(sink.ReferenceChanged);
            ReferenceRemoved -= new _dispReferencesEvents_ReferenceRemovedEventHandler(sink.ReferenceRemoved);
        }
        #endregion
    }
}
