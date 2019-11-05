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
using System.Globalization;
using System.IO;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using System.Threading;
using Microsoft.VisualStudio.Shell;

namespace Microsoft.VisualStudio.Project
{
    /// <summary>
    /// Defines abstract package.
    /// </summary>
    [ComVisible(true)]
    [CLSCompliant(false)]
    public abstract class AsyncProjectPackage : Microsoft.VisualStudio.Shell.AsyncPackage
    {
        #region fields
        /// <summary>
        /// This is the place to register all the solution listeners.
        /// </summary>
        private List<SolutionListener> solutionListeners = new List<SolutionListener>();
        #endregion

        #region properties
        /// <summary>
        /// Add your listener to this list. They should be added in the overridden Initialize befaore calling the base.
        /// </summary>
        internal IList<SolutionListener> SolutionListeners
        {
            get
            {
                return this.solutionListeners;
            }
        }

        public abstract string ProductUserContext { get; }
        #endregion

        #region ctor
        protected AsyncProjectPackage()
        {
        }

        #endregion

        #region methods
        protected override async System.Threading.Tasks.Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
            await base.InitializeAsync( cancellationToken, progress );

            // Subscribe to the solution events
            this.solutionListeners.Add(new SolutionListenerForProjectReferenceUpdate(this));
            this.solutionListeners.Add(new SolutionListenerForProjectOpen(this));
            this.solutionListeners.Add(new SolutionListenerForBuildDependencyUpdate(this));
            this.solutionListeners.Add(new SolutionListenerForProjectEvents(this));

            foreach(SolutionListener solutionListener in this.solutionListeners)
            {
                solutionListener.Init();
            }
        }

        protected override void Dispose(bool disposing)
        {
            // Unadvise solution listeners.
            try
            {
                if(disposing)
                {
                    foreach(SolutionListener solutionListener in this.solutionListeners)
                    {
                        solutionListener.Dispose();
                    }

                    // Dispose the UIThread singleton.
                    UIThread.Instance.Dispose();
                }
            }
            finally
            {

                base.Dispose(disposing);
            }
        }

        /// <summary>
        /// Called by the base package to load solution options.
        /// </summary>
        /// <param name="key">Name of the stream.</param>
        /// <param name="stream">The stream from ehere the pachage should read user specific options.</param>
        protected override void OnLoadOptions(string key, Stream stream)
        {
            // Check if the .suo file is safe, i.e. created on this computer
            // This should really go on the Package.cs
            IVsSolution solution = this.GetService(typeof(SVsSolution)) as IVsSolution;

            if (solution != null)
            {
                object valueAsBool;
                int result = solution.GetProperty((int)__VSPROPID2.VSPROPID_SolutionUserFileCreatedOnThisComputer, out valueAsBool);

                if (ErrorHandler.Failed(result) || !(bool)valueAsBool)
                {
                    return;
                }
            }

            base.OnLoadOptions(key, stream);
        }

        /// <summary>
        /// Called by the base package when the solution save the options
        /// </summary>
        /// <param name="key">Name of the stream.</param>
        /// <param name="stream">The stream from ehere the pachage should read user specific options.</param>
        protected override void OnSaveOptions(string key, Stream stream)
        {
            base.OnSaveOptions(key, stream);
        }
        #endregion
    }
}
