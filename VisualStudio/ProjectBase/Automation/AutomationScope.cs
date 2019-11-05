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

using EnvDTE;
using System;
using Microsoft.VisualStudio.Shell.Interop;
using System.Diagnostics.CodeAnalysis;
using ErrorHandler = Microsoft.VisualStudio.ErrorHandler;

namespace Microsoft.VisualStudio.Project.Automation
{
    /// <summary>
    /// Helper class that handle the scope of an automation function.
    /// It should be used inside a "using" directive to define the scope of the
    /// automation function and make sure that the ExitAutomation method is called.
    /// </summary>
    internal class AutomationScope : IDisposable
    {
        private IVsExtensibility3 extensibility;
        private bool inAutomation;
        private static volatile object Mutex;
        private bool isDisposed;

        /// <summary>
        /// Initializes the <see cref="AutomationScope"/> class.
        /// </summary>
        [SuppressMessage("Microsoft.Performance", "CA1810:InitializeReferenceTypeStaticFieldsInline")]
        static AutomationScope()
        {
            Mutex = new object();
        }

        /// <summary>
        /// Defines the beginning of the scope of an automation function. This constuctor
        /// calls EnterAutomationFunction to signal the Shell that the current function is
        /// changing the status of the automation objects.
        /// </summary>
        public AutomationScope(IServiceProvider provider)
        {
            Utilities.ArgumentNotNull("provider", provider);

            extensibility = provider.GetService(typeof(IVsExtensibility)) as IVsExtensibility3;
            Assumes.Present(extensibility);
            ErrorHandler.ThrowOnFailure(extensibility.EnterAutomationFunction());
            inAutomation = true;
        }

        /// <summary>
        /// Ends the scope of the automation function. This function is also called by the
        /// Dispose method.
        /// </summary>
        public void ExitAutomation()
        {
            if(inAutomation)
            {
                ErrorHandler.ThrowOnFailure(extensibility.ExitAutomationFunction());
                inAutomation = false;
            }
        }

        /// <summary>
        /// Gets the IVsExtensibility3 interface used in the automation function.
        /// </summary>
        public IVsExtensibility3 Extensibility
        {
            get { return extensibility; }
        }

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        public void Dispose()
        {
            this.Dispose(true);
            GC.SuppressFinalize(this);
        }

        #region IDisposable Members
        private void Dispose(bool disposing)
        {
            if(!this.isDisposed)
            {
                lock(Mutex)
                {
                    if(disposing)
                    {
                        ExitAutomation();
                    }

                    this.isDisposed = true;
                }
            }
        }
        #endregion
    }
}
