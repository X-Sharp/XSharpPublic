//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using IServiceProvider = System.IServiceProvider;

using Microsoft.VisualStudio.Project;
namespace XSharp.Project
{
    /// <summary>
    /// This class is used to suspend and resume the background scanner upon certain Solution Events
    /// </summary>
    class ModelScannerEvents : SolutionListener
    {
        #region ctors
        public ModelScannerEvents(IServiceProvider serviceProvider)
            : base(serviceProvider)
        {
            XSharpModel.ModelWalker.Suspend();
        }

        /// <summary>
        /// Called at load time when solution has finished opening.
        /// </summary>
        /// <param name="pUnkReserved">reserved</param>
        /// <param name="fNewSolution">true if this is a new solution</param>
        /// <returns></returns>
        public override int OnAfterOpenSolution(object pUnkReserved, int fNewSolution)
        {
            // Restart scanning. Was suspended on opening of project system
            // or closing of previous solution
            XSharpModel.ModelWalker.Resume();
            return VSConstants.S_OK;
        }

        public override int OnBeforeCloseSolution(object pUnkReserved)
        {
            XSharpModel.ModelWalker.Suspend();
            return VSConstants.S_OK;
        }
        public override int OnAfterCloseSolution(object reserved)
        {
            XSharpModel.XSolution.CloseAll();
            return VSConstants.S_OK;
        }
        #endregion

    }
}

