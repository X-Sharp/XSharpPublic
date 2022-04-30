//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Debugger;
using Microsoft.VisualStudio.Debugger.Clr;
using Microsoft.VisualStudio.Debugger.Evaluation;
using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;


namespace XSharpDebugger
{
    /// <summary>
    /// This class is our representation of the inspection session.  We add this as a data item to
    /// the debug engine's DkmInspectionContext.  When the user steps or continues the process, the
    /// debug engine disposes of the DkmInspectionContext and our inspection session along with it.
    /// This allows us to tie the lifetime of our objects to lifetime of the inspection session.
    /// </summary>
    internal sealed class InspectionSession : DkmDataItem, IDisposable
    {
        public readonly Importer Importer = new Importer();

        private Dictionary<DkmClrInstructionAddress, InspectionScope> _scopes;

        public InspectionSession()
        {
            XSharpType.CheckCase();
            _scopes = new Dictionary<DkmClrInstructionAddress, InspectionScope>(AddressComparer.Instance);
        }

        protected override void OnClose()
        {
            Dispose();
        }

        public void Dispose()
        {
            Importer.Dispose();
        }

        public static InspectionSession GetInstance(DkmInspectionSession dkmObject)
        {
            InspectionSession session = dkmObject.GetDataItem<InspectionSession>();
            if (session == null)
            {
                session = new InspectionSession();
                dkmObject.SetDataItem(DkmDataCreationDisposition.CreateNew, session);
            }

            return session;
        }

        public InspectionScope GetScope(DkmClrInstructionAddress address)
        {
            // Cache the various scopes used during the inspection session.  Different scopes are
            // used when the user selects different frames and when the debug engine asks us to
            // format each stack frame.
            InspectionScope scope;
            if (!_scopes.TryGetValue(address, out scope))
            {
                scope = new InspectionScope(address, this);
                _scopes.Add(address, scope);
            }

            return scope;
        }
    }
}
