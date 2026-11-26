//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Runtime.InteropServices;

namespace XSharp.Project.Options
{
    /// <summary>
    /// A provider for custom <see cref="DialogPage" /> implementations.
    /// </summary>
    internal class DialogPageProvider
    {
        [ComVisible(true)]
        public class WindowEditor : BaseOptionPage<WindowEditorOptions> { }
        [ComVisible(true)]
        public class OtherEditor : BaseOptionPage<OtherEditorOptions> { }

        [ComVisible(true)]
        public class Debugger : BaseOptionPage<DebuggerOptions> { }
    }
}
