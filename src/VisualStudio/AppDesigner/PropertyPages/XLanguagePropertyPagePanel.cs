//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Project;

namespace XSharp.Project
{
    /// <summary>
    /// Property page contents for the Candle Settings page.
    /// </summary>
    internal partial class XLanguagePropertyPagePanel : XPropertyPagePanel
    {
        public XLanguagePropertyPagePanel() : base()
        {
        }

        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventsPropertyPagePanel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The parent property page to which this is bound.</param>
        public XLanguagePropertyPagePanel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {

        }
    }
}
