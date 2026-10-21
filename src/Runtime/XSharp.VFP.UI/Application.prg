// Application.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.



USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// VFP-compatible <c>Application</c> object (VFP <c>_VFP</c> global).<br/>
	/// Currently provides <see cref="StartMode"/> as a stub. Intended as an extension point for
	/// future VFP application-level properties (<c>Caption</c>, <c>Visible</c>, <c>DataSession</c>, etc.).
	/// </summary>
	CLASS Application

		CONSTRUCTOR()
			RETURN

		/// <summary>
		/// VFP StartMode stub — intended to reflect how the application was started (0=IDE, 1=runtime, etc.). Stored for source compatibility.
		/// </summary>
		PROPERTY StartMode AS INT AUTO

	END CLASS
END NAMESPACE // XSharp.VFP.UI
