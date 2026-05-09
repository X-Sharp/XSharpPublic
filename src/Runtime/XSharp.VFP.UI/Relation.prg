// Relation.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// The VFP compatible Relation class (DataEnvironment child).
	/// </summary>
	PARTIAL CLASS Relation
		/// <summary>
		/// The alias of the parent cursor that owns this relation.
		/// </summary>
		PROPERTY ParentAlias AS STRING AUTO
	END CLASS
END NAMESPACE
