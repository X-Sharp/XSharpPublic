// Relation.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// VFP-compatible <c>Relation</c> object, a child of <c>DataEnvironment</c>.<br/>
	/// Describes a parent–child table relationship. The <see cref="ParentAlias"/> property
	/// identifies the parent cursor; the generated <c>DataEnvironment.Init</c> loop iterates
	/// the Relation objects and calls <c>DbSetRelation</c> / <c>DbRelation</c> to wire them up.
	/// </summary>
	PARTIAL CLASS Relation
		/// <summary>
		/// The alias of the parent cursor that owns this relation.
		/// </summary>
		PROPERTY ParentAlias AS STRING AUTO
	END CLASS
END NAMESPACE
