//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

// Class ListBox BaseClass   Listbox Class  Listbox
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS ListBox IMPLEMENTS IVFPList, IVFPGraphics
#include "VFPList.xh"
#include "VFPItems.xh"
		PROPERTY ColumnCount  AS LONG AUTO
		PROPERTY ColumnLines  AS LOGIC AUTO
		PROPERTY ColumnWidths AS STRING AUTO
		PROPERTY FontOutline  AS LOGIC AUTO
		PROPERTY FontShadow   AS LOGIC AUTO
	END CLASS
END NAMESPACE
