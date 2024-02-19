//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharp.RDD.Enums

begin namespace XSharp.RDD.SqlRDD

	/// <exclude />
	STATIC CLASS ExtensionMethods

    STATIC METHOD IsLong(SELF eType AS DbFieldType ) AS LOGIC
        SWITCH eType
        CASE DbFieldType.Memo
        CASE DbFieldType.Picture
        CASE DbFieldType.General
        CASE DbFieldType.VarBinary
        CASE DbFieldType.Blob
            RETURN TRUE
        END SWITCH
        RETURN FALSE

	END CLASS
END NAMESPACE // XSharp.SQLRdd.SupportClasses
