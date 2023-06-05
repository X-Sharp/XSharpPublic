//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Runtime.InteropServices
USING System.IO
USING System.Text
USING System.Linq
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.Globalization
USING System.Collections.Generic


BEGIN NAMESPACE XSharp.RDD
    PARTIAL CLASS DBF

    	/// <inheritdoc />
		OVERRIDE METHOD BlobInfo(uiPos AS DWORD, uiOrdinal AS DWORD) AS OBJECT
            RETURN SUPER:BlobInfo(uiPos, uiOrdinal)


        /// <inheritdoc />
		OVERRIDE METHOD ClearRel() AS LOGIC
            RETURN SUPER:ClearRel()

         /// <inheritdoc />
        OVERRIDE METHOD ClearScope() 	AS LOGIC
            RETURN SUPER:ClearScope()
         /// <inheritdoc />
        OVERRIDE METHOD Continue()		AS LOGIC
            RETURN SUPER:Continue()



         /// <inheritdoc />
        OVERRIDE METHOD DbEval(info AS DbEvalInfo) AS LOGIC
            RETURN SUPER:DbEval(info)


         /// <inheritdoc />
        OVERRIDE METHOD FieldIndex(fieldName AS STRING) AS LONG
            RETURN SUPER:FieldIndex(fieldName)

			/// <inheritdoc />
		OVERRIDE METHOD FieldName(nFldPos AS LONG) AS STRING
			RETURN SUPER:FieldName( nFldPos )



          /// <inheritdoc />
        OVERRIDE METHOD GetScope()		AS DbScopeInfo
            RETURN SUPER:GetScope()


		/// <inheritdoc />
		OVERRIDE METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
            RETURN SUPER:OrderCondition(info)


		/// <inheritdoc />
		OVERRIDE METHOD RelEval(info AS DbRelInfo) AS LOGIC
            RETURN SUPER:RelEval(info)
		/// <inheritdoc />
		OVERRIDE METHOD RelText(nRelNum AS DWORD) AS STRING
            RETURN SUPER:RelText(nRelNum)

		/// <inheritdoc />
		OVERRIDE METHOD SetRel(info AS DbRelInfo) AS LOGIC
            RETURN SUPER:SetRel(info)



         /// <inheritdoc />
        OVERRIDE METHOD SkipFilter(nToSkip AS INT) AS LOGIC
            RETURN SUPER:SkipFilter(nToSkip)

         /// <inheritdoc />
        OVERRIDE METHOD SkipScope(nToSkip AS INT) AS LOGIC
            RETURN SUPER:SkipScope(nToSkip)



         /// <inheritdoc />
        OVERRIDE METHOD SetScope(info AS DbScopeInfo) AS LOGIC
            RETURN SUPER:SetScope(info)

		/// <inheritdoc />
		OVERRIDE METHOD Trans(info AS DbTransInfo) 		AS LOGIC
            RETURN SUPER:Trans(info)


    END CLASS
END NAMESPACE
