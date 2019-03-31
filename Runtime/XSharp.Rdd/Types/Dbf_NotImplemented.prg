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
		METHOD BlobInfo(uiPos AS DWORD, uiOrdinal AS DWORD) AS OBJECT
            RETURN SUPER:BlobInfo(uiPos, uiOrdinal)

    
        /// <inheritdoc />
        METHOD ClearFilter() 	AS LOGIC
            RETURN SUPER:ClearFilter()

        /// <inheritdoc />
		METHOD ClearRel() AS LOGIC
            RETURN SUPER:ClearRel()

         /// <inheritdoc />
        METHOD ClearScope() 	AS LOGIC
            RETURN SUPER:ClearScope()
         /// <inheritdoc />
        METHOD Continue()		AS LOGIC
            RETURN SUPER:Continue()
            
         /// <inheritdoc />
        METHOD CreateFields(aFields AS RddFieldInfo[]) AS LOGIC
            RETURN SUPER:CreateFields(aFields)
            
         /// <inheritdoc />
        METHOD DbEval(info AS DbEvalInfo) AS LOGIC
            RETURN SUPER:DbEval(info)
            
            
         /// <inheritdoc />
        METHOD FieldIndex(fieldName AS STRING) AS LONG
            RETURN SUPER:FieldIndex(fieldName)
            
			/// <inheritdoc />
		METHOD FieldName(nFldPos AS LONG) AS STRING
			RETURN SUPER:FieldName( nFldPos )
			


          /// <inheritdoc />
        METHOD GetScope()		AS DbScopeInfo
            RETURN SUPER:GetScope()
           

		/// <inheritdoc />
		METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
            RETURN SUPER:OrderCondition(info)


		/// <inheritdoc />
		METHOD RelEval(info AS DbRelInfo) AS LOGIC
            RETURN SUPER:RelEval(info)
		/// <inheritdoc />
		METHOD RelText(nRelNum AS DWORD) AS STRING
            RETURN SUPER:RelText(nRelNum)

		/// <inheritdoc />
		METHOD SetRel(info AS DbRelInfo) AS LOGIC
            RETURN SUPER:SetRel(info)

	
            
         /// <inheritdoc />
        METHOD SkipFilter(nToSkip AS INT) AS LOGIC
            RETURN SUPER:SkipFilter(nToSkip)
            
         /// <inheritdoc />
        METHOD SkipScope(nToSkip AS INT) AS LOGIC
            RETURN SUPER:SkipScope(nToSkip)            
            
            
        
         /// <inheritdoc />
        METHOD SetScope(info AS DbScopeInfo) AS LOGIC
            RETURN SUPER:SetScope(info)

		/// <inheritdoc />
		METHOD Trans(info AS DbTransInfo) 		AS LOGIC
            RETURN SUPER:Trans(info)
        
		/// <inheritdoc />
    	METHOD TransRec(info AS DbTransInfo) 	AS LOGIC
            RETURN SUPER:TransRec(info)

            
    END CLASS
END NAMESPACE
