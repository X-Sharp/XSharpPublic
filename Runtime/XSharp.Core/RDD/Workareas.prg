//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Collections.Generic

BEGIN NAMESPACE XSharp.RDD
CLASS WorkAreas
	// Not static because every thread can have its own workareas structure
	#region Constants
		PUBLIC CONST MaxWorkAreas := 4096 AS LONG
	#endregion
	
	#region Fields
	PRIVATE Aliases  AS Dictionary<STRING, LONG>	// 1 based area numbers !
	PRIVATE RDDs	 AS IRDD[]    
	PRIVATE iCurrentWorkarea as LONG
	PUBLIC LastException AS Exception 

	///<Summary>Get singleton Workareas object for current thread</Summary>
	STATIC METHOD GetInstance() as WorkAreas
		VAR oState	:= XSharp.RuntimeState.GetInstance() 
		VAR oInstance := oState:WorkAreas 
		RETURN oInstance

	#endregion
	CONSTRUCTOR()
		Aliases 			:= Dictionary<STRING, LONG>{MaxWorkAreas}
		RDDs				:= IRDD[]{MaxWorkAreas}   
		iCurrentWorkArea	:= 1
	///<Summary>Convert 1 based Workarea number to 0 based with validation</Summary>
	PRIVATE METHOD AdjustArea( nArea REF LONG) AS LOGIC
		IF  nArea > 0 .and.  nArea <= MaxWorkAreas
			 nArea -=1
			RETURN TRUE
		ENDIF          
		RETURN FALSE
	
	///<Summary>Close All RDDs referenced by this workarea list</Summary>
	PUBLIC METHOD CloseAll() AS LOGIC
		LOCAL lResult := TRUE AS LOGIC
		BEGIN LOCK RDDs      
			LastException := NULL
			FOR VAR i := 0 TO MaxWorkAreas-1
				IF RDDs[i] != NULL
					VAR oRdd := RDDs[i]
					TRY
						lResult := lResult .and. oRdd:Close()
					CATCH e AS Exception
						lResult := FALSE
						LastException := e
					END TRY
				ENDIF              
				RDDs[i] 	:= NULL
			NEXT           
			Aliases:Clear()
		END LOCK                       
		RETURN lResult 

	///<Summary>Commit changes in all workares in this workarea list</Summary>
	PUBLIC METHOD CommitAll() AS LOGIC
		LOCAL lResult := TRUE AS LOGIC
		BEGIN LOCK RDDs      
			LastException := NULL
			FOR VAR i := 0 TO MaxWorkAreas-1
				IF RDDs[i] != NULL
					VAR oRdd := RDDs[i]
					TRY
						lResult := lResult .and. oRdd:Flush()
					CATCH e AS Exception
						lResult := FALSE
						LastException := e
					END TRY
				ENDIF              
			NEXT           
		END LOCK                       
		RETURN lResult 

	///<Summary>Close area with 1 based workarea number</Summary>
	PUBLIC METHOD CloseArea( nArea AS LONG) AS LOGIC
		LOCAL lResult := FALSE AS LOGIC
		IF AdjustArea(REF  nArea)
			BEGIN LOCK RDDs               
				LastException := NULL
				IF RDDs[ nArea] != NULL
					VAR oRdd := RDDs[ nArea]
					TRY
						lResult := oRdd:Close()
						if lResult
							Aliases:Remove(oRdd:Alias)
						endif
					CATCH e AS Exception
						lResult			:= FALSE  
						LastException	:= e
					END TRY
				ENDIF              
				RDDs[ nArea] 		:= NULL
			END LOCK               
		ENDIF
		RETURN lResult   

	///<Summary> Return 1 based Workarea Number for Alias or 0 when no found</Summary>
	PUBLIC METHOD FindAlias(sAlias AS STRING) AS LONG
		sAlias := sAlias:ToUpperInvariant()
		BEGIN LOCK RDDs 
			IF Aliases:ContainsKey(sAlias)
				RETURN Aliases[sAlias]
			ENDIF 
		END LOCK  
		RETURN 0  

	///<Summary> Return 1 based empty Workarea</Summary>
	PUBLIC METHOD FindEmptyArea(fromStart AS LOGIC) AS LONG
		LOCAL i AS LONG
		BEGIN LOCK RDDs                                  
			IF fromStart
				FOR i := 0 TO MaxWorkAreas-1   
					IF RDDs[i] == NULL
						RETURN i+1
					ENDIF
				NEXT
			ELSE 
				FOR i := MaxWorkAreas-1 DOWNTO 0 
					IF RDDs[i] == NULL
						RETURN i+1
					ENDIF
				NEXT
			ENDIF
		END LOCK  
		RETURN 0

	///<Summary>Get Alias for 1 based Workarea Number</Summary>
	PUBLIC METHOD GetAlias( nArea AS LONG) AS STRING
		IF AdjustArea(REF nArea) 
			BEGIN LOCK RDDs
				if RDDs[nArea] != NULL
					RETURN RDDs[nArea]:Alias
				ENDIF
			END LOCK
		ENDIF
		RETURN NULL       
		    
	///<Summary>Get RDD object for 1 based Workarea Number</Summary>
	PUBLIC METHOD GetRDD( nArea AS LONG) AS IRDD
		IF AdjustArea(REF nArea)
			BEGIN LOCK RDDs
				RETURN RDDs[ nArea]
			END LOCK
		ENDIF
		RETURN NULL
		
	///<Summary>Set RDD object and ALias for 1 based Workarea Number</Summary>
	PUBLIC METHOD SetArea( nArea AS LONG, sAlias AS STRING, oRDD AS IRDD) AS LOGIC
		// sAlias and oRdd may be empty (when clearing the RDD)
		IF AdjustArea(REF nArea)
			IF ! String.IsNullOrEmpty(sAlias)
				sAlias := sAlias:ToUpperInvariant()
			ENDIF			
			BEGIN LOCK RDDs
				RDDs[ nArea] 	:= oRDD 
				Aliases:Add(sAlias, nArea+1)
			END LOCK
			RETURN TRUE
		ENDIF          
		RETURN FALSE   

	///<Summary>Get 1 based Current workarea Number</Summary>
	PUBLIC PROPERTY CurrentWorkAreaNO as LONG GET iCurrentWorkArea

	///<Summary>Get Current workarea Object</Summary>
	PUBLIC PROPERTY CurrentWorkArea AS IRDD
		GET                               
			LOCAL  nArea AS LONG
			nArea := iCurrentWorkarea
			IF AdjustArea(REF nArea)
				BEGIN LOCK RDDs
					RETURN RDDs[ nArea]
				END LOCK
			ENDIF
			RETURN NULL					
		END GET
	END PROPERTY
END CLASS
END NAMESPACE
