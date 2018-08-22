//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Collections.Generic

BEGIN NAMESPACE XSharp.RDD
/// <summary>Class that contains the list of open workareas. Each thread will have its own list.</summary>
CLASS WorkAreas
	// Not static because every thread can have its own workareas structure
	#region Constants
		PUBLIC CONST MaxWorkAreas := 4096 AS LONG
	#endregion
	
	#region Fields
	PRIVATE Aliases  AS Dictionary<STRING, LONG>	// 1 based area numbers !
	PRIVATE RDDs	 AS IRDD[]    
	PRIVATE iCurrentWorkarea AS LONG
	PUBLIC LastException AS Exception 

	///<summary>Get singleton Workareas object for current thread</summary>
	STATIC METHOD GetInstance() AS WorkAreas
		VAR oState	:= XSharp.RuntimeState.GetInstance() 
		VAR oInstance := oState:WorkAreas 
		RETURN oInstance

	#endregion
	CONSTRUCTOR()
		Aliases 			:= Dictionary<STRING, LONG>{MaxWorkAreas}
		RDDs				:= IRDD[]{MaxWorkAreas}   
		iCurrentWorkArea	:= 1

	///<summary>Convert 1 based Workarea number to 0 based with validation</summary>
	PRIVATE METHOD AdjustArea( nArea REF LONG) AS LOGIC
		IF  nArea > 0 .AND.  nArea <= MaxWorkAreas
			 nArea -=1
			RETURN TRUE
		ENDIF          
		RETURN FALSE
	
	///<summary>Close All RDDs referenced by this workarea list</summary>
	PUBLIC METHOD CloseAll() AS LOGIC
		LOCAL lResult := TRUE AS LOGIC
		BEGIN LOCK RDDs      
			LastException := NULL
			FOR VAR i := 0 TO MaxWorkAreas-1
				IF RDDs[i] != NULL
					VAR oRdd := RDDs[i]
					TRY
						lResult := lResult .AND. oRdd:Close()
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

	///<summary>Commit changes in all workares in this workarea list</summary>
	PUBLIC METHOD CommitAll() AS LOGIC
		LOCAL lResult := TRUE AS LOGIC
		BEGIN LOCK RDDs      
			LastException := NULL
			FOR VAR i := 0 TO MaxWorkAreas-1
				IF RDDs[i] != NULL
					VAR oRdd := RDDs[i]
					TRY
						lResult := lResult .AND. oRdd:Flush()
					CATCH e AS Exception
						lResult := FALSE
						LastException := e
					END TRY
				ENDIF              
			NEXT           
		END LOCK                       
		RETURN lResult 

	///<summary>Close area with 1 based workarea number</summary>
	PUBLIC METHOD CloseArea( nArea AS LONG) AS LOGIC
		LOCAL lResult := FALSE AS LOGIC
		IF AdjustArea(REF  nArea)
			BEGIN LOCK RDDs               
				LastException := NULL
				IF RDDs[ nArea] != NULL
					VAR oRdd := RDDs[ nArea]
					TRY
						lResult := oRdd:Close()
						IF lResult
							Aliases:Remove(oRdd:Alias)
						ENDIF
					CATCH e AS Exception
						lResult			:= FALSE  
						LastException	:= e
					END TRY
				ENDIF              
				RDDs[ nArea] 		:= NULL
			END LOCK               
		ENDIF
		RETURN lResult   

	///<summary> Return 1 based Workarea Number for Alias or 0 when no found</summary>
	PUBLIC METHOD FindAlias(sAlias AS STRING) AS LONG
		sAlias := sAlias:ToUpperInvariant()
		BEGIN LOCK RDDs 
			IF Aliases:ContainsKey(sAlias)
				RETURN Aliases[sAlias]
			ENDIF 
		END LOCK  
		RETURN 0  

	///<summary> Return 1 based empty Workarea</summary>
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

	///<summary>Get Alias for 1 based Workarea Number</summary>
	PUBLIC METHOD GetAlias( nArea AS LONG) AS STRING
		IF AdjustArea(REF nArea) 
			BEGIN LOCK RDDs
				IF RDDs[nArea] != NULL
					RETURN RDDs[nArea]:Alias
				ENDIF
			END LOCK
		ENDIF
		RETURN NULL       
		    
	///<summary>Get RDD object for 1 based Workarea Number</summary>
	PUBLIC METHOD GetRDD( nArea AS LONG) AS IRDD
		IF AdjustArea(REF nArea)
			BEGIN LOCK RDDs
				RETURN RDDs[ nArea]
			END LOCK
		ENDIF
		RETURN NULL
		
	///<summary>Set RDD object and ALias for 1 based Workarea Number</summary>
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

	///<summary>Get 1 based Current workarea Number</summary>
	PUBLIC PROPERTY CurrentWorkAreaNO AS LONG GET iCurrentWorkArea

	///<summary>Get Current workarea Object</summary>
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
