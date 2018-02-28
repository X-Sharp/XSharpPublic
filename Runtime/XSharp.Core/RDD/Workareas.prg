//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp
CLASS WorkAreas
	// Not static because every thread can have its own workareas structure
	#region Constants
		PUBLIC CONST MaxWorkAreas := 4096 AS LONG
	#endregion
	
	#region Fields
	PRIVATE Aliases  AS STRING[]
	PRIVATE RDDs	 AS IRDD[]    
	PRIVATE iCurrentWorkarea as LONG
	PUBLIC LastException AS Exception 

	STATIC METHOD GetInstance() as WorkAreas
		LOCAL oState	:= XSharp.Runtime.State.GetInstance() as XSharp.Runtime.State
		LOCAL oInstance := oState:WorkAreas as WorkAreas
		RETURN oInstance

	#endregion
	CONSTRUCTOR()
		Aliases 			:= STRING[]{MaxWorkAreas}
		RDDs				:= IRDD[]{MaxWorkAreas}   
		iCurrentWorkArea	:= 1
		
	PRIVATE METHOD AdjustArea( nArea REF LONG) AS LOGIC
		IF  nArea > 0 .and.  nArea <= MaxWorkAreas
			 nArea -=1
			RETURN TRUE
		ENDIF          
		RETURN FALSE
		
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
				Aliases[i] 	:= NULL
			NEXT           
		END LOCK                       
		RETURN lResult 

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

	PUBLIC METHOD CloseArea( nArea AS LONG) AS LOGIC
		LOCAL lResult := FALSE AS LOGIC
		IF AdjustArea(REF  nArea)
			BEGIN LOCK RDDs               
				LastException := NULL
				IF RDDs[ nArea] != NULL
					VAR oRdd := RDDs[ nArea]
					TRY
						lResult := oRdd:Close()
					CATCH e AS Exception
						lResult := FALSE  
						LastException := e
					END TRY
				ENDIF              
				RDDs[ nArea] 	:= NULL
				Aliases[ nArea] 	:= NULL
			END LOCK               
		ENDIF
		RETURN lResult   

	PUBLIC METHOD FindAlias(sAlias AS STRING) AS LONG
		sAlias := sAlias:ToUpperInvariant()
		BEGIN LOCK RDDs  
			FOR VAR i := 0 TO MaxWorkAreas     
				IF String.Compare(Aliases[i],sAlias) == 0
					RETURN i+1
				ENDIF
			NEXT
		END LOCK  
		RETURN 0  

	PUBLIC METHOD FindEmptyArea(fromStart AS LOGIC) AS LONG
		LOCAL i AS LONG
		BEGIN LOCK RDDs                                  
			IF fromStart
				FOR i := 0 TO MaxWorkAreas     
					IF RDDs[i] == NULL
						RETURN i
					ENDIF
				NEXT
			ELSE 
				FOR i := MaxWorkAreas DOWNTO 0 
					IF RDDs[i] == NULL
						RETURN i
					ENDIF
				NEXT
			ENDIF
		END LOCK  
		RETURN 0

	PUBLIC METHOD GetAlias( nArea AS LONG) AS STRING
		IF AdjustArea(REF nArea) 
			BEGIN LOCK RDDs
				RETURN Aliases[ nArea]
			END LOCK
		ENDIF
		RETURN NULL               
		
	PUBLIC METHOD GetRDD( nArea AS LONG) AS IRDD
		IF AdjustArea(REF nArea)
			BEGIN LOCK RDDs
				RETURN RDDs[ nArea]
			END LOCK
		ENDIF
		RETURN NULL
		
	PUBLIC METHOD SetArea( nArea AS LONG, sAlias AS STRING, oRDD AS IRDD) AS LOGIC
		// sAlias and oRdd may be empty (when clearing the RDD)
		IF AdjustArea(REF nArea)
			IF ! String.IsNullOrEmpty(sAlias)
				sAlias := sAlias:ToUpperInvariant()
			ENDIF			
			BEGIN LOCK RDDs
				RDDs[ nArea] 	:= oRDD 
				Aliases[ nArea] := sAlias
			END LOCK
			RETURN TRUE
		ENDIF          
		RETURN FALSE   

	PUBLIC PROPERTY CurrentWorkAreaNO as LONG GET iCurrentWorkArea

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
