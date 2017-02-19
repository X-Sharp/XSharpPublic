//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp.RDD
STATIC CLASS WorkAreas
	#region Constants
		PUBLIC CONST MaxWorkAreas := 4096 AS LONG
	#endregion
	
	#region Fields
	PRIVATE STATIC Aliases  AS STRING[]
	PRIVATE STATIC RDDs		AS IRDD[]    

	PUBLIC STATIC LastException AS Exception 
	PUBLIC STATIC iCurrentWorkarea AS LONG			// 1 based Workarea number of current workarea
	#endregion
	STATIC CONSTRUCTOR
		Aliases 			:= STRING[]{MaxWorkAreas}
		RDDs				:= IRDD[]{MaxWorkAreas}   
		iCurrentWorkarea 	:= 1
	PRIVATE STATIC METHOD AdjustArea( nArea REF LONG) AS LOGIC
		IF  nArea > 0 .and.  nArea <= MaxWorkAreas
			 nArea -=1
			RETURN TRUE
		ENDIF          
		RETURN FALSE
		
	PUBLIC STATIC METHOD CloseAll() AS LOGIC
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
	PUBLIC STATIC METHOD CloseArea( nArea AS LONG) AS LOGIC
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
	PUBLIC STATIC METHOD FindAlias(sAlias AS STRING) AS LONG
		sAlias := sAlias:ToUpperInvariant()
		BEGIN LOCK RDDs  
			FOR VAR i := 0 TO MaxWorkAreas     
				IF String.Compare(Aliases[i],sAlias) == 0
					RETURN i+1
				ENDIF
			NEXT
		END LOCK  
		RETURN 0  
	PUBLIC STATIC METHOD FindEmptyArea(fromStart AS LOGIC) AS LONG
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
	PUBLIC STATIC METHOD GetAlias( nArea AS LONG) AS STRING
		IF AdjustArea(REF  nArea) 
			BEGIN LOCK RDDs
				RETURN Aliases[ nArea]
			END LOCK
		ENDIF
		RETURN NULL               
		
	PUBLIC STATIC METHOD GetRDD( nArea AS LONG) AS IRDD
		IF AdjustArea(REF  nArea)
			BEGIN LOCK RDDs
				RETURN RDDs[ nArea]
			END LOCK
		ENDIF
		RETURN NULL
		
	PUBLIC STATIC METHOD SetArea( nArea AS LONG, sAlias AS STRING, oRDD AS IRDD) AS LOGIC
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
	PUBLIC STATIC PROPERTY CurrentWorkArea AS IRDD
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
