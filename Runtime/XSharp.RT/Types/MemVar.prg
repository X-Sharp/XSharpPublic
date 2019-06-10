//
// Copyright (c) XSharp B.V.  All Rights Reserved. 
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.Linq
USING System.Diagnostics
USING System.Text
USING System.Threading


// Class that holds the memvars for a certain level on the callstack
[DebuggerDisplay("Level:{Depth}")];	
INTERNAL CLASS XSharp.MemVarLevel                     
	PROPERTY Variables AS Dictionary<STRING, MEMVAR> AUTO    
	PROPERTY Depth AS INT AUTO GET PRIVATE SET
	CONSTRUCTOR (nDepth AS INT)              
		Variables   := Dictionary<STRING, MEMVAR>{StringComparer.OrdinalIgnoreCase}
		Depth       := nDepth
		RETURN

	METHOD Add(variable AS XSharp.MemVar) AS VOID
		variable:Level := SELF
		Variables:Add(variable:Name, variable)
		RETURN
		
	METHOD ContainsKey(cName AS STRING) AS LOGIC
		RETURN Variables:ContainsKey(cName) 
		
	METHOD TryGetValue(cName AS STRING, oMemVar OUT XSharp.MemVar) AS LOGIC
		RETURN Variables:TryGetValue(cName, OUT oMemVar )
	
	METHOD Remove(cName AS STRING) AS LOGIC
		RETURN Variables:Remove(cName)

	PROPERTY SELF[Name AS STRING] AS XSharp.MemVar
		GET                     
			LOCAL oMemVar AS XSharp.MemVar
			IF Variables:TryGetValue(name, OUT oMemVar)
				RETURN oMemVar
			ENDIF
			RETURN NULL
		END GET
	END PROPERTY

	PROPERTY Keys AS IEnumerable<STRING> GET Variables:Keys
	
	METHOD Clear() AS VOID STRICT
		Variables:Clear()	                 
		
	PROPERTY Count AS INT GET Variables:Count	
		
END CLASS


/// <summary>Internal type that implements the Dynamic Memory Variables.<br/>
/// </summary>
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
[DebuggerDisplay("Memvar: {Name}")];	
PUBLIC CLASS XSharp.MemVar 


    INTERNAL CLASS MemVarThreadInfo
        INTERNAL Levels AS Stack <MemVarLevel>
        INTERNAL Depth  AS INT
        CONSTRUCTOR()
            Levels := Stack <MemVarLevel>{32}
            Depth  := 0
            RETURN
            
    END CLASS

	// Static fields that monitor all memvars 
	PRIVATE STATIC Publics  AS MemVarLevel

    // Stack local privates with initializer and property to access the current stacks privates
    PRIVATE STATIC ThreadList := ThreadLocal< MemVarThreadInfo >{ {=> MemVarThreadInfo{} }}  AS ThreadLocal< MemVarThreadInfo >
    PRIVATE STATIC PROPERTY Info       AS MemVarThreadInfo          GET ThreadList:Value
	PRIVATE STATIC PROPERTY Privates   AS Stack <MemVarLevel>       GET Info:Levels
    PRIVATE STATIC PROPERTY Depth      AS INT GET Info:Depth        SET Info:Depth := VALUE
	PRIVATE STATIC PROPERTY Current    AS MemVarLevel GET IIF (Privates:Count > 0, Privates:Peek(), NULL)

	
    // for the enumeration.
	PRIVATE STATIC _PrivatesEnum AS IEnumerator<STRING>
	PRIVATE STATIC _PublicsEnum  AS IEnumerator<STRING>

	STATIC CONSTRUCTOR
		Publics  		:= MemVarLevel{-1}
		_PrivatesEnum 	:= NULL
		_PublicsEnum  	:= NULL   
		Depth           := 0

    // Instance fields
    /// <summary>Name of the memory variable.</summary>
  	PUBLIC PROPERTY Name 	AS STRING AUTO
    /// <summary>Value of the memory variable. The default is NIL for PRIVATEs and FALSE for PUBLICs.</summary>
  	PUBLIC PROPERTY @@Value AS USUAL AUTO
  	INTERNAL Level	AS MemVarLevel
	CONSTRUCTOR (cName AS STRING, uValue AS USUAL)
	  	SELF:Name := cName:ToUpper()
	  	SELF:Value := uValue
	  	RETURN
#region Privates
    /// <exclude />
	STATIC INTERNAL METHOD InitPrivates() AS INT 
		// Initialize privates for a new function/method
		// returns # of levels available after initialization
		Depth 	+= 1   
		RETURN Depth
    /// <exclude />
	STATIC PRIVATE METHOD CheckCurrent() AS VOID
		IF Privates:Count() == 0 .OR. Privates:Peek():Depth < Depth
			Privates:Push( MemVarLevel{ Depth })
		ENDIF   

    
	STATIC METHOD ReleasePrivates(nLevel AS INT) AS LOGIC
		DO WHILE Privates:Count > 0 .AND. Privates:Peek():Depth >= nLevel
			Privates:Pop()
		ENDDO      
		Depth --
		RETURN TRUE
		
	
	STATIC METHOD GetHigherLevelPrivate(name AS STRING) AS XSharp.MemVar
		FOREACH VAR previous IN privates    
			LOCAL oMemVar AS XSharp.MemVar
			IF previous!= current .AND. previous:TryGetValue(name, OUT oMemVar)
				RETURN oMemVar
			ENDIF   
		NEXT		
		RETURN NULL	

    
	STATIC METHOD PrivatePut(name AS STRING, VALUE AS USUAL) AS LOGIC
		CheckCurrent()      
		LOCAL oMemVar AS XSharp.MemVar
		IF current:TryGetValue(name, OUT oMemVar)
			oMemVar:Value := VALUE
			RETURN TRUE			
		ENDIF
        oMemVar := GetHigherLevelPrivate(name)
        IF oMemVar != NULL
        	oMemVar:Value := VALUE
        	RETURN TRUE
        ENDIF
		RETURN FALSE	

	
	STATIC METHOD PrivateFind(name AS STRING) AS XSharp.MemVar
		LOCAL oMemVar AS XSharp.MemVar
		IF current != NULL .AND. current:TryGetValue(name, OUT oMemVar)
			RETURN oMemVar
		ENDIF   
        RETURN GetHigherLevelPrivate(name)

	
	STATIC METHOD Release(name AS STRING) AS VOID
		// release variable
		VAR oMemVar := PrivateFind(name)
		IF oMemVar == NULL
			oMemVar := PublicFind(name)
            IF oMemVar != NULL
                publics:Remove(oMemVar:Name)
            ENDIF
        ELSE
            LOCAL level AS MemVarLevel
            level := oMemVar:Level
            level:Remove(oMemVar:Name)
		ENDIF
		IF oMemVar != NULL
			oMemVar:Value := NIL
		ELSE
			THROW Exception{"Variable "+name:ToString()+" does not exist"}
		ENDIF
		RETURN 

    /// <exclude />
	STATIC PRIVATE METHOD _GetUniquePrivates(lCurrentOnly := FALSE AS LOGIC) AS List<STRING>
		VAR _TempPrivates := List<STRING>{}  
		IF lCurrentOnly                         
			IF Current != NULL
				_TempPrivates:AddRange(current:Keys)			
			ENDIF
		ELSE
			FOREACH VAR previous IN privates 
				IF _TempPrivates:Count == 0
					_TempPrivates:AddRange(previous:Keys)
				ELSE
					FOREACH VAR key IN previous:Keys
						IF !_tempPrivates:Contains(key)	
							_tempPrivates:Add(key)
						ENDIF
					NEXT
				ENDIF
			NEXT	
		ENDIF
	    RETURN _TempPrivates  
	    
    
	STATIC METHOD PrivatesEnum(lCurrentOnly := FALSE AS LOGIC) AS IEnumerator<STRING>
		RETURN _GetUniquePrivates(lCurrentOnly):GetEnumerator()


    
 	STATIC METHOD PrivatesFirst(lCurrentOnly := FALSE AS LOGIC) AS STRING
		_PrivatesEnum := PrivatesEnum(lCurrentOnly)
		_PrivatesEnum:Reset()
		RETURN PrivatesNext()

    
	STATIC METHOD PrivatesNext() AS STRING
		IF _PrivatesEnum != NULL
			IF _PrivatesEnum:MoveNext()
				RETURN _PrivatesEnum:Current
			ENDIF
			_PrivatesEnum  := NULL 
		ENDIF                
		RETURN NULL_STRING

    
	STATIC METHOD PrivatesCount(lCurrentOnly := FALSE AS LOGIC) AS INT   
		RETURN _GetUniquePrivates(lCurrentOnly):Count
		

    
	STATIC METHOD ReleaseAll() AS VOID
		// Assign NIL to all currenly visible private variables
		// Hidden privates are not affected
		FOREACH VAR sym IN _GetUniquePrivates(FALSE)
			PrivatePut(sym, NIL)
		NEXT
		RETURN

#endregion   

#region Generic - Public and Private
    
	STATIC METHOD Add(name AS STRING, _priv AS LOGIC) AS VOID
		IF _priv    
			CheckCurrent() 
			IF !Current:ContainsKey(name)
				Current:Add(MEMVAR{name,NIL})
			ENDIF
		ELSE
            BEGIN LOCK Publics
			    IF !Publics:ContainsKey(name)  
				    VAR memvar := MEMVAR{name,FALSE}        // publics are always initialized with FALSE
				    Publics:Add(MEMVAR )
			    ENDIF
            END LOCK
		ENDIF  
    

	STATIC METHOD Get(name AS STRING) AS USUAL 
		LOCAL oMemVar AS XSharp.MemVar
		// privates take precedence over publics ?
		oMemVar := PrivateFind(name)
		IF oMemVar == NULL
			oMemVar := PublicFind(name)
		ENDIF            
		IF oMemVar != NULL
			RETURN oMemVar:Value
		ENDIF            
		THROW Error{"Undeclared variable :"+name:ToString()}
    
	STATIC METHOD Put(name AS STRING, VALUE AS USUAL) AS USUAL
		LOCAL oMemVar AS XSharp.MemVar
		// assign to existing memvar first
		// privates take precedence over publics ?
		oMemVar := PrivateFind(name)
		IF oMemVar == NULL
			oMemVar := PublicFind(name)
		ENDIF      		
		IF oMemVar != NULL
            BEGIN LOCK oMemVar
			    oMemVar:Value := VALUE
            END LOCK
		ELSE
			// memvar does not exist, then add it at the current level
			CheckCurrent()
			Current:Add(MEMVAR{name,VALUE})
		ENDIF    
		RETURN VALUE

    
	STATIC METHOD ClearAll() AS VOID
		// Remove all public and private variables   
		// Does not clear the privates stack levels
        BEGIN LOCK Publics
		    Publics:Clear()
        END LOCK
		FOREACH VAR level IN Privates
			level:Clear()
		NEXT

    STATIC METHOD Clear(name AS STRING) AS LOGIC
	    // assign nil to visible private. Does not really release the variable.		
		VAR oMemVar := PrivateFind(name)
		IF oMemVar == NULL
            oMemVar := PublicFind(name)
		ENDIF
        IF oMemVar != NULL
			oMemVar:Value := NIL
		ELSE
			THROW Exception{"Variable "+name:ToString()+" does not exist"}
		ENDIF
		RETURN TRUE
				
#endregion	 

#region Publics	
    

	STATIC METHOD PublicFind(name AS STRING) AS XSharp.MemVar
		LOCAL oMemVar AS XSharp.MemVar
        BEGIN LOCK Publics
		    IF Publics:TryGetValue(name, OUT oMemVar)
			    RETURN oMemVar
		    ENDIF
        END LOCK
		RETURN NULL
		
				
    
	STATIC METHOD PublicPut(name AS STRING, VALUE AS USUAL) AS LOGIC
		VAR oMemVar := PublicFind(name)
		IF oMemVar != NULL
            BEGIN LOCK oMemVar
			    oMemVar:Value := VALUE
            END LOCK
			RETURN TRUE			
		ENDIF  
		RETURN FALSE
					
    
	STATIC METHOD PublicsEnum() AS IEnumerator<STRING>
        BEGIN LOCK Publics
		    RETURN Publics:Keys:GetEnumerator()
        END LOCK

    
	STATIC METHOD PublicsFirst() AS STRING
		_PublicsEnum := PublicsEnum()
		_PublicsEnum:Reset()
		RETURN PublicsNext()    
		
    STATIC METHOD PublicsNext() AS STRING
		IF _PublicsEnum != NULL
			IF _PublicsEnum:MoveNext()
				RETURN _PublicsEnum:Current
			ENDIF
			_PublicsEnum := NULL
		ENDIF                
		RETURN NULL_STRING
		
    
	STATIC METHOD PublicsCount() AS INT
        BEGIN LOCK Publics
		    RETURN Publics:Count
        END LOCK
     
#endregion		
END CLASS	
