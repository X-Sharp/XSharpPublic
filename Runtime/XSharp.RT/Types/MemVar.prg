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
	PROPERTY Variables AS Dictionary<STRING, MemVar> AUTO    
	PROPERTY Depth AS INT AUTO GET PRIVATE SET
	CONSTRUCTOR (nDepth AS INT)              
		Variables   := Dictionary<STRING, MemVar>{StringComparer.OrdinalIgnoreCase}
		Depth       := nDepth
		RETURN

	METHOD Add(variable AS MemVar) AS VOID
#ifdef DEBUG
		variable:Level := SELF
#endif
		Variables:Add(variable:Name, variable)
		RETURN
		
	METHOD ContainsKey(cName AS STRING) AS LOGIC
		RETURN Variables:ContainsKey(cName) 
		
	METHOD TryGetValue(cName AS STRING, VALUE OUT MemVar) AS LOGIC
		RETURN Variables:TryGetValue(cName, OUT VALUE)
	
	PROPERTY SELF[Name AS STRING] AS MemVar
		GET                     
			LOCAL memvar AS MemVar
			IF Variables:TryGetValue(name, OUT memvar)
				RETURN memvar
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
        INTERNAL Levels as Stack <MemVarLevel>
        INTERNAL Depth  as INT
        CONSTRUCTOR()
            Levels := Stack <MemVarLevel>{32}
            Depth  := 0
            RETURN
            
    END CLASS

	// Static fields that monitor all memvars 
	PRIVATE STATIC Publics  AS MemVarLevel

    // Stack local privates with initializer and property to access the current stacks privates
    PRIVATE STATIC ThreadList := ThreadLocal< MemVarThreadInfo >{ {=> MemVarThreadInfo{} }}  AS ThreadLocal< MemVarThreadInfo >
    PRIVATE STATIC PROPERTY Info       as MemVarThreadInfo          GET ThreadList:Value
	PRIVATE STATIC PROPERTY Privates   AS Stack <MemVarLevel>       GET Info:Levels
    PRIVATE STATIC PROPERTY Depth      AS INT GET Info:Depth        SET Info:Depth := Value
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
#ifdef DEBUG
  	INTERNAL Level	AS MemVarLevel
#endif
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
		
	/// <exclude />
	STATIC METHOD ReleasePrivates(nLevel AS INT) AS LOGIC
		DO WHILE Privates:Count > 0 .AND. Privates:Peek():Depth >= nLevel
			Privates:Pop()
		ENDDO      
		Depth --
		RETURN TRUE
		
	/// <exclude />
	STATIC METHOD GetHigherLevelPrivate(name AS STRING) AS XSharp.MemVar
		FOREACH VAR previous IN privates    
			LOCAL memvar AS MemVar
			IF previous!= current .AND. previous:TryGetValue(name, OUT memvar)
				RETURN memvar
			ENDIF   
		NEXT		
		RETURN NULL	

    /// <exclude />
	STATIC METHOD PrivatePut(name AS STRING, VALUE AS USUAL) AS LOGIC
		CheckCurrent()      
		LOCAL memvar AS MemVar
		IF current:TryGetValue(name, OUT memvar)
			MEMVAR:Value := VALUE
			RETURN TRUE			
		ENDIF
        MEMVAR := GetHigherLevelPrivate(name)
        IF memvar != NULL
        	MEMVAR:Value := VALUE
        	RETURN TRUE
        ENDIF
		RETURN FALSE	

	/// <exclude />		                                    
	STATIC METHOD PrivateFind(name AS STRING) AS MemVar
		LOCAL memvar AS MemVar
		IF current != NULL .AND. current:TryGetValue(name, OUT memvar)
			RETURN memvar
		ENDIF   
        RETURN GetHigherLevelPrivate(name)
	/// <exclude />	
	STATIC METHOD Release(name AS STRING) AS VOID
		// assign nil to visible private. Does not really release the variable.		
		VAR Memvar := PrivateFind(name)
		IF memvar == NULL
			MEMVAR := PublicFind(name)
		ENDIF
		IF memvar != NULL
			MEMVAR:Value := NIL
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
	    
    /// <exclude />
	STATIC METHOD PrivatesEnum(lCurrentOnly := FALSE AS LOGIC) AS IEnumerator<STRING>
		RETURN _GetUniquePrivates(lCurrentOnly):GetEnumerator()


    /// <exclude />
 	STATIC METHOD PrivatesFirst(lCurrentOnly := FALSE AS LOGIC) AS STRING
		_PrivatesEnum := PrivatesEnum(lCurrentOnly)
		_PrivatesEnum:Reset()
		RETURN PrivatesNext()

    /// <exclude />
	STATIC METHOD PrivatesNext() AS STRING
		IF _PrivatesEnum != NULL
			IF _PrivatesEnum:MoveNext()
				RETURN _PrivatesEnum:Current
			ENDIF
			_PrivatesEnum  := NULL 
		ENDIF                
		RETURN NULL_STRING

    /// <exclude />
	STATIC METHOD PrivatesCount(lCurrentOnly := FALSE AS LOGIC) AS INT   
		RETURN _GetUniquePrivates(lCurrentOnly):Count
		

    /// <exclude />
	STATIC METHOD ReleaseAll() AS VOID
		// Assign NIL to all currenly visible private variables
		// Hidden privates are not affected
		FOREACH VAR sym IN _GetUniquePrivates(FALSE)
			PrivatePut(sym, NIL)
		NEXT
		RETURN

#endregion   

#region Generic - Public and Private
    /// <exclude />
	STATIC METHOD Add(name AS STRING, _priv AS LOGIC) AS VOID
		IF _priv    
			CheckCurrent() 
			IF !Current:ContainsKey(name)
				Current:Add(MemVar{name,NIL})
			ENDIF
		ELSE
            BEGIN LOCK Publics
			    IF !Publics:ContainsKey(name)  
				    VAR memvar := MemVar{name,FALSE}        // publics are always initialized with FALSE
				    Publics:Add(memvar )
			    ENDIF
            END LOCK
		ENDIF  
    /// <exclude />

	STATIC METHOD Get(name AS STRING) AS USUAL 
		LOCAL oMemVar AS MemVar
		// privates take precedence over publics ?
		oMemVar := PrivateFind(name)
		IF oMemVar == NULL
			oMemVar := PublicFind(name)
		ENDIF            
		IF oMemVar != NULL
			RETURN oMemVar:Value
		ENDIF            
		THROW Error{"Undeclared variable :"+name:ToString()}
    /// <exclude />

	STATIC METHOD Put(name AS STRING, VALUE AS USUAL) AS USUAL
		LOCAL oMemVar AS MemVar
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
			Current:Add(MemVar{name,VALUE})
		ENDIF    
		RETURN VALUE 
    /// <exclude />
	STATIC METHOD ClearAll() AS VOID
		// Remove all public and private variables   
		// Does not clear the privates stack levels
        BEGIN LOCK Publics
		    Publics:Clear()
        END LOCK
		FOREACH VAR level IN Privates
			level:Clear()
		NEXT

				
#endregion	 

#region Publics	
    /// <exclude />

	STATIC METHOD PublicFind(name AS STRING) AS MemVar
		LOCAL oMemVar AS MemVar
        BEGIN LOCK Publics
		    IF Publics:TryGetValue(name, OUT oMemVar)
			    RETURN oMemVar
		    ENDIF
        END LOCK
		RETURN NULL
		
				
    /// <exclude />
	STATIC METHOD PublicPut(name AS STRING, VALUE AS USUAL) AS LOGIC
		VAR oMemVar := PublicFind(name)
		IF oMemVar != NULL
            BEGIN LOCK oMemVar
			    oMemVar:Value := VALUE
            END LOCK
			RETURN TRUE			
		ENDIF  
		RETURN FALSE
					
    /// <exclude />
	STATIC METHOD PublicsEnum() AS IEnumerator<STRING>
        BEGIN LOCK Publics
		    RETURN Publics:Keys:GetEnumerator()
        END LOCK

    /// <exclude />
	STATIC METHOD PublicsFirst() AS STRING
		_PublicsEnum := PublicsEnum()
		_PublicsEnum:Reset()
		RETURN PublicsNext()    
		
    /// <exclude />
	STATIC METHOD PublicsNext() AS STRING
		IF _PublicsEnum != NULL
			IF _PublicsEnum:MoveNext()
				RETURN _PublicsEnum:Current
			ENDIF
			_PublicsEnum := NULL
		ENDIF                
		RETURN NULL_STRING
		
    /// <exclude />
	STATIC METHOD PublicsCount() AS INT
        BEGIN LOCK Publics
		    RETURN Publics:Count
        END LOCK
     
#endregion		
END CLASS	
