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
	PROPERTY Variables AS Dictionary<SYMBOL, MemVar> AUTO    
	PROPERTY Depth AS INT AUTO GET PRIVATE SET
	CONSTRUCTOR (nDepth AS INT)              
		Variables   := Dictionary<SYMBOL, MemVar>{}
		Depth       := nDepth
		RETURN

	METHOD Add(variable AS MemVar) AS VOID
#ifdef DEBUG
		variable:Level := SELF
#endif
		Variables:Add(variable:Name, variable)
		RETURN
		
	METHOD ContainsKey(symName AS SYMBOL) AS LOGIC
		RETURN Variables:ContainsKey(symName) 
		
	METHOD TryGetValue(symName AS SYMBOL, VALUE OUT MemVar) AS LOGIC
		RETURN Variables:TryGetValue(symName, OUT VALUE)
	
	PROPERTY SELF[name AS SYMBOL] AS MemVar
		GET                     
			LOCAL memvar AS MemVar
			IF Variables:TryGetValue(name, OUT memvar)
				RETURN memvar
			ENDIF
			RETURN NULL
		END GET
	END PROPERTY

	PROPERTY Keys AS IEnumerable<SYMBOL> GET Variables:Keys
	
	METHOD Clear() AS VOID STRICT
		Variables:Clear()	                 
		
	PROPERTY Count AS INT GET Variables:Count	
		
END CLASS


[DebuggerDisplay("Memvar: {Name}")];	
PUBLIC CLASS XSharp.MemVar 

	// Static fields that monitor all memvars 
	PRIVATE STATIC Publics  AS MemVarLevel
	PRIVATE STATIC Current 	AS MemVarLevel
	PRIVATE STATIC Depth	AS INT

    // Stack local privates with initializer and property to access the current stacks privates
    PRIVATE STATIC PrivatesStack := ThreadLocal< Stack <MemVarLevel> >{ {=> Stack <MemVarLevel>{32} }}  AS ThreadLocal< Stack <MemVarLevel> >
	PRIVATE STATIC PROPERTY Privates AS Stack <MemVarLevel> GET PrivatesStack:Value
	
    // for the enumeration.
	PRIVATE STATIC _PrivatesEnum AS IEnumerator<SYMBOL>
	PRIVATE STATIC _PublicsEnum  AS IEnumerator<SYMBOL>

	STATIC CONSTRUCTOR
		Publics  		:= MemVarLevel{-1}
		Current  		:= NULL
		_PrivatesEnum 	:= NULL
		_PublicsEnum  	:= NULL   
		Depth           := 0

    // Instance fields
  	PUBLIC Name 	AS SYMBOL
  	PUBLIC @@Value 	AS USUAL
#ifdef DEBUG
  	INTERNAL Level	AS MemVarLevel
#endif
	CONSTRUCTOR (symName AS SYMBOL, uValue AS USUAL)
	  		Name := symName
	  		SELF:Value := uValue
	  		RETURN
#region Privates
	STATIC METHOD InitPrivates() AS INT 
		// Initialize privates for a new function/method
		// returns # of levels available after initialization
		Depth 	+= 1   
		Current := NULL
		RETURN Depth

	STATIC METHOD CheckCurrent() AS VOID
		IF Current == NULL
			IF Privates:Count() == 0 .OR. Privates:Peek():Depth < Depth
				Privates:Push( MemVarLevel{ Depth })
			ENDIF   
			Current := Privates:Peek()
		ENDIF
		
		
	STATIC METHOD ReleasePrivates(nLevel AS INT) AS LOGIC
		DO WHILE Privates:Count > 0 .AND. Privates:Peek():Depth >= nLevel
			Privates:Pop()
		ENDDO      
		Depth --
        Current := NULL
		IF Privates:Count > 0 .AND. Privates:Peek():Depth == Depth
			Current := Privates:Peek()
		ENDIF
		RETURN TRUE
		
	
	STATIC METHOD GetHigherLevelPrivate(name AS SYMBOL) AS XSharp.MemVar
		FOREACH VAR previous IN privates    
			LOCAL memvar AS MemVar
			IF previous!= current .AND. previous:TryGetValue(name, OUT memvar)
				RETURN memvar
			ENDIF   
		NEXT		
		RETURN NULL	
		
	STATIC METHOD PrivatePut(name AS SYMBOL, VALUE AS USUAL) AS LOGIC
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
			                                    
	STATIC METHOD PrivateFind(name AS SYMBOL) AS MemVar
		LOCAL memvar AS MemVar
		IF current != NULL .AND. current:TryGetValue(name, OUT memvar)
			RETURN memvar
		ENDIF   
        RETURN GetHigherLevelPrivate(name)
		
	STATIC METHOD Release(name AS SYMBOL) AS VOID
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

	STATIC PRIVATE METHOD _GetUniquePrivates(lCurrentOnly := FALSE AS LOGIC) AS List<SYMBOL>
		VAR _TempPrivates := List<SYMBOL>{}  
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
	    
	STATIC METHOD PrivatesEnum(lCurrentOnly := FALSE AS LOGIC) AS IEnumerator<SYMBOL>
		RETURN _GetUniquePrivates(lCurrentOnly):GetEnumerator()


 	STATIC METHOD PrivatesFirst(lCurrentOnly := FALSE AS LOGIC) AS SYMBOL
		_PrivatesEnum := PrivatesEnum(lCurrentOnly)
		_PrivatesEnum:Reset()
		RETURN PrivatesNext()

	STATIC METHOD PrivatesNext() AS SYMBOL
		IF _PrivatesEnum != NULL
			IF _PrivatesEnum:MoveNext()
				RETURN _PrivatesEnum:Current
			ENDIF
			_PrivatesEnum  := NULL 
		ENDIF                
		RETURN NULL_SYMBOL  				

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
		
	STATIC METHOD Add(name AS SYMBOL, _priv AS LOGIC) AS VOID
		IF _priv    
			CheckCurrent() 
			IF !Current:ContainsKey(name)
				Current:Add(MemVar{name,NIL})
			ENDIF
		ELSE
            BEGIN LOCK Publics
			    IF !Publics:ContainsKey(name)  
				    VAR memvar := MemVar{name,NIL}
				    Publics:Add(memvar )
			    ENDIF
            END LOCK
		ENDIF  

	STATIC METHOD Get(name AS SYMBOL) AS USUAL 
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

	STATIC METHOD Put(name AS SYMBOL, VALUE AS USUAL) AS USUAL
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

	STATIC METHOD PublicFind(name AS SYMBOL) AS MemVar
		LOCAL oMemVar AS MemVar
        BEGIN LOCK Publics
		    IF Publics:TryGetValue(name, OUT oMemVar)
			    RETURN oMemVar
		    ENDIF
        END LOCK
		RETURN NULL
		
				
	STATIC METHOD PublicPut(name AS SYMBOL, VALUE AS USUAL) AS LOGIC
		VAR oMemVar := PublicFind(name)
		IF oMemVar != NULL
            BEGIN LOCK oMemVar
			    oMemVar:Value := VALUE
            END LOCK
			RETURN TRUE			
		ENDIF  
		RETURN FALSE
					
	STATIC METHOD PublicsEnum() AS IEnumerator<SYMBOL>
        BEGIN LOCK Publics
		    RETURN Publics:Keys:GetEnumerator()
        END LOCK

	STATIC METHOD PublicsFirst() AS SYMBOL
		_PublicsEnum := PublicsEnum()
		_PublicsEnum:Reset()
		RETURN PublicsNext()    
		
	STATIC METHOD PublicsNext() AS SYMBOL
		IF _PublicsEnum != NULL
			IF _PublicsEnum:MoveNext()
				RETURN _PublicsEnum:Current
			ENDIF
			_PublicsEnum := NULL
		ENDIF                
		RETURN NULL_SYMBOL      
		

	STATIC METHOD PublicsCount() AS INT
        BEGIN LOCK Publics
		    RETURN Publics:Count
        END LOCK
     
#endregion		
END CLASS	
