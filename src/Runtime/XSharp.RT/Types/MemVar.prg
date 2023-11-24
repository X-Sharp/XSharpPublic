//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Collections.Concurrent
USING System.Collections.Generic
USING System.Collections
USING System.Linq
USING System.Diagnostics
USING System.Text
USING System.Threading


// Class that holds the memvars for a certain level on the callstack
[DebuggerDisplay("{DebuggerDisplay(),nq}")];
INTERNAL CLASS XSharp.MemVarLevel
    INTERNAL PROPERTY Variables      AS ConcurrentDictionary<STRING, XSharp.MemVar> AUTO
    INTERNAL PROPERTY Locals         AS ConcurrentDictionary<STRING, USUAL>   AUTO
    INTERNAL PROPERTY Depth          AS INT     AUTO GET PRIVATE SET
    INTERNAL PROPERTY SystemLevel    AS LOGIC   AUTO GET PRIVATE SET
#ifdef DEBUG
    INTERNAL PROPERTY Stack          AS STRING  AUTO
#endif

        PRIVATE  _localsUpdated AS LOGIC
        // We mark MemvarLevels declared in the runtime with TRUE
    INTERNAL CONSTRUCTOR (nDepth AS INT, lSystem AS LOGIC)
        Variables   := ConcurrentDictionary<STRING, XSharp.MemVar>{StringComparer.OrdinalIgnoreCase}
        Depth       := nDepth
        SystemLevel := lSystem
#ifdef DEBUG
        Stack       := System.Diagnostics.StackTrace{2,TRUE}:ToString()
#endif
        RETURN

    INTERNAL METHOD Add(variable AS XSharp.MemVar) AS VOID
        variable:Level := SELF
        Variables:TryAdd(variable:Name, variable)
        RETURN

    INTERNAL METHOD ContainsKey(cName AS STRING) AS LOGIC
        RETURN Variables:ContainsKey(cName)

    INTERNAL METHOD TryGetValue(cName AS STRING, oMemVar OUT XSharp.MemVar) AS LOGIC
        RETURN Variables:TryGetValue(cName, OUT oMemVar )

    INTERNAL METHOD Remove(cName AS STRING) AS LOGIC
        RETURN Variables:TryRemove(cName, out var _)

    INTERNAL PROPERTY SELF[Name AS STRING] AS XSharp.MemVar
    GET
        IF Variables:TryGetValue(Name, OUT VAR oMemVar)
            RETURN oMemVar
        ENDIF
        RETURN NULL
    END GET
    END PROPERTY

    INTERNAL PROPERTY Keys AS ICollection<STRING> GET Variables:Keys

    INTERNAL METHOD Clear() AS VOID STRICT
        Variables:Clear()

    INTERNAL PROPERTY Count AS INT GET Variables:Count

#region Locals support
        // Set value for local and mark as 'updated'
    INTERNAL METHOD UpdateLocal(cName AS STRING, uValue AS USUAL) AS VOID
        SELF:SetLocal(cName, uValue)
        _localsUpdated  := TRUE
        RETURN
        // Set value for local
    INTERNAL METHOD SetLocal(cName AS STRING, uValue AS USUAL) AS VOID
        IF Locals == NULL
            Locals := ConcurrentDictionary<STRING, USUAL>{StringComparer.OrdinalIgnoreCase}
        ENDIF
        Locals[cName] := uValue    // overwrite variable if it already exists
        RETURN

        // Clear list of locals, reset updated flag
    INTERNAL METHOD ClearLocals() AS VOID
        Locals          := NULL
        _localsUpdated  := FALSE
        RETURN

        // find local and value returns TRUE when found (value could also be NIL !)
    INTERNAL METHOD FindLocal(cName AS STRING, uValue OUT USUAL) AS LOGIC
        IF Locals != NULL .AND. Locals:TryGetValue(cName, OUT uValue)
            RETURN TRUE
        ENDIF
        uValue := NIL
        RETURN FALSE

        // Get local or NIL
    INTERNAL METHOD GetLocal(cName AS STRING) AS USUAL
        IF Locals != NULL .AND. Locals:TryGetValue(cName, OUT VAR ret)
            RETURN ret
        ENDIF
        RETURN NIL

    INTERNAL PROPERTY LocalsUpdated AS LOGIC GET _localsUpdated
#endregion
    INTERNAL METHOD DebuggerDisplay() AS STRING
        IF Depth == -1
            RETURN "Public collection"
        ENDIF
        RETURN "Memvar collection for Level: "+ Depth:ToString()+", defined in " + IIF(SystemLevel, "Runtime","Usercode")

END CLASS


/// <summary>Internal type that implements the Dynamic Memory Variables.<br/>
/// </summary>
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
[DebuggerDisplay("Memvar: {Name,nq}: {Value}")];
PUBLIC CLASS XSharp.MemVar
/// <summary>Delegate for reading memvars.</summary>
/// <param name="name">Name of the Memvar for which the value has to be retrieved</param>
/// <returns>The value of the memory variable. </returns>
DELEGATE Getter(name AS STRING) AS USUAL
/// <summary>Delegate for writing memvars.</summary>
/// <param name="name">Name of the Memvar for which the value has to be retrieved</param>
/// <param name="value">Value to assign to the memory variable</param>
/// <remarks>When the variable does not exist then a new memory variable may be created.</remarks>
DELEGATE Putter(name AS STRING, @@value AS USUAL) AS USUAL

INTERNAL CLASS MemVarThreadInfo
    INTERNAL Levels AS Stack <MemVarLevel>
    INTERNAL Depth  AS INT
    // This marks the levels with a "Is System" flag, so System Levels can access locals from outside of system
    // we do not keep this in the Levels stack because the stack entries may not be created in time.
    INTERNAL _isSystem AS BitArray

    INTERNAL PROPERTY IsSystem AS LOGIC
    SET
        // No locals on the Public Level !
        IF Depth > 0
            IF Depth > _isSystem:Length
                _isSystem:Length := Depth+32
            ENDIF
            _isSystem:@@Set(Depth-1, VALUE)
        ENDIF
    END SET
    GET
        IF Depth > 0
            IF Depth > _isSystem:Length
                _isSystem:Length := Depth+32
            ENDIF
            RETURN _isSystem:@@Get(Depth-1)
        ENDIF
        // Public Level
        RETURN FALSE
    END GET
    END PROPERTY

    INTERNAL CONSTRUCTOR()
        Levels       := Stack <MemVarLevel>{32}
        Depth        := 1
        _isSystem    := BitArray{32}
        RETURN

END CLASS

// Static fields that monitor all memvars
PRIVATE STATIC Publics  AS MemVarLevel

// Stack local privates with initializer and property to access the current stacks privates
PRIVATE STATIC ThreadList := ThreadLocal< MemVarThreadInfo >{ {=> MemVarThreadInfo{} }}  AS ThreadLocal< MemVarThreadInfo >
    PRIVATE STATIC PROPERTY Info            AS MemVarThreadInfo          GET ThreadList:Value
    PRIVATE STATIC PROPERTY MemVarLevels    AS Stack <MemVarLevel>       GET Info:Levels
    PRIVATE STATIC PROPERTY Depth           AS INT GET Info:Depth        SET Info:Depth := VALUE
    PRIVATE STATIC PROPERTY Current         AS MemVarLevel GET IIF (MemVarLevels:Count > 0, MemVarLevels:Peek(), NULL)
    PRIVATE STATIC PROPERTY IsSystem        AS LOGIC GET Info:IsSystem   SET Info:IsSystem := VALUE


        // for the enumeration.
        PRIVATE STATIC _PrivatesEnum AS IEnumerator<STRING>
        PRIVATE STATIC _PublicsEnum  AS IEnumerator<STRING>


#region Debugger Functions
        // These names are used by the Debugger MemVar window
        // if you change them then also change them in MemvarsWindow.prg

    /// <exclude />
    STATIC METHOD DbgPublicsFirst() AS STRING
        RETURN PublicsFirst()
    /// <exclude />
    STATIC METHOD DbgPublicsNext() AS STRING
        RETURN PublicsNext()


    /// <exclude />
    STATIC METHOD DbgPrivatesFirst() AS STRING
        RETURN PrivatesFirst()
    /// <exclude />
    STATIC METHOD DbgPrivatesNext() AS STRING
        RETURN PrivatesNext()


    /// <exclude />
    STATIC METHOD DbgGetVar(cName as STRING) AS USUAL
        RETURN GetSafe(cName)

#endregion

    STATIC CONSTRUCTOR()
        Publics  		:= MemVarLevel{-1,TRUE}
        _PrivatesEnum 	:= NULL
        _PublicsEnum  	:= NULL
        Depth           := 0
        Initialize()

    PUBLIC STATIC METHOD Initialize() AS VOID
        MemVar.Get      := _Get
        MemVar.Put      := _Put
        MemVar.GetSafe  := _GetSafe

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
    STATIC INTERNAL METHOD InitPrivates(lFromSystem AS LOGIC) AS INT
        // Initialize privates for a new function/method
        // returns # of levels available after initialization
        Depth 	+= 1
        IsSystem := lFromSystem
        RETURN Depth

    /// <exclude />
    STATIC PRIVATE METHOD CheckCurrent() AS MemVarLevel
        IF MemVarLevels:Count() == 0 .OR. MemVarLevels:Peek():Depth < Depth
            MemVarLevels:Push( MemVarLevel{ Depth , IsSystem} )
        ENDIF
        RETURN MemVarLevels:Peek()


    /// <summary>Release all privates at a certain level and higher</summary>
    STATIC METHOD ReleasePrivates(nLevel AS INT) AS LOGIC
        DO WHILE MemVarLevels:Count > 0 .AND. MemVarLevels:Peek():Depth >= nLevel
            MemVarLevels:Pop()
        ENDDO
        Depth --
        RETURN TRUE


    /// <summary>Get a memvar object from the stack (if it exists)</summary>
    STATIC METHOD GetHigherLevelPrivate(name AS STRING) AS XSharp.MemVar
        FOREACH VAR previous IN MemVarLevels
            IF previous!= Current .AND. previous:TryGetValue(name, OUT VAR oMemVar)
                RETURN oMemVar
            ENDIF
        NEXT
        RETURN NULL

    /// <summary>Update a private variable. Does NOT add a new variable</summary>
    STATIC METHOD PrivatePut(name AS STRING, uValue AS USUAL) AS LOGIC
        VAR current := CheckCurrent()
        LOCAL oMemVar AS XSharp.MemVar
        IF current != NULL .AND. current:TryGetValue(name, OUT oMemVar)
            oMemVar:Value := uValue
            RETURN TRUE
        ENDIF
        oMemVar := GetHigherLevelPrivate(name)
        IF oMemVar != NULL
            oMemVar:Value := uValue
            RETURN TRUE
        ENDIF
        RETURN FALSE


    /// <summary>Find a private variable. Try on the current level on the stack first and when not found then walk the stack.</summary>
    STATIC METHOD PrivateFind(name AS STRING) AS XSharp.MemVar
        VAR curr := CheckCurrent()
        IF curr != NULL .AND. curr:TryGetValue(name, OUT VAR oMemVar)
            RETURN oMemVar
        ENDIF
        RETURN GetHigherLevelPrivate(name)

    /// <summary>Release a private variable</summary>
    STATIC METHOD Release(name AS STRING) AS VOID
        // release variable
        VAR oMemVar := PrivateFind(name)
        IF oMemVar == NULL
            oMemVar := PublicFind(name)
            IF oMemVar != NULL
                Publics:Remove(oMemVar:Name)
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
        VAR curr := MemVarLevels:Peek()
        IF lCurrentOnly
            IF curr != NULL
                _TempPrivates:AddRange(Current:Keys)
            ENDIF
        ELSE
            FOREACH VAR previous IN MemVarLevels
                IF _TempPrivates:Count == 0
                    _TempPrivates:AddRange(previous:Keys)
                ELSE
                    FOREACH VAR key IN previous:Keys
                        IF !_TempPrivates:Contains(key)
                            _TempPrivates:Add(key)
                        ENDIF
                    NEXT
                ENDIF
            NEXT
        ENDIF
        RETURN _TempPrivates

    /// <summary>Get an enumerator for all the unique names of private variables</summary>
    STATIC METHOD PrivatesEnum(lCurrentOnly := FALSE AS LOGIC) AS IEnumerator<STRING>
        CheckCurrent()
        RETURN _GetUniquePrivates(lCurrentOnly):GetEnumerator()


    /// <summary>Get the first unique private variable name.</summary>
    STATIC METHOD PrivatesFirst(lCurrentOnly := FALSE AS LOGIC) AS STRING
        _PrivatesEnum := PrivatesEnum(lCurrentOnly)
        _PrivatesEnum:Reset()
        RETURN PrivatesNext()


    /// <summary>Get the next unique private variable name.</summary>
    STATIC METHOD PrivatesNext() AS STRING
        IF _PrivatesEnum != NULL
            IF _PrivatesEnum:MoveNext()
                RETURN _PrivatesEnum:Current
            ENDIF
            _PrivatesEnum  := NULL
        ENDIF
        RETURN NULL_STRING


    /// <summary>Get the total number of unique private variable names.</summary>
    STATIC METHOD PrivatesCount(lCurrentOnly := FALSE AS LOGIC) AS INT
        IF MemVarLevels:Count() == 0
            RETURN 0
        ENDIF
        RETURN _GetUniquePrivates(lCurrentOnly):Count



    /// <summary>Assign NIL to all visible private variables. Hidden privates are not affected.</summary>
    STATIC METHOD ReleaseAll() AS VOID
        FOREACH VAR sym IN _GetUniquePrivates(FALSE)
            PrivatePut(sym, NIL)
        NEXT
        RETURN

#endregion

#region Locals

    INTERNAL STATIC METHOD GetHigherLevelLocal(name AS STRING, startDepth AS LONG, uValue OUT USUAL,level OUT MemVarLevel) AS LOGIC
        // Check for locals declared one level up. We do not count Levels created in the runtime
        //
        uValue := NIL
        level  := NULL
        FOREACH VAR previous IN MemVarLevels
            IF previous:Depth >= startDepth
                LOOP
            ENDIF
            IF previous:FindLocal(name, OUT uValue)
                level := previous
                RETURN TRUE
            ENDIF
            IF !previous:SystemLevel
                EXIT
            ENDIF
        NEXT
        RETURN FALSE


    INTERNAL STATIC METHOD LocalFind(name AS STRING, uValue OUT USUAL, level OUT MemVarLevel) AS LOGIC
        level := NULL
        VAR curr := CheckCurrent()
        IF curr == NULL
            uValue := NIL
            RETURN FALSE
        ENDIF
        IF curr:FindLocal(name, OUT uValue)
            level := curr
            RETURN TRUE
        ENDIF
        uValue := NIL
        IF ! curr:SystemLevel
            RETURN FALSE
        ENDIF
        RETURN GetHigherLevelLocal(name, curr:Depth, OUT uValue, OUT level)

    INTERNAL STATIC METHOD ClearLocals() AS VOID
        VAR curr := CheckCurrent()
        IF curr != NULL
            curr:ClearLocals()
        ENDIF
        RETURN

    INTERNAL STATIC METHOD LocalsUpdated() AS LOGIC
        VAR curr := CheckCurrent()
        IF curr != NULL
            RETURN curr:LocalsUpdated
        ENDIF
        RETURN FALSE


    INTERNAL STATIC METHOD LocalPut(name AS STRING, uValue AS USUAL) AS VOID
        VAR curr := CheckCurrent()
        IF curr != NULL
            IF LocalFind(name, OUT VAR _ , OUT VAR level)
                level:SetLocal(name, uValue)
            ENDIF
            curr:SetLocal(name, uValue)
        ENDIF
        RETURN

    INTERNAL STATIC METHOD LocalGet(name AS STRING) AS USUAL STRICT
        VAR curr := CheckCurrent()
        IF curr != NULL
            RETURN curr:GetLocal(name)
        ENDIF
        RETURN NIL


#endregion
#region Generic - Public and Private
    /// <summary>Add a public memvar or a private memvar to the current level.</summary>
    /// <param name="cName">The name of the memory variable</param>
    /// <param name="lPrivate">Should the variable be created as private</param>
    STATIC METHOD Add(cName AS STRING, lPrivate AS LOGIC) AS VOID
        IF lPrivate
            VAR current := CheckCurrent()
            IF current != NULL .AND. !current:ContainsKey(cName)
                IF XSharp.RuntimeState.Dialect == XSharpDialect.FoxPro
                    current:Add(@@MemVar{cName,FALSE})
                ELSE
                    current:Add(@@MemVar{cName,NIL})
                ENDIF
            ENDIF
        ELSE
            BEGIN LOCK Publics
                IF !Publics:ContainsKey(cName)
                    VAR oMemVar := @@MemVar{cName,FALSE}        // publics are always initialized with FALSE
                    Publics:Add(oMemVar )
                ENDIF
            END LOCK
        ENDIF

    /// <summary>Try to retrieve the value of a local, private or public (in that order).</summary>
    /// <param name="cName">The name of the memory variable</param>
    /// <param name="uValue">Value of the variable, or NIL whe the variable does not exist.</param>
    /// <returns>TRUE when the variable was found. Otherwise FALSE</returns>
    STATIC METHOD TryGet(cName AS STRING, uValue OUT USUAL) AS LOGIC
        // Local takes precedence over private
        IF LocalFind(cName, OUT uValue, OUT VAR _)
            RETURN TRUE
        ENDIF
        VAR oMemVar := PrivateFind(cName)
        IF oMemVar == NULL
            oMemVar := PublicFind(cName)
        ENDIF
        IF oMemVar != NULL
            uValue := oMemVar:Value
            RETURN TRUE
        ENDIF
        RETURN FALSE

    /// <inheritdoc cref="M:XSharp.MemVar._GetSafe(System.String)"/>
    STATIC PUBLIC GetSafe AS Getter
    /// <summary>Get the value of a local, private or public (in that order). Returns NIL if the value does not exist.</summary>
    /// <returns>The value of the memory variable or NIL when it does not exist.</returns>
    /// <param name="cName">The name of the memory variable</param>
    /// <remarks>When a private and a public exist with the same name then the private has preference</remarks>
    STATIC METHOD _GetSafe(cName AS STRING) AS USUAL
        // Local takes precedence over private
        IF LocalFind(cName, OUT VAR uValue, OUT VAR _)
            RETURN uValue
        ENDIF
        LOCAL oMemVar AS XSharp.MemVar
        // privates take precedence over publics
        oMemVar := PrivateFind(cName)
        IF oMemVar == NULL
            oMemVar := PublicFind(cName)
        ENDIF
        IF oMemVar != NULL
            RETURN oMemVar:Value
        ENDIF
        RETURN NIL

    /// <inheritdoc cref="M:XSharp.MemVar._Get(System.String)"/>
    STATIC PUBLIC @@Get AS Getter
    /// <summary>Get the value of a local, private or public (in that order). Throws an exception when the variable does not exist.</summary>
    /// <returns>The value of the memory variable.</returns>
    /// <param name="cName">The name of the memory variable</param>
    STATIC METHOD _Get(cName AS STRING) AS USUAL
        // Local takes precedence over private
        IF LocalFind(cName, OUT VAR uValue, OUT VAR _)
            RETURN uValue
        ENDIF
        LOCAL oMemVar AS XSharp.MemVar
        // privates take precedence over publics
        oMemVar := PrivateFind(cName)
        IF oMemVar == NULL
            oMemVar := PublicFind(cName)
        ENDIF
        IF oMemVar != NULL
            RETURN oMemVar:Value
        ENDIF
        VAR err := Error.VOError(EG_NOVAR,"MemVarGet",nameof(cName),1, <OBJECT>{cName})
        THROW err

    /// <inheritdoc cref="M:XSharp.MemVar._Put(System.String,XSharp.__Usual)"/>
    STATIC PUBLIC Put AS Putter
    /// <summary>Updates the value of a local, private or public (in that order).
    /// If the value does not exist than a new variable at the current level is created.</summary>
    /// <returns>The value of the memory variable.</returns>
    /// <param name="cName">The name of the memory variable.</param>
    /// <param name="uValue">The value to assign.</param>
    STATIC METHOD _Put(cName AS STRING, uValue AS USUAL) AS USUAL
        // Local takes precedence over private
        IF LocalFind(cName, OUT VAR _, OUT VAR level)
            level:UpdateLocal(cName, uValue)
            RETURN uValue
        ENDIF
        LOCAL oMemVar AS XSharp.MemVar
        // assign to existing memvar first
        // privates take precedence over publics ?
        oMemVar := PrivateFind(cName)
        IF oMemVar == NULL
            oMemVar := PublicFind(cName)
        ENDIF
        IF oMemVar != NULL
            BEGIN LOCK oMemVar
                oMemVar:Value := uValue
            END LOCK
        ELSE
            // memvar does not exist, then add it at the current level
            VAR current := CheckCurrent()
            IF current != NULL
                current:Add(@@MemVar{cName,uValue})
            ENDIF
        ENDIF
        RETURN uValue

    /// <summary>Clear all memvar name/value pairs.
    /// Does not remove locals. Does not remove privates stack levels.</summary>
    STATIC METHOD ClearAll() AS VOID
        // Does not clear the privates stack levels
        BEGIN LOCK Publics
            Publics:Clear()
        END LOCK
        FOREACH VAR level IN MemVarLevels
            level:Clear()
        NEXT

    /// <summary>Clear a variable by name. Tries to clear a private first and when that is not found then a public</summary>
    /// <param name="cName">The name of the memory variable.</param>
    /// <remarks>Clear does not remove the variable but sets its value to NIL.<br />
    /// In the FoxPro dialect Clear does not clear local variables when these are visible.</remarks>
    STATIC METHOD Clear(cName AS STRING) AS LOGIC
        VAR oMemVar := PrivateFind(cName)
        IF oMemVar == NULL
            oMemVar := PublicFind(cName)
        ENDIF
        IF oMemVar != NULL
            oMemVar:Value := NIL
        ELSE
            THROW Exception{"Variable "+cName+" does not exist"}
        ENDIF
        RETURN TRUE

#endregion

#region Publics


    /// <summary>Find a public variable</summary>
    /// <param name="cName">The name of the memory variable.</param>
    STATIC METHOD PublicFind(cName AS STRING) AS XSharp.MemVar
        BEGIN LOCK Publics
            IF Publics:TryGetValue(cName, OUT VAR oMemVar)
                RETURN oMemVar
            ENDIF
        END LOCK
        RETURN NULL


    /// <summary>Update a public variable. Does NOT create a new public when there is no variable with that name.</summary>
    /// <param name="cName">The name of the memory variable.</param>
    /// <param name="uValue">The value to assign.</param>
    STATIC METHOD PublicPut(cName AS STRING, uValue AS USUAL) AS LOGIC
        VAR oMemVar := PublicFind(cName)
        IF oMemVar != NULL
            BEGIN LOCK oMemVar
                oMemVar:Value := uValue
            END LOCK
            RETURN TRUE
        ENDIF
        RETURN FALSE

    /// <summary>Get an enumerator for all the unique names of public variables</summary>
    STATIC METHOD PublicsEnum() AS IEnumerator<STRING>
        BEGIN LOCK Publics
            RETURN Publics:Keys:GetEnumerator()
        END LOCK


    /// <summary>Gets the name of the first public variable.</summary>
    STATIC METHOD PublicsFirst() AS STRING
        _PublicsEnum := PublicsEnum()
        _PublicsEnum:Reset()
        RETURN PublicsNext()

    /// <summary>Gets the name of the next public variable.</summary>
    STATIC METHOD PublicsNext() AS STRING
        IF _PublicsEnum != NULL
            IF _PublicsEnum:MoveNext()
                RETURN _PublicsEnum:Current
            ENDIF
            _PublicsEnum := NULL
        ENDIF
        RETURN NULL_STRING

    /// <summary>Gets the total number of public variables.</summary>
    STATIC METHOD PublicsCount() AS INT
        BEGIN LOCK Publics
            RETURN Publics:Count
        END LOCK

#endregion
END CLASS
