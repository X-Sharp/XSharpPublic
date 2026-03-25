//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections
USING System.Collections.Generic
USING System.Linq
USING System.Diagnostics

USING System.Runtime.CompilerServices

#include "attributes.xh"

/// <include file="XSharp.RT.Docs.xml" path="doc/Codeblock/*" />
//[DebuggerDisplay( "{ToString(),nq}", Type := "CODEBLOCK" )] ;
ABSTRACT CLASS XSharp.Codeblock IMPLEMENTS ICodeblock2
    PRIVATE INITONLY _pcount AS INT

    [NOSHOW] PRIVATE STATIC nullArgs AS USUAL[]

    STATIC CONSTRUCTOR
        nullArgs := USUAL[]{0}
        RETURN

    /// <include file="XSharp.RT.Docs.xml" path="doc/Codeblock.PCount/*" />
    PUBLIC VIRTUAL METHOD PCount AS INT
        RETURN _pcount

/// <include file="XSharp.RT.Docs.xml" path="doc/Codeblock.ctor/*" />
    [NODEBUG] [INLINE];
    PROTECTED CONSTRUCTOR (pCount AS INT)
        _pcount := pCount

    /// <include file="XSharp.RT.Docs.xml" path="doc/Codeblock.Eval/*" />
    PUBLIC ABSTRACT METHOD Eval(args PARAMS CONST USUAL[] ) AS USUAL

    /// <inheritdoc />
    PROPERTY ResultType as __UsualType AUTO

    /// <include file="XSharp.RT.Docs.xml" path="doc/Codeblock.EvalBlock/*" />
    [DebuggerStepThrough()];
    PUBLIC VIRTUAL METHOD EvalBlock(args PARAMS OBJECT[] ) AS OBJECT
        LOCAL uArgs as USUAL[]
        if args:Length == 0
            uArgs := nullArgs
        ELSE
            uArgs := args:ToUsualArray()
        ENDIF
        var result := SELF:Eval(uArgs)
        SELF:ResultType := result:Type
        IF RuntimeState.Dialect == XSharpDialect.FoxPro
            if result:IsNull
                result := DBNull.Value
            ELSEIF IsNil(result)
                result := FALSE
            ENDIF
        ENDIF
        RETURN result


    /// <include file="XSharp.RT.Docs.xml" path="doc/Codeblock.ToString/*" />
    PUBLIC OVERRIDE METHOD ToString() AS STRING
        RETURN "{|" + SELF:_pcount:ToString() + "| ... }"

        // This method is used in the compiled codeblocks to get the arguments
        // from the parameter list
    /// <exclude />
    [DebuggerStepThrough()];
    PROTECTED STATIC METHOD _BlockArg( args AS CONST USUAL[], index AS CONST INT ) AS USUAL
        RETURN IIF( index < args:Length, args[index + 1], NIL )
END CLASS


/// <include file="XSharp.RT.Docs.xml" path="doc/_Codeblock/*" />
[DebuggerDisplay( "{_cMacro}", Type := "_Codeblock" )] ;
PUBLIC CLASS XSharp._Codeblock INHERIT XSharp.Codeblock IMPLEMENTS IRtCodeblock
    /// <exclude />
    INITONLY PROTECT _innerBlock AS ICodeblock
    /// <exclude />
    INITONLY PROTECT _cMacro		AS STRING
    /// <exclude />
    INITONLY PROTECT _lIsBlock   AS LOGIC
    /// <exclude />
    INITONLY PROTECT _addsMemVars AS LOGIC

    [NOSHOW] STATIC PRIVATE nullArgs AS OBJECT[]

    STATIC CONSTRUCTOR
        nullArgs := OBJECT[]{0}
        RETURN

    /// <include file="XSharp.RT.Docs.xml" path="doc/_Codeblock.ctor/*" />
    PUBLIC CONSTRUCTOR(innerBlock AS ICodeblock, cMacro AS STRING, lIsBlock AS LOGIC, lAddsMemvars AS LOGIC)
        SUPER(IIF (lIsBlock, innerBlock:PCount(), -1))
        _innerBlock := innerBlock
        _cMacro		:= cMacro
        _lIsBlock   := lIsBlock
        _addsMemVars := lAddsMemvars

    /// <include file="XSharp.RT.Docs.xml" path="doc/_Codeblock.Eval/*" />
    PUBLIC OVERRIDE METHOD Eval(args PARAMS USUAL[]) AS USUAL
        LOCAL uRes      AS USUAL
        LOCAL oRes      AS OBJECT
        LOCAL iLevel    AS INT
        IF _addsMemVars
            iLevel := XSharp.MemVar.InitPrivates(TRUE)
        ELSE
            iLevel := 0
        ENDIF
        TRY
            LOCAL oArgs as OBJECT[]
            if args:Length == 0
                oArgs := nullArgs
            ELSE
                oArgs := args:ToObjectArray()
            ENDIF
            oRes := SELF:_innerBlock:EvalBlock(oArgs)
            uRes := __Usual{oRes}
            SELF:ResultType := if(oRes == NULL , __UsualType.Void, uRes:Type)
        FINALLY
            IF _addsMemVars
                XSharp.MemVar.ReleasePrivates(iLevel)
            ENDIF
        END TRY
        RETURN uRes

    /// <include file="XSharp.RT.Docs.xml" path="doc/_Codeblock.ToString/*" />
    PUBLIC OVERRIDE METHOD ToString() AS STRING
        IF _lIsBlock
            RETURN _cMacro
        ELSE
            RETURN "{|| "+_cMacro+" }"
        ENDIF

    /// <include file="XSharp.RT.Docs.xml" path="doc/_Codeblock.IsBlock/*" />
    PUBLIC PROPERTY IsBlock AS LOGIC GET _lIsBlock
END CLASS

