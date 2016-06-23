//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// The attributes and methods in this code are used by the compiler when generating
// code for the VO and Vulcan Language

USING System
USING System.Collections.Generic
USING System.Text
USING System.Runtime.InteropServices

BEGIN NAMESPACE Vulcan
    ///
    ///<Summary>This class may be used to help the compile and CreateInstance function to locate a class in a namespace</Summary>
    ///
    [AttributeUsage(AttributeTargets.Assembly)];
    PUBLIC SEALED CLASS VulcanImplicitNameSpaceAttribute INHERIT System.Attribute
        PRIVATE _nameSpace as STRING
        PUBLIC CONSTRUCTOR(nameSpace as STRING)
            SUPER()
            _nameSpace := nameSpace
            RETURN
        PUBLIC PROPERTY Namespace AS STRING GET _nameSpace
    END CLASS
END NAMESPACE

BEGIN NAMESPACE Vulcan.Internal
    ///
    ///<Summary>This attribute is used to annotate a Clipper Calling Convention method with its parameter names</Summary>
    ///
    [AttributeUsage(AttributeTargets.Method)];
    PUBLIC SEALED CLASS ClipperCallingConventionAttribute INHERIT System.Attribute
        PRIVATE _parameterNames as STRING[]
        PUBLIC CONSTRUCTOR(parameterNames as STRING[])
            SUPER()
            _parameterNames := parameterNames
            RETURN
        PUBLIC PROPERTY ParameterNames as STRING[] GET _parameterNames
    END CLASS

    ///
    ///<Summary>This attribute is used to register the Functions Class and Default namespace for an assembly
    // The compiler uses this to locate functions (USING STATIC NamespaceName) and classes </Summary>
    ///
    [AttributeUsage(AttributeTargets.Assembly)];
    PUBLIC SEALED CLASS VulcanClassLibraryAttribute INHERIT System.Attribute
        PRIVATE _defaultNamespace as STRING
        PRIVATE _globalClassName as STRING
        PUBLIC CONSTRUCTOR(globalClassName as STRING, defaultNameSpace as STRING)
            SUPER()
            _globalClassName := globalClassName
            _defaultNameSpace := defaultNameSpace
            RETURN
        PUBLIC PROPERTY defaultNameSpace as STRING GET _defaultNameSpace
        PUBLIC PROPERTY globalClassName  as STRING GET _globalClassName
    END CLASS

    ///
    ///<Summary>This class is used to register the compiler version used to create an assembly</Summary>
    ///
    [AttributeUsage(AttributeTargets.Assembly)];
    PUBLIC SEALED CLASS VulcanCompilerVersionAttribute INHERIT System.Attribute
        PRIVATE _version as STRING
        PUBLIC CONSTRUCTOR(version as STRING)
            SUPER()
            _version := version
            RETURN
        PUBLIC PROPERTY version as STRING GET _version
    END CLASS

    ///
    ///<Summary>This class is used by the BREAK statement to send an arbitrary value to the RECOVER clause of a BEGIN SEQUENCE</Summary>
    ///
    PUBLIC CLASS VulcanWrappedException INHERIT System.Exception
        PRIVATE _value as OBJECT
        PUBLIC CONSTRUCTOR(value as OBJECT)
            SUPER()
            _value := value
        PUBLIC PROPERTY value as OBJECT GET _value SET _value := value
    END CLASS

    PUBLIC STATIC CLASS CompilerServices
        #region Sequence Support
        INTERNAL STATIC SequenceLevel := 0 as INT
        ///
        ///<Summary>This methid is used to keep track of the BEGIN SEQUENCE nesting level</Summary>
        ///
        PUBLIC STATIC METHOD EnterBeginSequence() AS VOID
            SequenceLevel ++
            RETURN
        ///
        ///<Summary>This methid is used to keep track of the BEGIN SEQUENCE nesting level</Summary>
        ///
        PUBLIC STATIC METHOD ExitBeginSequence() AS VOID
            SequenceLevel --
            RETURN
        ///
        ///<Summary>This property returns TRUE when a BEGIN SEQUENCE is active</Summary>
        ///
        PUBLIC STATIC PROPERTY CanBreak as LOGIC GET SequenceLevel > 0
        #endregion

        #region PSZ Support
        ///
        ///<Summary>This method is called in stead of a StringPsz() function call to allocate a PSZ and add it to a list that can be cleaned
        // at exit of the method/function where the PSZ is used.</Summary>
        ///
        PUBLIC STATIC UNSAFE METHOD String2Psz(sText as STRING, pszList as List<IntPtr>) AS IntPtr
            LOCAL ptrPsz  as IntPtr
            IF (sText == NULL || sText:Length == 0)
                ptrPsz  := Marshal.StringToHGlobalAnsi("")
            ELSE
                ptrPsz  := Marshal.StringToHGlobalAnsi(sText)
            ENDIF
            pszList:Add(ptrPsz)
            RETURN ptrPsz

        ///
        ///<Summary>This method is called from the Finally clause in a method that allocates PSZs to release them</Summary>
        ///
        PUBLIC STATIC UNSAFE METHOD String2PszRelease(pszList as List<IntPtr>) AS VOID
            IF (pszList != NULL)
                FOREACH var ptrPsz in pszList
                    TRY
                        Marshal.FreeHGlobal(ptrPsz)
                    CATCH 
                    END TRY

                NEXT
            ENDIF
        ///
        ///<Summary>This method is used to implement the String Subtraction operation</Summary>
        ///
        PUBLIC STATIC METHOD StringSubtract(lhs as STRING, rhs as STRING) AS STRING
            LOCAL nTotalLength as INT
            IF lhs != NULL .and. rhs != NULL
                nTotalLength := lhs:Length + rhs:Length
                RETURN (lhs:TrimEnd()+rhs:TrimEnd()):PadRight(nTotalLength,' ')
            ELSEIF lhs != NULL
                RETURN lhs
            ELSEIF rhs != NULL
                RETURN rhs
            ENDIF
            RETURN NULL
        #endregion
    END CLASS


END NAMESPACE

