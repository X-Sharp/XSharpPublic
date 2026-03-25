//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp
  /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType/*" />
  ENUM __UsualType AS BYTE
        // These numbers must match with the types defined in the compiler
        // They also match with the USUAL types in VO (BaseType.h)
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Void/*" />
        MEMBER Void		:=0
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Long/*" />
        MEMBER Long		:=1
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Date/*" />
        MEMBER Date		:=2
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Float/*" />
        MEMBER Float	:=3
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Fixed/*" />
        MEMBER Fixed    := 4
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Array/*" />
        MEMBER Array	:=5
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Object/*" />
        MEMBER Object	:=6
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.String/*" />
        MEMBER String	:=7
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Logic/*" />
        MEMBER Logic	:=8
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Codeblock/*" />
        MEMBER Codeblock:=9
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Symbol/*" />
        MEMBER Symbol	:=10
        // see below for missing values
        // The follow numbers are defined but never stored inside a USUAL in VO and Vulcan
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Byte/*" />
        MEMBER Byte		:=11
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.ShortInt/*" />
        MEMBER ShortInt	:=12
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Word/*" />
        MEMBER Word		:=13
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.DWord/*" />
        MEMBER DWord	:=14
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Real4/*" />
        MEMBER Real4	:=15
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Real8/*" />
        MEMBER Real8	:=16
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Psz/*" />
        MEMBER Psz		:=17
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Ptr/*" />
        MEMBER Ptr		:=18
        /// <exclude/>
        MEMBER Usual	:=19	// USUAL by Ref, not implemented in Vulcan

        // 20 and 21 not used

        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Int64/*" />
        MEMBER Int64		:=22
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.UInt64/*" />
        MEMBER UInt64     :=23
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Char/*" />
        MEMBER Char		:=24    // not stored in a usual
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Dynamic/*" />
        MEMBER Dynamic    :=25
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.DateTime/*" />
        MEMBER DateTime	:=26
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Decimal/*" />
        MEMBER Decimal	:=27
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Currency/*" />
        MEMBER Currency	:=28
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Binary/*" />
        MEMBER Binary	:=29
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Null/*" />
        MEMBER Null	    := 30
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Memo/*" />
        MEMBER Memo		:=32	// Used in RDD system in VO
        /// <include file="XSharp.Core.Docs.xml" path="doc/__UsualType.Invalid/*" />
        MEMBER Invalid    :=99
    END ENUM

END NAMESPACE // global::XSharp.Types


/// <include file="XSharp.Core.Docs.xml" path="doc/SystemTypeToUsualType/*" />
FUNCTION SystemTypeToUsualType(oType as System.Type) AS __UsualType
switch Type.GetTypeCode(oType)
    case TypeCode.Boolean
      return __UsualType.Logic
    case TypeCode.Byte
      return __UsualType.Byte
    case TypeCode.Char
      return __UsualType.Char
    case TypeCode.DateTime
      return __UsualType.DateTime // new in X#
    case TypeCode.DBNull
      return __UsualType.Null    // new in X#
    case TypeCode.Decimal
      return __UsualType.Decimal // new in X#
    case TypeCode.Double
      return __UsualType.Real8
    case TypeCode.Empty
      return __UsualType.Void
    case TypeCode.Int16
      return __UsualType.ShortInt
    case TypeCode.Int32
      return __UsualType.Long
    case TypeCode.Int64
      return __UsualType.Int64
    case TypeCode.SByte
      return __UsualType.Byte
    case TypeCode.Single
      return __UsualType.Real4
    case TypeCode.UInt16
      return __UsualType.Word
    case TypeCode.UInt32
      return __UsualType.DWord
    case TypeCode.UInt64
      return __UsualType.UInt64
    case TypeCode.String
      return __UsualType.String
    otherwise
        // use the type names because the types are defined in another assembly
        switch oType:FullName:ToLower()
        case "xsharp.__array"
          return __UsualType.Array
        case "xsharp.__binary"
          return __UsualType.Binary // new in X#
        case "xsharp.__codeblock"
          return __UsualType.Codeblock
        case "xsharp.__currency"
          return __UsualType.Currency // new in X#
        case "xsharp.__date"
          return __UsualType.Date
        case "xsharp.__float"
          return __UsualType.Float
        case "xsharp.__psz"
          return __UsualType.Psz
        case "xsharp.__symbol"
          return __UsualType.Symbol
        case "xsharp.__usual"
          return __UsualType.Usual
        case "system.intptr"
          return __UsualType.Ptr

      end switch
    end switch
    return __UsualType.Void
