//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp.RDD
BEGIN NAMESPACE XSharp

 /// <include file="XSharp.Core.Docs.xml" path="doc/IDate/*" />
	INTERFACE IDate
  /// <include file="XSharp.Core.Docs.xml" path="doc/IDate.Year/*" />
		PROPERTY Year		AS INT GET
  /// <include file="XSharp.Core.Docs.xml" path="doc/IDate.Month/*" />
		PROPERTY Month		AS INT GET
  /// <include file="XSharp.Core.Docs.xml" path="doc/IDate.Day/*" />
		PROPERTY Day		AS INT GET
  /// <include file="XSharp.Core.Docs.xml" path="doc/IDate.Value/*" />
		PROPERTY @@Value		AS DateTime GET
  /// <include file="XSharp.Core.Docs.xml" path="doc/IDate.IsEmpty/*" />
		PROPERTY IsEmpty	AS LOGIC GET
	END INTERFACE

 /// <include file="XSharp.Core.Docs.xml" path="doc/IFloat/*" />
	INTERFACE IFloat
  /// <include file="XSharp.Core.Docs.xml" path="doc/IFloat.Value/*" />
		property @@Value    as real8 get
  /// <include file="XSharp.Core.Docs.xml" path="doc/IFloat.Digits/*" />
		PROPERTY Digits	  AS INT  GET
  /// <include file="XSharp.Core.Docs.xml" path="doc/IFloat.Decimals/*" />
		PROPERTY Decimals AS INT  GET
	END INTERFACE

    INTERFACE IClosedRDD
        PROPERTY Closed as LOGIC GET
    END INTERFACE
    /// <include file="XSharp.Core.Docs.xml" path="doc/ICurrency/*" />
    interface ICurrency
        /// <include file="XSharp.Core.Docs.xml" path="doc/ICurrency.Value/*" />
        property @@Value    as System.Decimal	get
    end interface

END NAMESPACE
