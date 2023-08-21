//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="VFPDocs.xml" path="Runtimefunctions/payment/*" />
FUNCTION Payment( nPrincipal AS FLOAT , nInterestRate AS FLOAT , nPayments AS FLOAT ) AS FLOAT

	// ensures that both values are always positive
	nInterestRate := (FLOAT) System.Math.Abs( nInterestRate )
	nPayments := (FLOAT) System.Math.Abs( nPayments )

	// note: something like >= 12.5 becomes 13, otherwise 12
	nPayments := (FLOAT) System.Math.Round(nPayments,MidpointRounding.AwayFromZero)

   	RETURN nPrincipal * (nInterestRate * ( (1 + nInterestRate)^nPayments) ) / (((1+ nInterestRate)^nPayments) - 1)

/// <include file="VFPDocs.xml" path="Runtimefunctions/pv/*" />
FUNCTION PV ( nPayment AS FLOAT , nInterestRate AS FLOAT , nTotalPayments AS FLOAT ) AS FLOAT

	// ensures that both values are always positive
 	nInterestRate := (FLOAT) System.Math.Abs( nInterestRate )
	nTotalPayments := (FLOAT) System.Math.Abs( nTotalPayments )

	// note: something like >= 12.5 becomes 13, otherwise 12
	nTotalPayments := (FLOAT) System.Math.Round(nTotalPayments,MidpointRounding.AwayFromZero)

	RETURN nPayment * (1 - (1 + nInterestRate)^-nTotalPayments)/nInterestRate

/// <include file="VFPDocs.xml" path="Runtimefunctions/fv/*" />
FUNCTION FV ( nPayment AS FLOAT , nInterestRate AS FLOAT , nPeriods AS FLOAT ) AS FLOAT

	// ensures that both values are always positive
	nInterestRate := (FLOAT) System.Math.Abs( nInterestRate )
	nPeriods := (FLOAT) System.Math.Abs( nPeriods )

	// note: something like >= 12.5 becomes 13, otherwise 12
	nPeriods := (FLOAT) System.Math.Round(nPeriods,MidpointRounding.AwayFromZero)

	RETURN nPayment * ((1 + nInterestRate)^nPeriods -1) / nInterestRate

