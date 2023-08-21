//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Management
USING System.Drawing.Printing
USING System.Windows.Forms


/// <include file="VFPDocs.xml" path="Runtimefunctions/aprinters/*" />
FUNCTION APrinters ( ArrayName , nValue ) AS INT CLIPPER
LOCAL cPrinterName AS STRING
LOCAL iCount AS INT

	Default ( @nValue , 0 )

	IF ( iCount := PrinterSettings.InstalledPrinters:Count ) > 0

		IF ! IsArray( ArrayName ) .or. ( IsArray ( ArrayName ) .and. ! ArrayName IS __FoxArray )
			THROW ArgumentException {"parameter must be a Fox Array", nameof ( ArrayName ) }
		ELSEIF ! IsNumeric ( nValue )
			THROW ArgumentException {"wrong parameter type, must be numeric", nameof ( nValue ) }
		ELSEIF nValue < 0 .or. nValue > 1
			THROW ArgumentException {"parameter value must be either 0 or 1", nameof ( nValue ) }
		ENDIF

		nValue := (INT) nValue  // note: VFP allows to pass a float in the range from 0.00 to 1.00 !


		IF nValue > 0
			ArrayName := __FoxRedim(ArrayName, (DWORD) iCount, 5 )
		ELSE
			ArrayName := __FoxRedim(ArrayName, (DWORD) iCount, 2 )
		ENDIF


		FOR VAR i := 0 TO iCount - 1

			cPrinterName := PrinterSettings.InstalledPrinters[i]
			VAR oProperty := ManagementObject{"Win32_Printer.DeviceID='" + cPrinterName + "'"}

			ArrayName [ i + __ARRAYBASE__ , __ARRAYBASE__ ]     := cPrinterName
			ArrayName [ i + __ARRAYBASE__ , __ARRAYBASE__ + 1 ] := IIF ( oProperty["PortName"] == NULL , "" , oProperty["PortName"]   )

			IF nValue > 0
				ArrayName [ i + __ARRAYBASE__ ,__ARRAYBASE__ + 2  ] := IIF ( oProperty["DriverName"] == NULL , "" , oProperty["DriverName"] )
				ArrayName [ i + __ARRAYBASE__ ,__ARRAYBASE__ + 3  ] := IIF ( oProperty["Comment"]    == NULL , "" , oProperty["Comment"] )
				ArrayName [ i + __ARRAYBASE__ ,__ARRAYBASE__ + 4  ] := IIF ( oProperty["Location"]   == NULL , "" , oProperty["Location"] )
			ENDIF

		NEXT

	ENDIF

	RETURN iCount
