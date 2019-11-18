// Copyright (c) 2001-2005 Extended Systems, Inc.
// Portions Copyright (c) 2005-2010, iAnywhere Solutions, Inc.
// All rights reserved. All unpublished rights reserved.
//
// This source code can be used, modified, or copied by the licensee as long as
// the modifications (or the new binary resulting from a copy or modification of
// this source code) are used with Extended Systems' products. The source code
// is not redistributable as source code, but is redistributable as compiled
// and linked binary code. If the source code is used, modified, or copied by
// the licensee, Extended Systems Inc. reserves the right to receive from the
// licensee, upon request, at no cost to Extended Systems Inc., the modifications.
//
// Extended Systems Inc. does not warrant that the operation of this software
// will meet your requirements or that the operation of the software will be
// uninterrupted, be error free, or that defects in software will be corrected.
// This software is provided "AS IS" without warranty of any kind. The entire
// risk as to the quality and performance of this software is with the purchaser.
// If this software proves defective or inadequate, purchaser assumes the entire
// cost of servicing or repair. No oral or written information or advice given
// by an Extended Systems Inc. representative shall create a warranty or in any
// way increase the scope of this warranty.


//#include "VOSystemLibrary.vh"
// #include "dbfaxs.vh"

CLASS AdsSQLServer INHERIT DBServer

CONSTRUCTOR( oFile, lShareMode, lReadOnlyMode, xDriver, aRdd )
    LOCAL cTemp AS STRING
    LOCAL cFileName AS STRING

    // Set the query text, this is necessary because the VO runtime doesn't like
    // some of the special characters that are used in SQL queries
    RDDINFO( _SET_SQL_QUERY, oFile )

    // Some VO libraries have trouble with the alias as is.  So for the SQL RDDS,
    // just grab the first word of the SQL query and use it as the alias.  The VO
    // runtime will adjust it to be unique if there is a naming conflict.
    cTemp := Left( oFile, At( " ", oFile ) - 1 )

    // Call the DBServer init method which will execute the query
    SUPER( cTemp, lShareMode, lReadOnlyMode, xDriver, aRdd )

    // Now that the query is executed, fixup some member variables that couldn't
    // be set properly before the query was executed.  Only do this if the table
    // handle is set (i.e. there is a result set and thus a table in the DBServer)
    IF SELF:Info( DBI_GET_ACE_TABLE_HANDLE ) <> IntPtr.Zero
        oFileSpec := FileSpec{ SELF:Info( DBI_FULLPATH ) }
        cFileName := oFileSpec:FileName

        cTemp := Symbol2String( ClassName( SELF ) )
        oHyperLabel := HyperLabel{ cFileName, cFileName,  ;
            cTemp + ": " + cFileName + " Alias=" +  ;
            cTemp + "_" + cFileName }
    ENDIF

RETURN

END CLASS




