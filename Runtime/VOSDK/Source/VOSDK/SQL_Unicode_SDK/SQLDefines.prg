//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

#region defines
DEFINE _assert  := TRUE
DEFINE __CAVO_SQL_TXN_READ_COMMITTED := SQL_TXN_READ_COMMITTED
   //
   // Connect option: Isolation level between concurring accesses.
   // Possible values are: (not all are supported by any driver)
   //
   // SQL_TXN_READ_UNCOMMITTED
   // SQL_TXN_READ_COMMITTED
   // SQL_TXN_REPEATABLE_READ
   // SQL_TXN_SERIALIZABLE
   // SQL_TXN_VERSIONING
   //
DEFINE __CAVO_SQL_SIMULATE_CURSOR := SQL_SC_TRY_UNIQUE
   //
   // SQL_SIMULATE_CURSOR options. Possible values are:
   //
   // SQL_SC_NON_UNIQUE
   // SQL_SC_TRY_UNIQUE
   // SQL_SC_UNIQUE
   //
DEFINE __CAVO_SQL_CURSOR_TYPE := SQL_CURSOR_KEYSET_DRIVEN
   //
   // SQL_CURSOR_TYPE options. Possible values are:
   //
   // SQL_CURSOR_FORWARD_ONLY
   // SQL_CURSOR_KEYSET_DRIVEN
   // SQL_CURSOR_DYNAMIC
   // SQL_CURSOR_STATIC
   //
DEFINE __CAVO_SQL_CONCURRENCY := SQL_CONCUR_ROWVER
   //
   // SQL_CONCURRENCY options. Possible values are:
   //
   // SQL_CONCUR_READ_ONLY
   // SQL_CONCUR_LOCK
   // SQL_CONCUR_ROWVER
   // SQL_CONCUR_VALUES
   //
// DEFINE __CAVO_SQL_ODBC_CURSORS := SQL_CUR_USE_IF_NEEDED
DEFINE __CAVO_SQL_ODBC_CURSORS := SQL_CUR_USE_IF_NEEDED    
   //
   // Connect option: Use ODBC or driver cursors.
   // Possible values are:
   //
   // SQL_CUR_USE_IF_NEEDED      my suggestion
   // SQL_CUR_USE_ODBC           CAVO 2.0 default
   // SQL_CUR_USE_DRIVER
   //
   // Notes:
   //
DEFINE __CAVO_SQL_MODE_READ_WRITE := SQL_MODE_READ_WRITE
   //
   // Connect option: Allow read only or read and write access.
   // Possible values are:
   //
   // SQL_MODE_READ_ONLY
   // SQL_MODE_READ_WRITE
   //
DEFINE MAX_CONNECT_INFO_STRING  := 256
DEFINE MAX_LONGVARCHAR          := 32000
DEFINE MAX_LONGVARCHAR_EXT      := 100000
DEFINE MAX_COLNAME_SIZE         := 40
DEFINE SQL_RELOP_AND            :=   1
DEFINE SQL_RELOP_OR             :=   2
DEFINE SQL_RELOP_NOT            :=   3
DEFINE SQL_RELOP_OPENP          :=   4
DEFINE SQL_RELOP_CLOSEP         :=   5
// defines for 'scroll concurrency for update' types ( siScrollUpdateType )
DEFINE SQL_SC_UPD_AUTO          :=   0
DEFINE SQL_SC_UPD_CURSOR        :=   1
DEFINE SQL_SC_UPD_KEY           :=   2
DEFINE SQL_SC_UPD_VALUE         :=   3
DEFINE SQL_DATA_DELETE := 1
DEFINE SQL_DATA_BUFFER := 2
DEFINE SQL_DATA_NULL   := 3
DEFINE SQL_LOGICAL_TRUE     := 0x31
DEFINE SQL_LOGICAL_FALSE    := 0x30
DEFINE SQL_BLANK_CHARACTER  := 0x20
DEFINE __CAVOSTR_SQLCLASS__QE_LIC   := "IVC3.LIC"
DEFINE __CAVOSTR_SQLCLASS__QE_PSWD  := "jys777JE04nN13Ef52eR4x3b"
DEFINE __CAVOSTR_SQLCLASS__DSN      := "DSN="
DEFINE __CAVOSTR_SQLCLASS__UID      := "UID="
DEFINE __CAVOSTR_SQLCLASS__PWD      := "PWD="
DEFINE __CAVOSTR_SQLCLASS__IS_NULL      := " IS NULL"
DEFINE __CAVOSTR_SQLCLASS__EQ_NULL      := " = NULL"
DEFINE __CAVOSTR_SQLCLASS__NULL         := " NULL"
DEFINE __CAVOSTR_SQLCLASS__EQ           := " ="
DEFINE __CAVOSTR_SQLCLASS__1_QUOTE      := "'"
DEFINE __CAVOSTR_SQLCLASS__2_QUOTE      := "''"
DEFINE __CAVOSTR_SQLCLASS__SELECT       := "select "
DEFINE __CAVOSTR_SQLCLASS__FROM         := " from "
DEFINE __CAVOSTR_SQLCLASS__UPDATE       := "update "
DEFINE __CAVOSTR_SQLCLASS__SET          := " set "
DEFINE __CAVOSTR_SQLCLASS__CURR_OF      := " where current of "
DEFINE __CAVOSTR_SQLCLASS__WHERE        := " where "      
DEFINE __CAVOSTR_SQLCLASS__AND          := " and "        
DEFINE __CAVOSTR_SQLCLASS__DEL_FROM     := "delete from "  
DEFINE __CAVOSTR_SQLCLASS__INS_INTO     := "insert into "  
DEFINE __CAVOSTR_SQLCLASS__VALUES       := ") values ("    
DEFINE __CAVOSTR_SQLCLASS__ORDER_BY     := " order by "      
#endregion
