//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Runtime.InteropServices

BEGIN NAMESPACE XSharp.ADS
INTERNAL STATIC CLASS ACE64
	[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
	internal static extern METHOD AdsAddCustomKey(hIndex as IntPtr ) as DWORD 

	[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
	internal static extern METHOD AdsAppendRecord(hTable as IntPtr ) as DWORD 

	[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
	internal static extern METHOD AdsApplicationExit() as DWORD 

	[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
	internal static extern METHOD AdsAtBOF(hTable as IntPtr , pbBof out WORD ) as DWORD 

	[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
	internal static extern METHOD AdsAtEOF(hTable as IntPtr , pbEof out WORD ) as DWORD 

	[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
	internal static extern METHOD AdsBeginTransaction(hConnect as IntPtr) as DWORD 

	[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
	internal static extern METHOD AdsBinaryToFile(hTable as IntPtr , pucFldName as string , pucFileName as string ) as DWORD 

	[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
	internal static extern METHOD AdsBinaryToFile(hTable as IntPtr , lFieldOrdinal as DWORD, pucFileName as string ) as DWORD 

	[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
	internal static extern METHOD AdsCacheOpenCursors(usOpen as WORD) as DWORD 

	[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
	internal static extern METHOD AdsCacheOpenTables(usOpen as WORD) as DWORD 

	[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
	internal static extern METHOD AdsCacheRecords(hTable as IntPtr , usNumRecords as WORD ) as DWORD 

	[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
	internal static extern METHOD AdsCancelUpdate(hTable as IntPtr ) as DWORD 

	[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
	internal static extern METHOD AdsCancelUpdate90(hTable as IntPtr , ulOptions as DWORD) as DWORD 

	[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
	internal static extern METHOD AdsCheckExistence(hConnect as IntPtr, pucFileName as string , pusOnDisk out WORD) as DWORD 

	[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
	internal static extern METHOD AdsClearAllScopes(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsClearDefault() as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsClearFilter(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsClearRelation(hTableParent as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsClearScope(hIndex as IntPtr , usScopeOption as WORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCloneTable(hTable as IntPtr , phClone out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCloseAllIndexes(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCloseAllTables() as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCloseIndex(hIndex as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCloseTable(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCloseCachedTables(hConnection as IntPtr) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCommitTransaction(hConnect as IntPtr) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsConnect(pucServerName as string , phConnect out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsConnect26(pucServerName as string , usServerTypes as WORD, phConnect out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsConnect60(pucServerPath as string , usServerTypes as WORD, pucUserName as string , pucPassword as string , ulOptions as DWORD, phConnect out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsConnectionAlive(hConnect as IntPtr, pbConnectionIsAlive out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsContinue(hTable as IntPtr , pbFound out WORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsConvertTable(hObj as IntPtr ,  usFilterOption as WORD, pucFile as string , usTableType as WORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCopyTable(hObj as IntPtr , usFilterOption as WORD , pucFile as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCopyTableContents(hObjFrom as IntPtr , hTableTo as IntPtr , usFilterOption as WORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCopyTableStructure(hTable as IntPtr , pucFile as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCreateIndex(hObj as IntPtr , pucFileName as string , pucTag as string , pucExpr as string , pucCondition as string , pucWhile as string , ulOptions as DWORD, phIndex out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCreateIndex61(hObj as IntPtr , pucFileName as string , pucTag as string , pucExpr as string , pucCondition as string , pucWhile as string , ulOptions as DWORD, ulPageSize as DWORD , phIndex out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCreateIndex90(hObj as IntPtr , pucFileName as string , pucTag as string , pucExpr as string , pucCondition as string , pucWhile as string , ulOptions as DWORD, ulPageSize as DWORD , pucCollation as string , phIndex out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCreateFTSIndex(hTable as IntPtr , pucFileName as string , pucTag as string , pucField as string , ulPageSize as DWORD , ulMinWordLen as DWORD , ulMaxWordLen as DWORD , usUseDefaultDelim as WORD , pucDelimiters as string , usUseDefaultNoise as WORD , pucNoiseWords as string , usUseDefaultDrop as WORD , pucDropChars as string , usUseDefaultConditionals as WORD , pucConditionalChars as string , pucReserved1 as string , pucReserved2 as string , ulOptions as DWORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCreateTable(hConnect as IntPtr, pucName as string , pucAlias as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , usMemoSize as WORD , pucFields as string , phTable out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCreateTable71(hConnect as IntPtr, pucName as string , pucDBObjName as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , usMemoSize as WORD , pucFields as string , ulOptions as DWORD, phTable out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCreateTable90(hConnect as IntPtr, pucName as string , pucDBObjName as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , usMemoSize as WORD , pucFields as string , ulOptions as DWORD, pucCollation as string , phTable out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDCreate(pucDictionaryPath as string , usEncrypt as WORD, pucDescription as string , phDictionary out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDCreateRefIntegrity(hDictionary AS IntPtr , pucRIName as string , pucFailTable as string , pucParentTableName as string , pucParentTagName as string , pucChildTableName as string , pucChildTagName as string , usUpdateRule as WORD , usDeleteRule as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDCreateRefIntegrity62(hDictionary AS IntPtr , pucRIName as string , pucFailTable as string , pucParentTableName as string , pucParentTagName as string , pucChildTableName as string , pucChildTagName as string , usUpdateRule as WORD , usDeleteRule as WORD , pucNoPrimaryError as string , pucCascadeError as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDRemoveRefIntegrity(hDictionary AS IntPtr , pucRIName as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetDatabaseProperty(hObject as IntPtr, usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetDatabaseProperty(hObject as IntPtr, usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetDatabaseProperty(hObject as IntPtr, usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetFieldProperty(hObject as IntPtr, pucTableName as string , pucFieldName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetFieldProperty(hObject as IntPtr, pucTableName as string , pucFieldName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetFieldProperty(hObject as IntPtr, pucTableName as string , pucFieldName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetIndexFileProperty(hObject as IntPtr, pucTableName as string , pucIndexFileName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetIndexFileProperty(hObject as IntPtr, pucTableName as string , pucIndexFileName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetIndexFileProperty(hObject as IntPtr, pucTableName as string , pucIndexFileName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetIndexProperty(hObject as IntPtr, pucTableName as string , pucIndexName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetIndexProperty(hObject as IntPtr, pucTableName as string , pucIndexName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetIndexProperty(hObject as IntPtr, pucTableName as string , pucIndexName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetLinkProperty(hConnect as IntPtr, pucLinkName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetLinkProperty(hConnect as IntPtr, pucLinkName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetLinkProperty(hConnect as IntPtr, pucLinkName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetTableProperty(hObject as IntPtr, pucTableName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetTableProperty(hObject as IntPtr, pucTableName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetTableProperty(hObject as IntPtr, pucTableName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetUserGroupProperty(hObject as IntPtr, pucUserGroupName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetUserGroupProperty(hObject as IntPtr, pucUserGroupName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetUserGroupProperty(hObject as IntPtr, pucUserGroupName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetUserProperty(hObject as IntPtr, pucUserName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetUserProperty(hObject as IntPtr, pucUserName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetUserProperty(hObject as IntPtr, pucUserName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetViewProperty(hObject as IntPtr, pucViewName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetViewProperty(hObject as IntPtr, pucViewName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetViewProperty(hObject as IntPtr, pucViewName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetTriggerProperty(hObject as IntPtr, pucTriggerName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetTriggerProperty(hObject as IntPtr, pucTriggerName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetTriggerProperty(hObject as IntPtr, pucTriggerName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetProcedureProperty(hObject as IntPtr, pucProcName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetProcedureProperty(hObject as IntPtr, pucProcName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetProcedureProperty(hObject as IntPtr, pucProcName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetRefIntegrityProperty(hObject as IntPtr, pucRIName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetPermissions(hDBConn as IntPtr , pucGrantee as string , usObjectType as WORD, pucObjectName as string , pucParentName as string , usGetInherited as WORD , pulPermissions out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGrantPermission(hAdminConn as IntPtr , usObjectType as WORD, pucObjectName as string , pucParentName as string , pucGrantee as string , ulPermissions as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDRevokePermission(hAdminConn as IntPtr , usObjectType as WORD, pucObjectName as string , pucParentName as string , pucGrantee as string , ulPermissions as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetDatabaseProperty(hDictionary AS IntPtr , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetDatabaseProperty(hDictionary AS IntPtr , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetDatabaseProperty(hDictionary AS IntPtr , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetFieldProperty(hDictionary AS IntPtr , pucTableName as string , pucFieldName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD , usValidateOption as WORD , pucFailTable as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetFieldProperty(hDictionary AS IntPtr , pucTableName as string , pucFieldName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD , usValidateOption as WORD , pucFailTable as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetFieldProperty(hDictionary AS IntPtr , pucTableName as string , pucFieldName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD , usValidateOption as WORD , pucFailTable as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetProcedureProperty(hDictionary AS IntPtr , pucProcedureName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetProcedureProperty(hDictionary AS IntPtr , pucProcedureName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetProcedureProperty(hDictionary AS IntPtr , pucProcedureName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetTableProperty(hDictionary AS IntPtr , pucTableName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD , usValidateOption as WORD , pucFailTable as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetTableProperty(hDictionary AS IntPtr , pucTableName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD , usValidateOption as WORD , pucFailTable as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetTableProperty(hDictionary AS IntPtr , pucTableName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD , usValidateOption as WORD , pucFailTable as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetUserGroupProperty(hDictionary AS IntPtr , pucUserGroupName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetUserGroupProperty(hDictionary AS IntPtr , pucUserGroupName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetUserGroupProperty(hDictionary AS IntPtr , pucUserGroupName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetUserProperty(hDictionary AS IntPtr , pucUserName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetUserProperty(hDictionary AS IntPtr , pucUserName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetUserProperty(hDictionary AS IntPtr , pucUserName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetViewProperty(hDictionary AS IntPtr , pucViewName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetViewProperty(hDictionary AS IntPtr , pucViewName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetViewProperty(hDictionary AS IntPtr , pucViewName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetObjectAccessRights(hDictionary AS IntPtr , pucObjectName as string , pucAccessorName as string , pucAllowedAccess as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDAddProcedure(hDictionary AS IntPtr , pucName as string , pucContainer as string , pucProcName as string , ulInvokeOption as DWORD , pucInParams as string , pucOutParams as string , pucComments as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDAddTable(hDictionary AS IntPtr , pucTableName as string , pucTablePath as string , usTableType as WORD, usCharType as WORD , pucIndexFiles as string , pucComments as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDAddTable90(hDictionary AS IntPtr , pucTableName as string , pucTablePath as string , usTableType as WORD, usCharType as WORD , pucIndexFiles as string , pucComments as string , pucCollation as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDAddView(hDictionary AS IntPtr , pucName as string , pucComments as string , pucSQL as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDCreateTrigger(hDictionary AS IntPtr , pucName as string , pucTableName as string ,  ulTriggerType as DWORD, ulEventTypes as DWORD , ulContainerType as DWORD , pucContainer as string , pucFunctionName as string , ulPriority as DWORD , pucComments as string , ulOptions as DWORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDRemoveTrigger(hDictionary AS IntPtr , pucName as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDAddIndexFile(hDictionary AS IntPtr , pucTableName as string , pucIndexFilePath as string , pucComment as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDCreateUser(hDictionary AS IntPtr , pucGroupName as string , pucUserName as string , pucPassword as string , pucDescription as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDAddUserToGroup(hDictionary AS IntPtr , pucGroupName as string , pucUserName as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDRemoveUserFromGroup(hDictionary AS IntPtr , pucGroupName as string , pucUserName as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDDeleteUser(hDictionary AS IntPtr , pucUserName as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDCreateUserGroup(hDictionary AS IntPtr , pucGroupName as string , pucDescription as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDDeleteUserGroup(hDictionary AS IntPtr , pucGroupName as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDDeleteIndex(hDictionary AS IntPtr , pucTableName as string , pucIndexName as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDRemoveIndexFile(hDictionary AS IntPtr , pucTableName as string , pucIndexFileName as string , usDeleteFile as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDRemoveProcedure(hDictionary AS IntPtr , pucName as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDRemoveTable(hObject as IntPtr, pucTableName as string , usDeleteFiles as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDRemoveView(hDictionary AS IntPtr , pucName as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDRenameObject(hDictionary AS IntPtr , pucObjectName as string , pucNewObjectName as string , usObjectType as WORD, ulOptions as DWORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDMoveObjectFile(hDictionary AS IntPtr , usObjectType as WORD, pucObjectName as string , pucNewPath as string , pucIndexFiles as string , pucParent as string , ulOptions as DWORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDFindFirstObject(hObject as IntPtr, usFindObjectType as WORD , pucParentName as string , [In] [Out] pucObjectName as char[] , pusObjectNameLen ref WORD , phFindHandle out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDFindNextObject(hObject as IntPtr, hFindHandle as IntPtr , [In] [Out] pucObjectName as char[] , pusObjectNameLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDFindClose(hObject as IntPtr, hFindHandle as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDCreateLink(hDBConn as IntPtr , pucLinkAlias as string , pucLinkedDDPath as string , pucUserName as string , pucPassword as string , ulOptions as DWORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDModifyLink(hDBConn as IntPtr , pucLinkAlias as string , pucLinkedDDPath as string , pucUserName as string , pucPassword as string , ulOptions as DWORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDDropLink(hDBConn as IntPtr , pucLinkedDD as string , usDropGlobal as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDCreatePublication(hDictionary AS IntPtr , pucPublicationName as string , pucComments as string , ulOptions as DWORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetPublicationProperty(hObject as IntPtr, pucPublicationName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetPublicationProperty(hObject as IntPtr, pucPublicationName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetPublicationProperty(hObject as IntPtr, pucPublicationName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetPublicationProperty(hDictionary AS IntPtr , pucPublicationName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetPublicationProperty(hDictionary AS IntPtr , pucPublicationName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetPublicationProperty(hDictionary AS IntPtr , pucPublicationName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDDeletePublication(hDictionary AS IntPtr , pucPublicationName as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDCreateArticle(hDictionary AS IntPtr , pucPublicationName as string , pucObjectName as string , pucRowIdentColumns as string , pucFilter as string , ulOptions as DWORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetArticleProperty(hObject as IntPtr, pucPublicationName as string , pucObjectName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetArticleProperty(hObject as IntPtr, pucPublicationName as string , pucObjectName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetArticleProperty(hObject as IntPtr, pucPublicationName as string , pucObjectName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetArticleProperty(hDictionary AS IntPtr , pucPublicationName as string , pucObjectName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetArticleProperty(hDictionary AS IntPtr , pucPublicationName as string , pucObjectName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetArticleProperty(hDictionary AS IntPtr , pucPublicationName as string , pucObjectName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDDeleteArticle(hDictionary AS IntPtr , pucPublicationName as string , pucObjectName as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDCreateSubscription(hDictionary AS IntPtr , pucSubscriptionName as string , pucPublicationName as string , pucTarget as string , pucUser as string , pucPassword as string , pucReplicationQueue as string , usForward as WORD , pucComments as string , ulOptions as DWORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetSubscriptionProperty(hObject as IntPtr, pucSubscriptionName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetSubscriptionProperty(hObject as IntPtr, pucSubscriptionName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDGetSubscriptionProperty(hObject as IntPtr, pucSubscriptionName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetSubscriptionProperty(hDictionary AS IntPtr , pucSubscriptionName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetSubscriptionProperty(hDictionary AS IntPtr , pucSubscriptionName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetSubscriptionProperty(hDictionary AS IntPtr , pucSubscriptionName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDDeleteSubscription(hDictionary AS IntPtr , pucSubscriptionName as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDecryptRecord(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDecryptTable(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDeleteCustomKey(hIndex as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDeleteIndex(hIndex as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDeleteRecord(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetKeyColumn(hCursor as IntPtr , [In] [Out] pucKeyColumn as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDisableEncryption(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDisableLocalConnections() as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDisconnect(hConnect as IntPtr) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsEnableEncryption(hTable as IntPtr , pucPassword as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsEncryptRecord(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsEncryptTable(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsEvalLogicalExpr(hTable as IntPtr , pucExpr as string , pbResult out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsEvalNumericExpr(hTable as IntPtr , pucExpr as string , pdResult out double ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsEvalStringExpr(hTable as IntPtr , pucExpr as string , [In] [Out] pucResult as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsEvalTestExpr(hTable as IntPtr , pucExpr as string , pusType out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsExtractKey(hIndex as IntPtr , [In] [Out] pucKey as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsFailedTransactionRecovery(pucServer as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsFileToBinary(hTable as IntPtr , pucFldName as string , usBinaryType as WORD , pucFileName as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsFileToBinary(hTable as IntPtr , lFieldOrdinal as DWORD, usBinaryType as WORD , pucFileName as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsFindConnection(pucServerName as string , phConnect out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsFindConnection25(pucFullPath as string , phConnect out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsFindClose(hConnect as IntPtr, lHandle as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsFindFirstTable(hConnect as IntPtr, pucFileMask as string , [In] [Out] pucFirstFile as char[] , pusFileLen ref WORD , plHandle out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsFindNextTable(hConnect as IntPtr, lHandle as IntPtr , [In] [Out] pucFileName as char[] , pusFileLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsFindFirstTable62(hConnect as IntPtr, pucFileMask as string , [In] [Out] pucFirstDD as char[] , pusDDLen ref WORD , [In] [Out] pucFirstFile as char[] , pusFileLen ref WORD , plHandle out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsFindNextTable62(hConnect as IntPtr, lHandle as IntPtr , [In] [Out] pucDDName as char[] , pusDDLen ref WORD , [In] [Out] pucFileName as char[] , pusFileLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetAllIndexes(hTable as IntPtr , [In] [Out] ahIndex as IntPtr[] , pusArrayLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetFTSIndexes(hTable as IntPtr , [In] [Out] ahIndex as IntPtr[] , pusArrayLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetAllLocks(hTable as IntPtr , [In] [Out] aulLocks as DWORD[] , pusArrayLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetAllTables([In] [Out] ahTable as IntPtr[] , pusArrayLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetBinary(hTable as IntPtr , pucFldName as string , ulOffset as DWORD , [In] [Out] pucBuf as byte[] , pulLen ref DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetBinary(hTable as IntPtr , lFieldOrdinal as DWORD, ulOffset as DWORD , [In] [Out] pucBuf as byte[] , pulLen ref DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetBinaryLength(hTable as IntPtr , pucFldName as string , pulLength out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetBinaryLength(hTable as IntPtr , lFieldOrdinal as DWORD, pulLength out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetBookmark(hTable as IntPtr , phBookmark out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetBookmark60(hObj as IntPtr , [In] [Out] pucBookmark as char[] , pulLength ref DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetBookmarkLength(hObj as IntPtr , pulLength ref DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCompareBookmarks(pucBookmark1 as string , pucBookmark2 as string , plResult out int ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetCollationLang([In] [Out] pucLang as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetCollation(hConnect as IntPtr, [In] [Out] pucCollation as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetConnectionType(hConnect as IntPtr, pusConnectType out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetConnectionPath(hConnect as IntPtr, [In] [Out] pucConnectionPath as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetConnectionProperty(hConnect as IntPtr, usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pulPropertyLen ref DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetDate(hTable as IntPtr , pucFldName as string , [In] [Out] pucBuf as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetDate(hTable as IntPtr , lFieldOrdinal as DWORD, [In] [Out] pucBuf as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetDateFormat([In] [Out] pucFormat as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetDateFormat60(hConnect as IntPtr, [In] [Out] pucFormat as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetDecimals(pusDecimals out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetDefault([In] [Out] pucDefault as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetDeleted(pbUseDeleted out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetDouble(hTable as IntPtr , pucFldName as string , pdValue out double ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetDouble(hTable as IntPtr , lFieldOrdinal as DWORD, pdValue out double ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetEpoch(pusCentury out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetErrorString(ulErrCode as DWORD , [In] [Out] pucBuf as char[] , pusBufLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetExact(pbExact out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetExact22(hObj as IntPtr , pbExact out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetField(hTable as IntPtr , pucFldName as string , [In] [Out] pucBuf as char[] , pulLen ref DWORD , usOption as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetField(hTable as IntPtr , lFieldOrdinal as DWORD, [In] [Out] pucBuf as char[] , pulLen ref DWORD , usOption as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetField(hTable as IntPtr , pucFldName as string , [In] [Out] abBuf as byte[] , pulLen ref DWORD , usOption as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetField(hTable as IntPtr , lFieldOrdinal as DWORD, [In] [Out] abBuf as byte[] , pulLen ref DWORD , usOption as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetFieldDecimals(hTable as IntPtr , pucFldName as string , pusDecimals out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetFieldDecimals(hTable as IntPtr , lFieldOrdinal as DWORD, pusDecimals out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetFieldLength(hTable as IntPtr , pucFldName as string , pulLength out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetFieldLength(hTable as IntPtr , lFieldOrdinal as DWORD, pulLength out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetFieldName(hTable as IntPtr , usFld as WORD , [In] [Out] pucName as char[] , pusBufLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetFieldNum(hTable as IntPtr , pucFldName as string , pusNum out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetFieldNum(hTable as IntPtr , lFieldOrdinal as DWORD, pusNum out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetFieldOffset(hTable as IntPtr , pucFldName as string , pulOffset out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetFieldOffset(hTable as IntPtr , lFieldOrdinal as DWORD, pulOffset out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetFieldType(hTable as IntPtr , pucFldName as string , pusType out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetFieldType(hTable as IntPtr , lFieldOrdinal as DWORD, pusType out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetFilter(hTable as IntPtr , [In] [Out] pucFilter as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetHandleINT64(hObj as IntPtr , pulVal out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetHandleType(hObj as IntPtr , pusType out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetIndexCondition(hIndex as IntPtr , [In] [Out] pucExpr as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetIndexExpr(hIndex as IntPtr , [In] [Out] pucExpr as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetIndexFilename(hIndex as IntPtr , usOption as WORD , [In] [Out] pucName as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetIndexHandle(hTable as IntPtr , pucIndexOrder as string , phIndex out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetIndexHandleByOrder(hTable as IntPtr , usOrderNum as WORD , phIndex out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetIndexHandleByExpr(hTable as IntPtr , pucExpr as string , ulDescending as DWORD , phIndex out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetIndexName(hIndex as IntPtr , [In] [Out] pucName as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetIndexOrderByHandle(hIndex as IntPtr , pusIndexOrder out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetJulian(hTable as IntPtr , pucFldName as string , plDate out int ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetJulian(hTable as IntPtr , lFieldOrdinal as DWORD, plDate out int ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetKeyCount(hIndex as IntPtr , usFilterOption as WORD, pulCount out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetKeyNum(hIndex as IntPtr , usFilterOption as WORD, pulKey out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetKeyLength(hIndex as IntPtr , pusKeyLength out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetKeyType(hIndex as IntPtr , usKeyType out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetLastError(pulErrCode out DWORD , [In] [Out] pucBuf as char[] , pusBufLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetLastTableUpdate(hTable as IntPtr , [In] [Out] pucDate as char[] , pusDateLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetLogical(hTable as IntPtr , pucFldName as string , pbValue out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetLogical(hTable as IntPtr , lFieldOrdinal as DWORD, pbValue out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetINT64(hTable as IntPtr , pucFldName as string , plValue out int ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetINT64(hTable as IntPtr , lFieldOrdinal as DWORD, plValue out int ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetINT64INT64(hTable as IntPtr , pucFldName as string , pqValue out INT64 ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetINT64INT64(hTable as IntPtr , lFieldOrdinal as DWORD, pqValue out INT64 ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetMemoBlockSize(hTable as IntPtr , pusBlockSize out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetMemoLength(hTable as IntPtr , pucFldName as string , pulLength out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetMemoLength(hTable as IntPtr , lFieldOrdinal as DWORD, pulLength out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetMemoDataType(hTable as IntPtr , pucFldName as string , pusType out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetMemoDataType(hTable as IntPtr , lFieldOrdinal as DWORD, pusType out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetMilliseconds(hTable as IntPtr , pucFldName as string , plTime out int ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetMilliseconds(hTable as IntPtr , lFieldOrdinal as DWORD, plTime out int ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetMoney(hTbl as IntPtr , pucFldName as string , pqValue out INT64 ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetMoney(hTbl as IntPtr , lFieldOrdinal as DWORD, pqValue out INT64 ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetActiveLinkInfo(hDBConn as IntPtr , usLinkNum as WORD , [In] [Out] pucLinkInfo as char[] , pusBufferLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetNumActiveLinks(hDBConn as IntPtr , pusNumLinks out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetNumFields(hTable as IntPtr , pusCount out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetNumIndexes(hTable as IntPtr , pusNum out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetNumFTSIndexes(hTable as IntPtr , pusNum out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetNumLocks(hTable as IntPtr , pusNum out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetNumOpenTables(pusNum out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetRecord(hTable as IntPtr , [In] [Out] pucRec as byte[] , pulLen ref DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetRecordCount(hTable as IntPtr , usFilterOption as WORD, pulCount out DWORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetRecordNum(hTable as IntPtr , usFilterOption as WORD, pulRec out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetRecordLength(hTable as IntPtr , pulLength out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetRecordCRC(hTable as IntPtr , pulCRC out DWORD , ulOptions as DWORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetRelKeyPos(hIndex as IntPtr , pdPos out double ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetScope(hIndex as IntPtr , usScopeOption as WORD, [In] [Out] pucScope as char[] , pusBufLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetSearchPath([In] [Out] pucPath as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetServerName(hConnect as IntPtr, pucName as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetServerTime(hConnect as IntPtr,  pucDateBuf as char[], pusDateBufLen ref WORD , plTime out int , [In] [Out] pucTimeBuf as char[] , pusTimeBufLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetShort(hTable as IntPtr , pucFldName as string , psValue out short ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetShort(hTable as IntPtr , lFieldOrdinal as DWORD, psValue out short ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetString(hTable as IntPtr , pucFldName as string , [In] [Out] pucBuf as char[] , pulLen ref DWORD , usOption as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetString(hTable as IntPtr , lFieldOrdinal as DWORD, [In] [Out] pucBuf as char[] , pulLen ref DWORD , usOption as WORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetTableAlias(hTable as IntPtr , [In] [Out] pucAlias as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetTableCharType(hTable as IntPtr , pusCharType out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetTableConnection(hTable as IntPtr , phConnect out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetTableFilename(hTable as IntPtr , usOption as WORD , [In] [Out] pucName as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetTableHandle(pucName as string , phTable out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetTableHandle25(hConnect as IntPtr, pucName as string , phTable out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetTableLockType(hTable as IntPtr , pusLockType out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetTableMemoSize(hTable as IntPtr , pusMemoSize out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetTableOpenOptions(hTable as IntPtr , pulOptions out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetTableRights(hTable as IntPtr , pusRights out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetTableType(hTable as IntPtr , pusType out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetTime(hTable as IntPtr , pucFldName as string , [In] [Out] pucBuf as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetTime(hTable as IntPtr , lFieldOrdinal as DWORD, [In] [Out] pucBuf as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetVersion(pulMajor out DWORD , pulMinor out DWORD , pucLetter as string , [In] [Out] pucDesc as char[] , pusDescLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGotoBookmark(hTable as IntPtr , hBookmark as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGotoBookmark60(hObj as IntPtr , pucBookmark as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGotoBottom(hObj as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGotoRecord(hTable as IntPtr , ulRec as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGotoTop(hObj as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsImageToClipboard(hTable as IntPtr , pucFldName as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsInTransaction(hConnect as IntPtr, pbInTrans out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsEmpty(hTable as IntPtr , pucFldName as string , pbEmpty out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsEmpty(hTable as IntPtr , lFieldOrdinal as DWORD, pbEmpty out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsExprValid(hTable as IntPtr , pucExpr as string , pbValid out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsFound(hObj as IntPtr , pbFound out WORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsIndexCompound(hIndex as IntPtr , pbCompound out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsIndexCandidate(hIndex as IntPtr , pbCandidate out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsIndexNullable(hIndex as IntPtr , pbNullable out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsIndexCustom(hIndex as IntPtr , pbCustom out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsIndexDescending(hIndex as IntPtr , pbDescending out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsIndexPrimaryKey(hIndex as IntPtr , pbPrimaryKey out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsIndexFTS(hIndex as IntPtr , pbFTS out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsIndexUnique(hIndex as IntPtr , pbUnique out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsRecordDeleted(hTable as IntPtr , pbDeleted out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsRecordEncrypted(hTable as IntPtr , pbEncrypted out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsRecordLocked(hTable as IntPtr , ulRec as DWORD , pbLocked out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsRecordVisible(hObj as IntPtr , pbVisible out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsServerLoaded(pucServer as string , pbLoaded out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsTableEncrypted(hTable as IntPtr , pbEncrypted out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsTableLocked(hTable as IntPtr , pbLocked out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsLocate(hTable as IntPtr , pucExpr as string , bForward as WORD , pbFound out WORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsLockRecord(hTable as IntPtr , ulRec as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsLockTable(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsLookupKey(hIndex as IntPtr , pucKey as string , usKeyLen AS WORD, usDataType as WORD, pbFound out WORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsMgConnect(pucServerName as string , pucUserName as string , pucPassword as string , phMgmtHandle out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsMgDisconnect(hMgmtHandle as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsMgResetCommStats(hMgmtHandle as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsMgDumpInternalTables(hMgmtHandle as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsMgGetServerType(hMgmtHandle as IntPtr , pusServerType out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsMgKillUser(hMgmtHandle as IntPtr , pucUserName as string , usConnNumber as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsNullTerminateStrings(bNullTerminate as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsOpenIndex(hTable as IntPtr , pucName as string , [In] [Out] ahIndex as IntPtr[] , pusArrayLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsOpenTable(hConnect as IntPtr, pucName as string , pucAlias as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , ulOptions as DWORD, phTable out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsOpenTable90(hConnect as IntPtr, pucName as string , pucAlias as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , ulOptions as DWORD, pucCollation as string , phTable out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsPackTable(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsRecallRecord(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsRecallAllRecords(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsRefreshRecord(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsClearProgressCallback() as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsRegisterCallbackFunction(pfn as CallbackFn , ulCallBackID as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsClearCallbackFunction() as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsReindex(hObject as IntPtr) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsReindex61(hObject as IntPtr, ulPageSize as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsReindexFTS(hObject as IntPtr, ulPageSize as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsResetConnection(hConnect as IntPtr) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsRollbackTransaction(hConnect as IntPtr) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSeek(hIndex as IntPtr , pucKey as string , usKeyLen AS WORD, usDataType as WORD, usSeekType as WORD, pbFound out WORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSeek(hIndex as IntPtr , abKey as byte[] , usKeyLen AS WORD, usDataType as WORD, usSeekType as WORD, pbFound out WORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSeekLast(hIndex as IntPtr , pucKey as string , usKeyLen AS WORD, usDataType as WORD, pbFound out WORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSeekLast(hIndex as IntPtr , abKey as byte[] , usKeyLen AS WORD, usDataType as WORD, pbFound out WORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetBinary(hTable as IntPtr , pucFldName as string , usBinaryType as WORD , ulTotalLength as DWORD , ulOffset as DWORD , pucBuf as byte[] , ulLen as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetBinary(hTable as IntPtr , lFieldOrdinal as DWORD, usBinaryType as WORD , ulTotalLength as DWORD , ulOffset as DWORD , pucBuf as byte[] , ulLen as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetCollationLang(pucLang as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetCollation(hConnect as IntPtr, pucCollation as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetDate(hObj as IntPtr , pucFldName as string , pucValue as string , usLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetDate(hObj as IntPtr , lFieldOrdinal as DWORD, pucValue as string , usLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetDateFormat(pucFormat as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetDateFormat60(hConnect as IntPtr, pucFormat as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetDecimals(usDecimals as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetDefault(pucDefault as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsShowDeleted(bShowDeleted as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetDouble(hObj as IntPtr , pucFldName as string , dValue as Real8) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetDouble(hObj as IntPtr , lFieldOrdinal as DWORD, dValue as real8) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetEmpty(hObj as IntPtr , pucFldName as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetEmpty(hObj as IntPtr , lFieldOrdinal as DWORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetEpoch(usCentury as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetExact(bExact as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetExact22(hObj as IntPtr , bExact as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetField(hObj as IntPtr , pucFldName as string , pucBuf as string , ulLen as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetField(hObj as IntPtr , lFieldOrdinal as DWORD, pucBuf as string , ulLen as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetField(hObj as IntPtr , pucFldName as string , abBuf as byte[] , ulLen as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetField(hObj as IntPtr , lFieldOrdinal as DWORD, abBuf as byte[] , ulLen as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetFilter(hTable as IntPtr , pucFilter as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetHandleINT64(hObj as IntPtr , ulVal as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetJulian(hObj as IntPtr , pucFldName as string , lDate as int ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetJulian(hObj as IntPtr , lFieldOrdinal as DWORD, lDate as int) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetLogical(hObj as IntPtr , pucFldName as string , bValue as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetLogical(hObj as IntPtr , lFieldOrdinal as DWORD, bValue as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetINT64(hObj as IntPtr , pucFldName as string , lValue as int ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetINT64(hObj as IntPtr , lFieldOrdinal as DWORD, lValue as int ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetINT64INT64(hObj as IntPtr , pucFldName as string , qValue as INT64 ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetINT64INT64(hObj as IntPtr , lFieldOrdinal as DWORD, qValue as INT64 ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetMilliseconds(hObj as IntPtr , pucFldName as string , lTime as int ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetMilliseconds(hObj as IntPtr , lFieldOrdinal as DWORD, lTime as int ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetMoney(hObj as IntPtr , pucFldName as string , qValue as INT64 ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetMoney(hObj as IntPtr , lFieldOrdinal as DWORD, qValue as INT64 ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetRecord(hObj as IntPtr , pucRec as byte[] , ulLen as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetRelation(hTableParent as IntPtr , hIndexChild as IntPtr , pucExpr as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetRelKeyPos(hIndex as IntPtr , dPos as Real8) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetScope(hIndex as IntPtr , usScopeOption as WORD, pucScope as string , usScopeLen as WORD , usDataType as WORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetScope(hIndex as IntPtr , usScopeOption as WORD, abScope as byte[] , usScopeLen as WORD , usDataType as WORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetScopedRelation(hTableParent as IntPtr , hIndexChild as IntPtr , pucExpr as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetSearchPath(pucPath as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetServerType(usServerOptions as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetShort(hObj as IntPtr , pucFldName as string , sValue as short ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetShort(hObj as IntPtr , lFieldOrdinal as DWORD, sValue as short ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetString(hObj as IntPtr , pucFldName as string , pucBuf as string , ulLen as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetString(hObj as IntPtr , lFieldOrdinal as DWORD, pucBuf as string , ulLen as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetTime(hObj as IntPtr , pucFldName as string , pucValue as string , usLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetTime(hObj as IntPtr , lFieldOrdinal as DWORD, pucValue as string , usLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsShowError(pucTitle as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSkip(hObj as IntPtr , lRecs as int) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSkipUnique(hIndex as IntPtr , lRecs as int) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsThreadExit() as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsUnlockRecord(hTable as IntPtr , ulRec as DWORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsUnlockTable(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsVerifyPassword(hTable as IntPtr , pusEnabled out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsEncryptionEnabled(hTable as IntPtr , pusEnabled out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsWriteAllRecords() as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsWriteRecord(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsZapTable(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetAOF(hTable as IntPtr , pucFilter as string , usOptions as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsEvalAOF(hTable as IntPtr , pucFilter as string , pusOptLevel out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsClearAOF(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsRefreshAOF(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetAOF(hTable as IntPtr , [In] [Out] pucFilter as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetAOFOptLevel(hTable as IntPtr , pusOptLevel out WORD , [In] [Out] pucNonOpt as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsRecordInAOF(hTable as IntPtr , ulRecordNum as DWORD , pusIsInAOF out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCustomizeAOF(hTable as IntPtr , ulNumRecords as DWORD , pulRecords out DWORD , usOption as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsInitRawKey(hIndex as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsBuildRawKey(hIndex as IntPtr , [In] [Out] pucKey as byte[] , pusKeyLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCreateSQLStatement(hConnect as IntPtr, phStatement out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsPrepareSQL(hStatement as IntPtr , pucSQL as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsExecuteSQL(hStatement as IntPtr , phCursor out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsExecuteSQLDirect(hStatement as IntPtr , pucSQL as string , phCursor out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCloseSQLStatement(hStatement as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsStmtSetTableRights(hStatement as IntPtr , usCheckRights as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsStmtSetTableReadOnly(hStatement as IntPtr , usReadOnly as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsStmtSetTableLockType(hStatement as IntPtr , usLockType as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsStmtSetTableCharType(hStatement as IntPtr , usCharType as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsStmtSetTableType(hStatement as IntPtr , usTableType as WORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsStmtSetTableCollation(hStatement as IntPtr , pucCollation as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsStmtConstrainUpdates(hStatement as IntPtr , usConstrain as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsStmtEnableEncryption(hStatement as IntPtr , pucPassword as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsStmtDisableEncryption(hStatement as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsStmtSetTablePassword(hStatement as IntPtr , pucTableName as string , pucPassword as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsStmtClearTablePasswords(hStatement as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsStmtReadAllColumns(hStatement as IntPtr , usReadColumns as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsClearSQLParams(hStatement as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetTimeStamp(hObj as IntPtr , pucFldName as string , pucBuf as string , ulLen as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetTimeStamp(hObj as IntPtr , lFieldOrdinal as DWORD, pucBuf as string , ulLen as DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsClearSQLAbortFunc() as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetNumParams(hStatement as IntPtr , pusNumParams out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetLastAutoinc(hObj as IntPtr , pulAutoIncVal out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsIndexUserDefined(hIndex as IntPtr , pbUserDefined out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsRestructureTable(hObj as IntPtr , pucName as string , pucPassword as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , pucAddFields as string , pucDeleteFields as string , pucChangeFields as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsRestructureTable90(hObj as IntPtr , pucName as string , pucPassword as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , pucAddFields as string , pucDeleteFields as string , pucChangeFields as string , pucCollation as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetSQLStatementHandle(hCursor as IntPtr , phStmt out IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetSQLStatement(hStmt as IntPtr , [In] [Out] pucSQL as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsFlushFileBuffers(hTable as IntPtr ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDDeployDatabase(pucDestination as string , pucDestinationPassword as string , pucSource as string , pucSourcePassword as string , usServerTypes as WORD, usValidateOption as WORD , usBackupFiles as WORD , ulOptions as DWORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsVerifySQL(hStatement as IntPtr , pucSQL as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDisableUniqueEnforcement(hConnect as IntPtr) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsEnableUniqueEnforcement(hConnect as IntPtr) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDisableRI(hConnect as IntPtr) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsEnableRI(hConnect as IntPtr) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDisableAutoIncEnforcement(hConnection as IntPtr) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsEnableAutoIncEnforcement(hConnection as IntPtr) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsRollbackTransaction80(hConnect as IntPtr, pucSavepoint as string , ulOptions as DWORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsCreateSavepoint(hConnect as IntPtr, pucSavepoint as string , ulOptions as DWORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDFreeTable(pucTableName as string , pucPassword as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetIndexProperty(hAdminConn as IntPtr , pucTableName as string , pucIndexName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetIndexProperty(hAdminConn as IntPtr , pucTableName as string , pucIndexName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsDDSetIndexProperty(hAdminConn as IntPtr , pucTableName as string , pucIndexName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsFieldBinary(hTable as IntPtr , pucFldName as string , pbBinary out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsFieldBinary(hTable as IntPtr , lFieldOrdinal as DWORD, pbBinary out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsNull(hTable as IntPtr , pucFldName as string , pbNull out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsNull(hTable as IntPtr , lFieldOrdinal as DWORD, pbNull out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsNullable(hTable as IntPtr , pucFldName as string , pbNullable out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsIsNullable(hTable as IntPtr , lFieldOrdinal as DWORD, pbNullable out WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetNull(hTable as IntPtr , pucFldName as string ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetNull(hTable as IntPtr , lFieldOrdinal as DWORD) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetTableCollation(hTbl as IntPtr , [In] [Out] pucCollation as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetIndexCollation(hIndex as IntPtr , [In] [Out] pucCollation as char[] , pusLen ref WORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetDataLength(hTable as IntPtr , pucFldName as string , ulOptions as DWORD, pulLength out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsGetDataLength(hTable as IntPtr , lFieldOrdinal as DWORD, ulOptions as DWORD, pulLength out DWORD ) as DWORD 

[DllImport("ace64.dll", CharSet := CharSet.Ansi)];
internal static extern METHOD AdsSetIndexDirection(hIndex as IntPtr ,  usReverseDirection as WORD) as DWORD 

END CLASS
END NAMESPACE