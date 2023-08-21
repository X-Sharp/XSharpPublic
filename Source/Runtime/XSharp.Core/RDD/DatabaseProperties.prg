USING System.Collections.Generic
USING XSharp.RDD
/// <summary>Enum that matches the various FoxPro database properties, used in DbGetProp() and DbSetProp()</summary>
/// <seealso cref="O:XSharp.VFP.Functions.DbGetProp" />
/// <seealso cref="O:XSharp.VFP.Functions.DbSetProp" />
ENUM XSharp.RDD.DatabasePropertyType
    /// <summary>Null (Internal) </summary>
    MEMBER Null             := 0
    /// <summary>The relative path with respect to the DBC to the table including the name of the file. </summary>
    MEMBER Path             := 1
    /// <summary>Class (Internal) </summary>
    MEMBER Class            := 2
    /// <summary>Not defined</summary>
    MEMBER Unknown_3         := 3
    /// <summary>Not defined</summary>
    MEMBER Unknown_4         := 4
    /// <summary>Not defined</summary>
    MEMBER Unknown_5         := 5
    /// <summary>Not defined</summary>
    MEMBER Unknown_6         := 6
    /// <summary>The text of the field comment. (C) </summary>
    MEMBER Comment          := 7
    /// <summary>Not defined</summary>
    MEMBER Unknown_8         := 8
    /// <summary>The rule expression. (C)</summary>
    MEMBER RuleExpression   := 9 
    /// <summary>The row rule error text.(C) </summary>
    MEMBER RuleText         := 10
    /// <summary>The field default value.(C) </summary>
    MEMBER DefaultValue     := 11
    /// <summary>The WHERE clause parameters. The format for the parameters is ''ParameterName1, 'Type1'; ParameterName2, 'Type2'; ...'' </summary>
    MEMBER ParameterList    := 12
    /// <summary>ChildTag for a Relation Object</summary>
    MEMBER RelatedChild     := 13
    /// <summary>The Insert trigger expression.</summary>
    MEMBER InsertTrigger    := 14
    /// <summary>The Update trigger expression.</summary>
    MEMBER UpdateTrigger    := 15
    /// <summary>The Delete trigger expression.</summary>
    MEMBER DeleteTrigger    := 16
    /// <summary>The IsUnique flag for an Index Object</summary>
    MEMBER IsUnique         := 17
    /// <summary>Related table for a Relation Object</summary>
    MEMBER RelatedTable     := 18
    /// <summary>Related tag for a Relatin Object</summary>
    MEMBER RelatedTag       := 19
    /// <summary>The tag name of the primary key. (C) </summary>
    MEMBER PrimaryKey       := 20
    /// <summary>Not defined</summary>
    MEMBER Unknown_21        := 21
    /// <summary>Not defined</summary>
    MEMBER Unknown_22        := 22
    /// <summary>Not defined</summary>
    MEMBER Unknown_23        := 23
    /// <summary>The database version number. (N) </summary>
    MEMBER Version          := 24
    /// <summary>Not defined</summary>
    MEMBER Unknown_25        := 25
    /// <summary>Not defined</summary>
    MEMBER Unknown_26        := 26
    /// <summary>Not defined</summary>
    MEMBER Unknown_27        := 27
    /// <summary>The number of update statements sent to the back end for views. 1 is the default. Adjusting this value can greatly increase update performance. (N) </summary>
    MEMBER BatchUpdateCount := 28
    /// <summary>The name of the data source as defined in the Odbc.ini file.</summary>
    MEMBER DataSource       := 29
    /// <summary>Not defined</summary>
    MEMBER Unknown_30        := 30
    /// <summary>Not defined</summary>
    MEMBER Unknown_31        := 31
    /// <summary>The named connection used when the view is opened. (C)</summary>
    MEMBER ConnectName      := 32
    /// <summary>Not defined</summary>
    MEMBER Unknown_33        := 33
    /// <summary>Not defined</summary>
    MEMBER Unknown_34        := 34
    /// <summary>The name of the field used when data in the field is updated to the remote table. By default, the remote table field name.Read/write. </summary>
    MEMBER UpdateName       := 35
    /// <summary>Contains true (.T.) (default) if memo and general fields are fetched with the view results; otherwise, contains false (.F.).</summary>
    MEMBER FetchMemo        := 36
    /// <summary>Contains the number of records fetched at a time from the remote tables. The default is 100 records. Setting FetchSize to –1 retrieves the complete result set, limited by the MaxRecords setting</summary>
    MEMBER FetchSize        := 37
    /// <summary>Contains True (.T.) if the field is specified in an index key expression; otherwise, contains False (.F.). (L)  </summary>
    MEMBER KeyField         := 38
    /// <summary>The maximum number of records fetched when result sets are returned. (N) The default is – 1 (all rows are returned). A value of 0 specifies that the view is executed but no results are fetched.</summary>
    MEMBER MaxRecords       := 39
    /// <summary>Contains true (.T.) if the view can share its connection handle with other connections; otherwise, contains false (.F.).</summary>
    MEMBER ShareConnection  := 40
    /// <summary>The view source. (N) SourceType may assume the following values: 1 - The view uses local tables.2 - The view uses remote tables.</summary>
    MEMBER SourceType       := 41
    /// <summary>The SQL statement executed when the view is opened.(C) </summary>
    MEMBER SQL              := 42
    /// <summary>A comma-delimited list of the names of the tables.(C) </summary>
    MEMBER Tables           := 43
    /// <summary>Contains true (.T.) if a SQL update query is sent to update remote tables; otherwise, contains false (.F.) (default).</summary>
    MEMBER SendUpdates      := 44
    /// <summary>Contains True (.T.) if the field can be updated; otherwise, contains False (.F.). </summary>
    MEMBER Updatable        := 45
    /// <summary>The update type. Valid values are: 1 or DB_UPDATE (from Foxpro.h). The old data is updated with the new data (default). 2 or DB_DELETEINSERT (from Foxpro.h). The old data is deleted and the new data is inserted.</summary>
    MEMBER UpdateType       := 46
    /// <summary>The minimum size (in bytes) for which result columns are returned in memo fields. For example, if the width of a column result is greater than the value of UseMemoSize, the column result is stored in a memo field. UseMemoSize may vary from 1 to 255; the default value is 255.</summary>
    MEMBER UseMemoSize      := 47
    /// <summary>The WHERE clause for updates to remote tables.WhereType may assume the following values: 1 or DB_KEY , 2 or DB_KEYANDUPDATABLE, 3 or DB_KEYANDMODIFIED and 4 or DB_KEYANDTIMESTAMP </summary>
    MEMBER WhereType        := 48
    /// <summary>Not defined</summary>
    MEMBER Unknown_49        := 49
    /// <summary>Name of the class used for field mapping. (C) </summary>
    MEMBER DisplayClass     := 50
    /// <summary>Path to the class library specified with the DisplayClass property.(C)  </summary>
    MEMBER DisplayClassLibrary := 51
    /// <summary>Not defined</summary>
    MEMBER Unknown_52        := 52
    /// <summary>Not defined</summary>
    MEMBER Unknown_53        := 53
    /// <summary>The field input format. (C) .Read/write. </summary>
    MEMBER InputMask        := 54
    /// <summary>The field display format. (C) .Read/write. </summary>
    MEMBER Format           := 55
    /// <summary>The field caption. (C) If an equal sign (=) precedes the value of this property, Visual FoxPro evaluates the value as an expression. Otherwise, it is treated as a string literal.Read/write. FIELD</summary>
    MEMBER Caption          := 56
    /// <summary>Not defined</summary>
    MEMBER Unknown_57        := 57
    /// <summary>Not defined</summary>
    MEMBER Unknown_58        := 58
    /// <summary>Not defined</summary>
    MEMBER Unknown_59        := 59
    /// <summary>Not defined</summary>
    MEMBER Unknown_60        := 60
    /// <summary>Not defined</summary>
    MEMBER Unknown_61        := 61
    /// <summary>Not defined</summary>
    MEMBER Unknown_62        := 62
    /// <summary>Not defined</summary>
    MEMBER Unknown_63        := 63
    /// <summary>The connection mode. (Default) False (.F.) specifies a synchronous connection. True (.T.) specifies an asynchronous connection.Read/write.</summary>
    MEMBER Asynchronous     := 64
    /// <summary>The batch-processing mode. (Default) True (.T.) specifies the connection that operates in batch mode.</summary>
    MEMBER BatchMode        := 65
    /// <summary>The login connection string.</summary>
    MEMBER ConnectString    := 66
    /// <summary>The connection timeout interval in seconds. The default is 0 (wait indefinitely).</summary>
    MEMBER ConnectTimeout   := 67
    /// <summary>Contains a numeric value that determines when the ODBC Login dialog box is displayed.</summary>
    MEMBER DispLogin        := 68
    /// <summary>Contains a numeric value that determines when the ODBC Login dialog box is displayed.</summary>
    MEMBER DispWarnings     := 69
    /// <summary>The idle timeout interval in minutes. Timeout is checked no more than once per minute, therefore it is possible that active connections might deactivate one or two minutes later after the specified time interval has elapsed. The default value is 0 (wait indefinitely).</summary>
    MEMBER IdleTimeout      := 70
    /// <summary>The query timeout interval in seconds. The default value is 0 (wait indefinitely).</summary>
    MEMBER QueryTimeOut     := 71
    /// <summary>The connection password.</summary>
    MEMBER Password         := 72
    /// <summary>Contains a numeric value that determines how the connection manages transactions on the remote table. </summary>
    MEMBER Transactions     := 73
    /// <summary>The user identification.</summary>
    MEMBER UserId           := 74
    /// <summary>The amount of time in milliseconds that elapses before Visual FoxPro checks whether the SQL statement has completed executing. </summary>
    MEMBER WaitTime         := 75
    /// <summary>Timestamp (internal)</summary>
    MEMBER TimeStamp        := 76
    /// <summary>The data type for a field in a view. Initially set to the data type for the field in the data source.</summary>
    MEMBER DataType         := 77
    /// <summary>The size of the network packet used by the connection. Adjusting this value can improve performance. The default value is 4096 bytes (4K).</summary>
    MEMBER PacketSize       := 78
    /// <summary>The name of the server database specified with the DATABASE clause in the CREATE CONNECTION command or in the Connection Designer.</summary>
    MEMBER Database         := 79
    /// <summary>Contains True (.T.) if SQL statements are prepared for subsequent REQUERY( ) function calls. REQUERY( ) is used to retrieve data again for a SQL view. See SQLPREPARE( ) for additional information about preparing SQL statements. The default is false (.F.).</summary>
    MEMBER Prepared         := 80
    /// <summary>Contains true (.T.) (default) if memo fields (of type Memo, General, or Picture, or, for remote views, type Timestamp) are included in the WHERE clause for updates; otherwise, contains false (.F.).Read/write.</summary>
    MEMBER CompareMemo      := 81
    /// <summary>If True (.T.), data is fetched only when needed, such as when record pointer moves to a row that has not been fetched. If False (.F.), additional data is fetched during idle time. (Default)</summary>
    MEMBER FetchAsNeeded    := 82
    /// <summary>Not defined</summary>
    MEMBER Unknown_83        := 83
    /// <summary>When this property is found the table is offline. The Path property then returns the OffLinePath</summary>
    MEMBER OfflineRecs       := 84
    /// <summary># of remote records offline.</summary>
    MEMBER OfflineRemRecs   := 85
    /// <summary>Relative path and filename of external program file containing DBC Events code. (C) </summary>
    MEMBER DBCEventFileName := 86
    /// <summary>Enabled state of DBC Events. Set to True (.T.) to enable. (L) </summary>
    MEMBER DBCEvents        := 87
    /// <summary>Applies when using remote views, a shared connection, and to cursors created using ODBC. </summary>
    MEMBER AllowSimultaneousFetch := 88
    /// <summary>Specifies if a pending transaction is committed or rolled back when SQLDISCONNECT( ) is called for the last connection handle.</summary>
    MEMBER DisconnectRollback := 89
    /// <summary>Is the table offline (this is not stored as a real property but derived from the extistence of the properties OfflineRecs, OfflineRemRecs and Path.</summary>
    MEMBER OffLine          := 99
    /// <summary>X# Column Name property (not persisted in DBC)</summary>
    MEMBER ColumnName   := 100
END ENUM

