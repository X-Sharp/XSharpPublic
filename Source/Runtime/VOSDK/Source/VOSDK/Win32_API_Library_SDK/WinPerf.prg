VOSTRUCT _WINPERF_DATA_BLOCK
	MEMBER  DIM Signature[4] AS WORD	//RvdH 070412 changed from BYTE to WORD
	MEMBER  LittleEndian AS DWORD
	MEMBER  Version AS DWORD
	MEMBER  Revision AS DWORD
	MEMBER  TotalByteLength AS DWORD
	MEMBER  HeaderLength AS DWORD
	MEMBER  NumObjectTypes AS DWORD
	MEMBER  DefaultObject AS LONGINT




	MEMBER  SystemTime IS _winSYSTEMTIME

	MEMBER  PerfTime IS _winLARGE_INTEGER

	MEMBER  PerfFreq IS _winLARGE_INTEGER

	MEMBER  PerfTime100nSec IS _winLARGE_INTEGER

	MEMBER  SystemNameLength AS DWORD
	MEMBER  SystemNameOffset AS DWORD






VOSTRUCT _winPERF_OBJECT_TYPE
	MEMBER  TotalByteLength AS DWORD







	MEMBER  DefinitionLength AS DWORD







	MEMBER  HeaderLength AS DWORD



	MEMBER  ObjectNameTitleIndex AS DWORD

	MEMBER  ObjectNameTitle AS PSZ


	MEMBER  ObjectHelpTitleIndex AS DWORD

	MEMBER  ObjectHelpTitle AS PSZ


	MEMBER  DetailLevel AS DWORD



	MEMBER  NumCounters AS DWORD


	MEMBER  DefaultCounter AS LONGINT



	MEMBER  NumInstances AS LONGINT











	MEMBER  CodePage AS DWORD


	MEMBER  PerfTime IS _winLARGE_INTEGER

	MEMBER  PerfFreq IS _winLARGE_INTEGER


VOSTRUCT _winPERF_COUNTER_DEFINITION
	MEMBER  ByteLength AS DWORD
	MEMBER  CounterNameTitleIndex AS DWORD


	MEMBER  CounterNameTitle AS PSZ


	MEMBER  CounterHelpTitleIndex AS DWORD


	MEMBER  CounterHelpTitle AS PSZ


	MEMBER  DefaultScale AS LONGINT


	MEMBER  DetailLevel AS DWORD

	MEMBER  CounterType  AS DWORD
	MEMBER  CounterSize  AS DWORD
	MEMBER  CounterOffset   AS DWORD



//
//
//  If (PERF_DATA_BLOCK.NumInstances >= 0) then there will be
//  PERF_DATA_BLOCK.NumInstances of a (PERF_INSTANCE_DEFINITION
//  followed by a PERF_COUNTER_BLOCK followed by the counter data fields)
//  for each instance.
//
//  If (PERF_DATA_BLOCK.NumInstances < 0) then the counter definition
//  strucutre above will be followed by only a PERF_COUNTER_BLOCK and the
//  counter data for that COUNTER.
//


VOSTRUCT _winPERF_INSTANCE_DEFINITION

	MEMBER  ByteLength AS DWORD

	MEMBER  ParentObjectTitleIndex AS DWORD





	MEMBER  ParentObjectInstance AS DWORD



	MEMBER  UniqueID AS LONGINT


	MEMBER  NameOffset AS DWORD


	MEMBER  NameLength AS DWORD
//
//  If .ParentObjectName is 0, there
//  is no parent-child hierarchy for this object type.  Otherwise,
//   the .ParentObjectInstance is an index, starting at 0, into the
//  instances reported for the parent object type.  It is only
//  meaningful if .ParentObjectName is not 0.  The purpose of all this
//  is to permit reporting/summation of object instances like threads
//  within processes, and logical drives within physical drives.
//
//
//  The PERF_INSTANCE_DEFINITION will be followed by a PERF_COUNTER_BLOCK.
//




VOSTRUCT _winPERF_COUNTER_BLOCK
	MEMBER  ByteLength AS DWORD
//
//  The PERF_COUNTER_BLOCK is followed by PERF_OBJECT_TYPE.NumCounters
//  number of counters.
//

//
// Support for New Extensible API starting with NT 5.0
//



#region defines
DEFINE PERF_DATA_VERSION   := 1
DEFINE PERF_DATA_REVISION  := 1
DEFINE PERF_NO_INSTANCES          := -1
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//
//  PERF_COUNTER_DEFINITION.CounterType field values
//
//
//        Counter ID Field Definition:
//
//   3      2        2    2    2        1        1    1
//   1      8        4    2    0        6        2    0    8                0
//  +--------+--------+----+----+--------+--------+----+----+----------------+
//  |Display |Calculation  |Time|Counter |        |Ctr |Size|                |
//  |Flags   |Modifiers    |Base|SubType |Reserved|Type|Fld |   Reserved     |
//  +--------+--------+----+----+--------+--------+----+----+----------------+
//
//
//  The counter type is the "or" of the following values as described below
//
//  select one of the following to indicate the counter's data size
//
DEFINE PERF_SIZE_DWORD         := 0x00000000
DEFINE PERF_SIZE_LARGE         := 0x00000100
DEFINE PERF_SIZE_ZERO          := 0x00000200
DEFINE PERF_SIZE_VARIABLE_LEN  := 0x00000300
//
//  select one of the following values to indicate the counter field usage
//
DEFINE PERF_TYPE_NUMBER        := 0x00000000
DEFINE PERF_TYPE_COUNTER       := 0x00000400
DEFINE PERF_TYPE_TEXT          := 0x00000800
DEFINE PERF_TYPE_ZERO          := 0x00000C00
//
//  If the PERF_TYPE_NUMBER field was selected, then select one of the
//  following to describe the Number
//
DEFINE PERF_NUMBER_HEX         := 0x00000000
DEFINE PERF_NUMBER_DECIMAL     := 0x00010000
DEFINE PERF_NUMBER_DEC_1000    := 0x00020000
//
//  If the PERF_TYPE_COUNTER value was selected then select one of the
//  following to indicate the type of counter
//
DEFINE PERF_COUNTER_VALUE      := 0x00000000
DEFINE PERF_COUNTER_RATE       := 0x00010000
DEFINE PERF_COUNTER_FRACTION   := 0x00020000
DEFINE PERF_COUNTER_BASE       := 0x00030000
DEFINE PERF_COUNTER_ELAPSED    := 0x00040000
DEFINE PERF_COUNTER_QUEUELEN   := 0x00050000
DEFINE PERF_COUNTER_HISTOGRAM  := 0x00060000
DEFINE PERF_COUNTER_PRECISION  := 0x00070000  // divide ctr / private clock
//
//  If the PERF_TYPE_TEXT value was selected, then select one of the
//  following to indicate the type of TEXT data.
//
DEFINE PERF_TEXT_UNICODE       := 0x00000000
DEFINE PERF_TEXT_ASCII         := 0x00010000
//
//  Timer SubTypes
//
DEFINE PERF_TIMER_TICK         := 0x00000000
DEFINE PERF_TIMER_100NS        := 0x00100000
DEFINE PERF_OBJECT_TIMER       := 0x00200000
//
//  Any types that have calculations performed can use one or more of
//  the following calculation modification flags listed here
//
DEFINE PERF_DELTA_COUNTER      := 0x00400000
DEFINE PERF_DELTA_BASE         := 0x00800000
DEFINE PERF_INVERSE_COUNTER    := 0x01000000
DEFINE PERF_MULTI_COUNTER      := 0x02000000
//
//  Select one of the following values to indicate the display suffix (if any)
//
DEFINE PERF_DISPLAY_NO_SUFFIX  := 0x00000000
DEFINE PERF_DISPLAY_PER_SEC    := 0x10000000
DEFINE PERF_DISPLAY_PERCENT    := 0x20000000
DEFINE PERF_DISPLAY_SECONDS    := 0x30000000
DEFINE PERF_DISPLAY_NOSHOW     := 0x40000000
//
//  Predefined counter types
//
// 32-bit Counter.  Divide delta by delta time.  Display suffix: "/sec"
DEFINE PERF_COUNTER_COUNTER := 0x10410400
// 64-bit Timer.  Divide delta by delta time.  Display suffix: "%"
DEFINE PERF_COUNTER_TIMER   := 0x10410500
DEFINE PERF_COUNTER_QUEUELEN_TYPE := 0x00450400
DEFINE PERF_COUNTER_BULK_COUNT := 0x10410500
DEFINE PERF_COUNTER_TEXT      := 0x00000B00
// Indicates the data is a counter  which should not be
// time averaged on display (such as an error counter on a serial line)
// Display as is.  No Display Suffix.
DEFINE PERF_COUNTER_RAWCOUNT   := 0x00010000
// Same as PERF_COUNTER_RAWCOUNT except its size is a large integer
DEFINE PERF_COUNTER_LARGE_RAWCOUNT := 0x00010100
// Special case for RAWCOUNT that want to be displayed in hex
// Indicates the data is a counter  which should not be
// time averaged on display (such as an error counter on a serial line)
// Display as is.  No Display Suffix.
DEFINE PERF_COUNTER_RAWCOUNT_HEX  := 0x00000000
DEFINE PERF_COUNTER_LARGE_RAWCOUNT_HEX  := 0x00000100
DEFINE PERF_SAMPLE_FRACTION  := 0x20c20400
DEFINE PERF_SAMPLE_COUNTER   := 0x00410400
DEFINE PERF_COUNTER_NODATA   := 0x40000200
DEFINE PERF_COUNTER_TIMER_INV  := 0x21410500
DEFINE PERF_SAMPLE_BASE  := 0x40030401
DEFINE PERF_AVERAGE_TIMER   := 0x30020400
DEFINE PERF_AVERAGE_BASE    := 0x40030402
DEFINE PERF_AVERAGE_BULK   := 0x4020500
DEFINE PERF_100NSEC_TIMER  := 0x20510500
DEFINE PERF_100NSEC_TIMER_INV  := 0x21500500
DEFINE PERF_COUNTER_MULTI_TIMER  := 0x22410500
DEFINE PERF_COUNTER_MULTI_TIMER_INV  := 0x23410500
DEFINE PERF_COUNTER_MULTI_BASE    := 0x42030500
DEFINE PERF_100NSEC_MULTI_TIMER   := 0x22510500
DEFINE PERF_100NSEC_MULTI_TIMER_INV := 0x23510500
DEFINE PERF_RAW_FRACTION   := 0x20020400
DEFINE PERF_RAW_BASE       := 0x40030403
DEFINE PERF_ELAPSED_TIME     := 0x30240500
DEFINE PERF_COUNTER_HISTOGRAM_TYPE   := 0x80000000
DEFINE PERF_DETAIL_NOVICE          := 100
DEFINE PERF_DETAIL_ADVANCED        := 200
DEFINE PERF_DETAIL_EXPERT          := 300
DEFINE PERF_DETAIL_WIZARD          := 400
DEFINE PERF_NO_UNIQUE_ID   := -1
DEFINE     PERF_QUERY_OBJECTS      := 0x80000000L
DEFINE     PERF_QUERY_GLOBAL       := 0x80000001L
DEFINE     PERF_QUERY_COSTLY       := 0x80000002L
//
// The following are the possible values for
// HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Perflib\EventLogLevel
// The default is WINPERF_LOG_NONE if the registry value is not defined.
// This should be adopted by all perfdlls to avoid flooding the application
// event log.
//
DEFINE  WINPERF_LOG_NONE       := 0           // No event reported
DEFINE  WINPERF_LOG_USER       := 1           // Report only errors
DEFINE  WINPERF_LOG_DEBUG      := 2           // Report debug errors as well
DEFINE  WINPERF_LOG_VERBOSE    := 3           // Report everything
#endregion
