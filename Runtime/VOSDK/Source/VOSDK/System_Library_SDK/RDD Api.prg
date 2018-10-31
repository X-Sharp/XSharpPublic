DEFINE MAXFILENAME     := 260
DEFINE MAX_EXT_NAME    := 5
DEFINE DB_MAXAREAS     := 1024
DEFINE MAXDRIVERNAME                := 12
DEFINE MAX_KEY_LEN                  := 256
DEFINE DBF_ANSI            := 0x04
DEFINE DBF_VER             := 0x03
DEFINE DBF_MEMO            := 0x80
DEFINE DBF_DB4MEMO         := 0x88
// ----------------------------------------------------------------------
//
// Bit field of first byte within the DBF file header
//
//   7  6  5  4  3  2  1  0
//  -- -- -- -- -- -- -- --
//   M  R  R  R  D  A  V  V
//
//   R = reserved
//   V = Number of dBASE version where this format were introducted
//   A = ANSI flag
//   D = DBASEIV flag for memofield structure
//   M = Memofield flag
//
/*
typedef VOSTRUCT
    {
    BYTE        nVer        : 2;
    BYTE        bitAnsi     : 1;
    BYTE        bitDBT4     : 1;
    BYTE        bitRes1     : 1;
    BYTE        bitRes2     : 1;
    BYTE        bitRes3     : 1;
    BYTE        bitDBT      : 1;
    } VERBYTE;
*/
//
//  Flags for bProdIndex (DBFNTX)
//
DEFINE DBF_PRODINDEX       := 0x01
DEFINE DBF_MEMOS           := 0x02
DEFINE DBF_ISDATABASE      := 0x04
DEFINE DBF_OLE             := 0x80;
   /*
typedef VOSTRUCT
    {
    BYTE        bitProduction   : 1;
    BYTE        bitMemo         : 1;
    BYTE        bitDatabase     : 1;
    BYTE        bitRes1         : 1;
    BYTE        bitRes2         : 1;
    BYTE        bitRes3         : 1;
    BYTE        bitRes4         : 1;
    BYTE        bitOLE          : 1;
    } PRODBYTE;
*/
#warning RDD_FUNCCOUNT
//DEFINE RDD_FUNCCOUNT  :=  _SIZEOF(_RDDFUNCS)/4
