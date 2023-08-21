Structure of a CDX file

Important: All numbers in the CDX file are stored in Little Endian !

- Each file consists of pages, each page is 512 bytes. 
- In the DBFCDX driver we differentiate between:
  Header pages   - FileHeader and TagHeader 
  Tree pages


There are 3 types of tree pages: Branch (0), Root (1) and Leaf(2).
  Unused pages have the type -1 (0xFF) 
- The TAG header and Root of the orderbag have the same structure and are both 2 pages.
  they contain some flags and a pointer to the top page of the tag. it is followed by a page with the key
  expression and for expression. The positions of these strings in this second page are in the first page.
  Some of these values are meaningless in the root of the orderbag (there is no index expression) but the 
  fields are there and the KeyExprLen, ForExprPos and ForExpLen are all set to 1 in the root page.
  The root node in the File header points to the list of tags, the root node in the tag header points to 
  the first page of the tag. This can be either a BRANCH or a LEAF page. The TagList is  special LEAF page.

        type    description         start   length
        =====================================================================
		long	root;		        0x00	4       offset of the root node
		long	freePage;			0x04    4       offset of list of free pages, or -1
		long	version;			0x08	4       counter that increments when index is updated
		int2	keyLen;				0x0c	2       
		char	tagType;			0x0e	1       
		char	signature;			0x0f	1       
        int2    headerLen;          0x10    2       .
        int2    pageLen;            0x12    2       .
        long    collation           0x14    4       .
        char    reserved            0x18    68      . This area is often not filled
        char    lang                0x44    26      .
        char    collatver           0x76    4       .
        char    reserved            0x7a    372     .
        char    vfpcodepage         0x1ee   5       .
        char    ignorecase          0x1f3   1       .
        char    expression length   0x1f4   2       .
		int2	descend;			0x1f6   2
		int2	foxExprPos          0x1f8   2
		int2	forExprLen;			0x1fa   2
		int2    keyExprPos          0x1fc   2
		int2	keyExprLen;			0x1fe   2
        // the next page after the tag header has the key expression and for expression. 
        // The key expression is delimited with a zero byte. The for expression starts at the position
        // indicated in the tag header.


BRANCH Page
- Branch pages are used to link the tree. Their contents is
  BYTE     attr    [ 2 ];    node type = 1 for the top level branch and 0 for other branches
  BYTE     nKeys   [ 2 ];    number of keys 
  BYTE     leftPtr [ 4 ];    offset of left node or -1 
  BYTE     rightPtr[ 4 ];    offset of right node or -1
  BYTE     freeSpc [ 2 ];    free space available in a page 
  // array of key entries
  // each key entry is keyLen + 8 bytes long
  // BYTE Key data [keyLen]
  // BYTE record number[ 4]
  // BYTE child page [4]
  The recno/key on the branch page is the same as the last recno/key of the child page that they point to
  So this key should not be processed when skipping.
  This into is only used to quickly determine which child page should be selected.
  So the last key on the root page is also the last key in the file.
  There can be more than one level of branch keys, but the bottom level in the tree consists of leaf pages.
       
LEAF Page
- A Leaf page has type 2. The List of CDX tags is a special leaf page with type 3 (Root + Leaf)
- It starts with 24 bytes:
  BYTE     attr    [ 2 ];    node type = 2 or 3 (for the taglist)
  BYTE     nKeys   [ 2 ];    number of keys 
  BYTE     leftPtr [ 4 ];    offset of left node or -1 
  BYTE     rightPtr[ 4 ];    offset of right node or -1
  BYTE     freeSpc [ 2 ];    free space available in a page 
  BYTE     recMask [ 4 ];    record number mask 
  BYTE     dupMask;          duplicate bytes count mask 
  BYTE     trlMask;          trailing bytes count mask 
  BYTE     recBits;          number of bits for record number 
  BYTE     dupBits;          number of bits for duplicate count
  BYTE     trlBits;          number of bits for trailing count 
  BYTE     keyBytes;         total number of bytes for recno/dup/trail info */ 
  The remaining 488 bytes are used by the list of key descriptors (which starts immediately after this header) and the keys
  (which are stored from the end of the page forward)
  Each key descriptor consists of 3 numbers
  1) The record number
  2) The number of Duplicate bytes (so for 2 keys following eachother with the same start bytes only the different bytes for the 2nd one are stored)
  3) The number of Trailing bytes (trailing spaces are trimmed)
  The header describes how many bits each of these elements occupies and how many bytes (in keyBytes) the 3 together 
  (often 4 bytes, but for larger CDX files more bytes may be needed for the recno, so more bytes will be used for the key descriptors)
  The header also contains "masks" that help to mask out the record number, duplicate and trailing bytes counts
  This is stored in the file as dddddtttttrrrrrrrrrr  where ddd are the duplicate bits, tttt the trail bits and rrrr the recno bits

  Key data is stored as:
  Character: ASCII
  Date     : Julian date converted to number, so also stored as 8 bytes
  Number   : IEEE double (8 bytes), swapped the order of bytes. When negative invert all bits, otherwise only highest order bit.
             This is complicated but allows to use memcmp to compare numeric values

  


