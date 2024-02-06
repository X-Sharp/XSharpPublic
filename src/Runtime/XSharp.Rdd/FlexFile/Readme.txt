Recycling deleted records in FlexFile

Flexfile introduced a mechanism to recycle deleted blocks in FPT files.
The FPT file can store tables of deleted blocks in the file. This is used to reuse the space of deleted records
The blocks are listed in tables:
1. This is the so called Length Index. This table contains entries sorted by the length of the block, and then the location
2. This is the so called Location Index. This table contains entries sorted by the location of the block, and then the length
The tables are stored in the FPT file and are used to find the best block to reuse when a new record is added
The FlexHeader contains the location of the tables in the FPT file

The tables are stored as normal blocks in the file, with a special type that indicates that this is an index block (numeric value 1000)
Deleted blocks are in the FPT file on the location that they were previously, and thei type is set to the value 1001

Index node is a page in the FPT file that has the following contents:
FPTDataToken:        8 Bytes (Type 4, Length 4), Type = 1000
bIsLeafNode:         1 bit       |
bIsHot:              1 bit       |   NodeInfo 2 bytes
usKeyCount:          14 bits     |   Number of keys in the node
filler:              16 bits     |
After that the node contains the keys

There are 2 types of index pages possible:
- Pages with 'leaf' nodes. Each leaf node contains 2 32 bit numbers
- Pages with 'branch' nodes. Each branch node contains 2 32 but numbers and a location of a next page in the tree, 
which can be another branch page or a leaf page
The table starts with an leaf node. When during the insertion of a new key in the leaf node the RDD discovers that the page is full, 
then the page is split into 2 pages, and a new parent page is created that points to the 2 new pages. 


Deleting a FPT block, Inseting blocks in the index
When a block in the FPT is no longer needed, then the RDD will do the following (assuming that the indexes are correct)
1. It calculates the size as multiple of the block sizes
2. It looks in the location index for the first block that follows the deleted block
3. When that block is immediately behind the deleted block, then it will merge that block with the deleted block and 
   update the indexes and delete that block from the index
4. Then the RDD checks the block then it will look at the block preceding the deleted block,
   and when that is adjacent then it will merge the deleted block before the block. 
   It will then update the indexes and then delete that block from the index
5. So if 3 or 4 were both successful, then the RDD will have merged 3 blocks into 1
6. If 3 or 4 were succesfull then the RDD will have merged 2 blocls into 1
7. If the deleted block is at the end of the file, then the file will be truncated.
8. Otherwise the RDD will set the type of the deleted block to 1001 and its length to the (merged) length of the block
   and will write that block to the FPT
9. Finally the RDD will insert the block in the location index and the length index
10. During this process it is possible that the last key from an index page was deleted. 
    In that case the page number of that index page is stored on a stack, and after the deleted blocks were 
    written then the RDD will start the same process for the index pages that are no more needed. So these will
    also be added to the Lenth and Location index with the same possible merging of blocks


Creating a new FPT block, looking up blocks in the index
When a new block is needed, then the RDD will do the following
1. It will look in the length index for the first block that is at least as large as the requested block
2. When it does not find a block, then the RDD will append the block to the end of the file
3. When it finds a block, then (since the index is sorted by length and then by location) it will take the first block
   that has a size >= the requested size. However it is possible that that block is too big.
   Assume we have found block of 1000 bytes and we need a block of 500 bytes. 
   Then the RDD will round the size the the nearest multiple of the block size, and will only use the first part of the block.
   The remaining part of the block will be added to the index as a new deleted block
4. The area in the FPT file for the remaining area will also be rewritten, to make sure that it starts with the deleted type
   and to make sure that the length is set correctly.

   

