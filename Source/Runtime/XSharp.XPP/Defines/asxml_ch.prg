/*
-----------------------------------------------------------------------
                      ===========================
                              Xbase++ XML
                      ===========================

DISCLAIMER
THIS SOURCECODE IS PROVIDED AS "IS". NO GUARANTY FOR APPLICABILITY AND
OPERATIVENESS. NO LIABILITY FOR DAMAGES. TO THE MAXIMUM EXTENT PERMITTED
BY APPLICABLE LAW, IN NO EVENT SHALL ALASKA SOFTWARE BE LIABLE FOR ANY
DAMAGES WHATSOEVER (INCLUDING, WITHOUT LIMITATION, DAMAGES FOR LOSS OF
BUSINESS PROFITS, BUSINESS INTERRUPTION, LOSS OF BUSINESS INFORMATION, OR
ANY OTHER PECUNIARY LOSS) ARISING OUT OF THE USE OF OR INABILITY TO USE
THIS ALASKA SOFTWARE PRODUCT, EVEN IF ALASKA SOFTWARE HAS BEEN ADVISED OF
THE POSSIBILITY OF SUCH DAMAGES. BECAUSE SOME STATES/JURISDICTIONS DO NOT
ALLOW THE EXCLUSION OR LIMITATION OF LIABILITY FOR CONSEQUENTIAL OR
INCIDENTAL DAMAGES, THE ABOVE LIMITATION MAY NOT APPLY TO YOU.


-----------------------------------------------------------------------

*/


/*
 * number of members in XMLTAG
 */
define XMLTAG_N_MEMBER     := 6
/*
 * indices of XMLTAG array member
 */
define XMLTAG_NAME         := 1
define XMLTAG_CONTENT      := 2
define XMLTAG_CHILD        := 3
define XMLTAG_ACTION       := 4
define XMLTAG_ATTRIB       := 5
define XMLTAG_OBJECT       := 6

/*
 * indices of attribute name and value
 */
define TAGATTR_NAME        := 1
define TAGATTR_VALUE       := 2
define TAGATTR_OBJECT      := 3

/*
 * number of entries in a XML_ERROR array
 */
define XML_ERROR_N_MEMBER   := 5
/*
 * indices of XML_ERROR entries
 */
define XML_ERROR_ID         := 1
define XML_ERROR_FILE       := 2
define XML_ERROR_LINE       := 3
define XML_ERROR_COLUMN     := 4
define XML_ERROR_ADDINFO    := 5

/*
 * XML error definitions
 */
define XMLDOC_ERROR_NO_ERROR         := 0    // no error
define XMLDOC_ERROR_OUT_OF_MEMORY    := 1    // not enough memory 
define XMLDOC_ERROR_INVALID_DTD_DECL := 2    // invalid DTD declaration
define XMLDOC_ERROR_INVALID_XML      := 3    // content outside XML tags
define XMLDOC_ERROR_ROOTTAG_EMPTY    := 4    // document has no root tag
define XMLDOC_ERROR_ENDTAG_MISSING   := 5    // invalid endtag or no endtag found
define XMLDOC_ERROR_EXPECT_DELIMIT   := 6    // expecting string delimiter
define XMLDOC_ERROR_UNTERM_STRING    := 7    // unterminated string
define XMLDOC_ERROR_FILE_NOT_FOUND   := 8    // file cannot be found
define XMLDOC_ERROR_READING_FILE     := 9    // file cannot be read
define XMLDOC_ERROR_NO_FILENAME      := 10   // no filename has been provided
define XMLDOC_ERROR_DUPLICATE_ATTR   := 11   // duplicate attribute
define XMLDOC_ERROR_MALFORMED_ATTR   := 12   // malformed attribute
define XMLDOC_ERROR_INVALID_ATTRNAME := 13   // invalid name for attribute
define XMLDOC_ERROR_PROCESS          := 14   // error WHILE processing action codeblocks

/*
 * return codes of action codeblocks that
 * control the processing of action codeblocks
 */
define XML_PROCESS_CONTINUE          := 0    // continue normal processing
define XML_PROCESS_ABORT             := 1    // abort processing of action codeblocks

