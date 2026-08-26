//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.IO
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests

    CLASS XmlToCursorTests

        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        #region helpers
        PRIVATE METHOD Stru(cAlias AS STRING) AS STRING
            LOCAL aStru   AS ARRAY
            LOCAL cResult AS STRING
            LOCAL n       AS DWORD
            DbSelectArea(cAlias)
            aStru   := DbStruct()
            cResult := ""
            FOR n := 1 UPTO ALen(aStru)
                cResult += Upper(AllTrim(aStru[n, 1])) + " " + Upper(aStru[n, 2]) + ;
                    "(" + AllTrim(Str(aStru[n, 3])) + "," + AllTrim(Str(aStru[n, 4])) + ") "
            NEXT
            RETURN AllTrim(cResult)
        END METHOD

        PRIVATE METHOD Close(cAlias AS STRING) AS VOID
            IF Used(cAlias)
                DbCloseArea(cAlias)
            ENDIF
        END METHOD

        PRIVATE METHOD Doc(cRows AS STRING) AS STRING
            RETURN "<VFPData>" + cRows + "</VFPData>"
        END METHOD

        PRIVATE METHOD OneCol(cValues AS STRING) AS STRING
            LOCAL cResult AS STRING
            cResult := ""
            FOREACH cValue AS STRING IN cValues:Split(c'|')
                cResult += "<t><v>" + cValue + "</v></t>"
            NEXT
            RETURN SELF:Doc(cResult)
        END METHOD

        PRIVATE METHOD Infer(cValues AS STRING) AS STRING
            LOCAL cAlias AS STRING
            LOCAL cResult AS STRING
            cAlias := "inf" + Right(Guid.NewGuid():ToString("N"), 6)
            XmlToCursor(SELF:OneCol(cValues), cAlias, 0)
            cResult := SELF:Stru(cAlias)
            SELF:Close(cAlias)
            RETURN cResult
        END METHOD

        // The XML VFP 9 itself produces for a cursor holding one row of each
        // interesting type plus one empty row. It is embedded verbatim rather than
        // regenerated with CursorToXml(): our own writer emits every column as
        // xs:string, so a round trip through it would prove nothing about the
        // types XmlToCursor() is supposed to rebuild.
        PRIVATE METHOD TypesXml(lSchema AS LOGIC) AS STRING
            IF lSchema
                RETURN SELF:VfpXmlWithSchema()
            ENDIF
            RETURN SELF:VfpXmlNoSchema()
        END METHOD

        // Captured verbatim from VFP 9: one row of each type plus one empty row, with the inline schema
        PRIVATE METHOD VfpXmlWithSchema() AS STRING
            LOCAL oSb AS System.Text.StringBuilder
            oSb := System.Text.StringBuilder{}
            oSb:AppendLine("<?xml version = ""1.0"" encoding=""Windows-1252"" standalone=""yes""?>")
            oSb:AppendLine("<VFPData>")
            oSb:AppendLine("	<xsd:schema id=""VFPData"" xmlns:xsd=""http://www.w3.org/2001/XMLSchema"" xmlns:msdata=""urn:schemas-microsoft-com:xml-msdata"">")
            oSb:AppendLine("		<xsd:element name=""VFPData"" msdata:IsDataSet=""true"">")
            oSb:AppendLine("			<xsd:complexType>")
            oSb:AppendLine("				<xsd:choice maxOccurs=""unbounded"">")
            oSb:AppendLine("					<xsd:element name=""src"" minOccurs=""0"" maxOccurs=""unbounded"">")
            oSb:AppendLine("						<xsd:complexType>")
            oSb:AppendLine("							<xsd:sequence>")
            oSb:AppendLine("								<xsd:element name=""cchar"">")
            oSb:AppendLine("									<xsd:simpleType>")
            oSb:AppendLine("										<xsd:restriction base=""xsd:string"">")
            oSb:AppendLine("											<xsd:maxLength value=""10""/>")
            oSb:AppendLine("										</xsd:restriction>")
            oSb:AppendLine("									</xsd:simpleType>")
            oSb:AppendLine("								</xsd:element>")
            oSb:AppendLine("								<xsd:element name=""nnum"">")
            oSb:AppendLine("									<xsd:simpleType>")
            oSb:AppendLine("										<xsd:restriction base=""xsd:decimal"">")
            oSb:AppendLine("											<xsd:totalDigits value=""7""/>")
            oSb:AppendLine("											<xsd:fractionDigits value=""2""/>")
            oSb:AppendLine("										</xsd:restriction>")
            oSb:AppendLine("									</xsd:simpleType>")
            oSb:AppendLine("								</xsd:element>")
            oSb:AppendLine("								<xsd:element name=""llog"" type=""xsd:boolean""/>")
            oSb:AppendLine("								<xsd:element name=""ddate"" type=""xsd:date""/>")
            oSb:AppendLine("								<xsd:element name=""ttime"" type=""xsd:dateTime""/>")
            oSb:AppendLine("								<xsd:element name=""ycur"">")
            oSb:AppendLine("									<xsd:simpleType>")
            oSb:AppendLine("										<xsd:restriction base=""xsd:decimal"">")
            oSb:AppendLine("											<xsd:totalDigits value=""19""/>")
            oSb:AppendLine("											<xsd:fractionDigits value=""4""/>")
            oSb:AppendLine("										</xsd:restriction>")
            oSb:AppendLine("									</xsd:simpleType>")
            oSb:AppendLine("								</xsd:element>")
            oSb:AppendLine("								<xsd:element name=""iint"" type=""xsd:int""/>")
            oSb:AppendLine("								<xsd:element name=""mmemo"">")
            oSb:AppendLine("									<xsd:simpleType>")
            oSb:AppendLine("										<xsd:restriction base=""xsd:string"">")
            oSb:AppendLine("											<xsd:maxLength value=""2147483647""/>")
            oSb:AppendLine("										</xsd:restriction>")
            oSb:AppendLine("									</xsd:simpleType>")
            oSb:AppendLine("								</xsd:element>")
            oSb:AppendLine("								<xsd:element name=""bdbl"" type=""xsd:double""/>")
            oSb:AppendLine("							</xsd:sequence>")
            oSb:AppendLine("						</xsd:complexType>")
            oSb:AppendLine("					</xsd:element>")
            oSb:AppendLine("				</xsd:choice>")
            oSb:AppendLine("				<xsd:anyAttribute namespace=""http://www.w3.org/XML/1998/namespace"" processContents=""lax""/>")
            oSb:AppendLine("			</xsd:complexType>")
            oSb:AppendLine("		</xsd:element>")
            oSb:AppendLine("	</xsd:schema>")
            oSb:AppendLine("	<src>")
            oSb:AppendLine("		<cchar>hello</cchar>")
            oSb:AppendLine("		<nnum>123.45</nnum>")
            oSb:AppendLine("		<llog>true</llog>")
            oSb:AppendLine("		<ddate>2024-01-15</ddate>")
            oSb:AppendLine("		<ttime>2024-01-15T10:30:45</ttime>")
            oSb:AppendLine("		<ycur>99.9999</ycur>")
            oSb:AppendLine("		<iint>42</iint>")
            oSb:AppendLine("		<mmemo>memo text</mmemo>")
            oSb:AppendLine("		<bdbl>3.142</bdbl>")
            oSb:AppendLine("	</src>")
            oSb:AppendLine("	<src>")
            oSb:AppendLine("		<cchar/>")
            oSb:AppendLine("		<nnum>0.00</nnum>")
            oSb:AppendLine("		<llog>false</llog>")
            oSb:AppendLine("		<ddate/>")
            oSb:AppendLine("		<ttime/>")
            oSb:AppendLine("		<ycur>0.0000</ycur>")
            oSb:AppendLine("		<iint>0</iint>")
            oSb:AppendLine("		<mmemo/>")
            oSb:AppendLine("		<bdbl>0.000</bdbl>")
            oSb:AppendLine("	</src>")
            oSb:AppendLine("</VFPData>")
            RETURN oSb:ToString()
        END METHOD

        // The same cursor, written by VFP 9 without a schema
        PRIVATE METHOD VfpXmlNoSchema() AS STRING
            LOCAL oSb AS System.Text.StringBuilder
            oSb := System.Text.StringBuilder{}
            oSb:AppendLine("<?xml version = ""1.0"" encoding=""Windows-1252"" standalone=""yes""?>")
            oSb:AppendLine("<VFPData>")
            oSb:AppendLine("	<src>")
            oSb:AppendLine("		<cchar>hello</cchar>")
            oSb:AppendLine("		<nnum>123.45</nnum>")
            oSb:AppendLine("		<llog>true</llog>")
            oSb:AppendLine("		<ddate>2024-01-15</ddate>")
            oSb:AppendLine("		<ttime>2024-01-15T10:30:45</ttime>")
            oSb:AppendLine("		<ycur>99.9999</ycur>")
            oSb:AppendLine("		<iint>42</iint>")
            oSb:AppendLine("		<mmemo>memo text</mmemo>")
            oSb:AppendLine("		<bdbl>3.142</bdbl>")
            oSb:AppendLine("	</src>")
            oSb:AppendLine("	<src>")
            oSb:AppendLine("		<cchar/>")
            oSb:AppendLine("		<nnum>0.00</nnum>")
            oSb:AppendLine("		<llog>false</llog>")
            oSb:AppendLine("		<ddate/>")
            oSb:AppendLine("		<ttime/>")
            oSb:AppendLine("		<ycur>0.0000</ycur>")
            oSb:AppendLine("		<iint>0</iint>")
            oSb:AppendLine("		<mmemo/>")
            oSb:AppendLine("		<bdbl>0.000</bdbl>")
            oSb:AppendLine("	</src>")
            oSb:AppendLine("</VFPData>")
            RETURN oSb:ToString()
        END METHOD
        #endregion

        #region round trip through the inline schema

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD SchemaRebuildsEveryFieldType AS VOID
            VAR cXml := SELF:TypesXml(TRUE)
            SELF:Close("dst")
            Assert.Equal(2, (INT) XmlToCursor(cXml, "dst", 0))
            Assert.Equal("CCHAR C(10,0) NNUM N(8,2) LLOG L(1,0) DDATE D(8,0) TTIME T(8,0) " + ;
                "YCUR N(20,4) IINT I(4,0) MMEMO M(4,0) BDBL B(8,0)", SELF:Stru("dst"))
            SELF:Close("dst")
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD Flag2048RebuildsCurrency AS VOID
            VAR cXml := SELF:TypesXml(TRUE)
            SELF:Close("dst")
            XmlToCursor(cXml, "dst", 2048)
            DbSelectArea("dst")
            Assert.Equal("Y", DbFieldInfo(DBS_TYPE, FieldPos("ycur")))
            SELF:Close("dst")
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD SchemaRoundTripKeepsValues AS VOID
            VAR cXml := SELF:TypesXml(TRUE)
            SELF:Close("dst")
            XmlToCursor(cXml, "dst", 0)
            DbSelectArea("dst")
            DbGoTop()
            Assert.Equal("hello", AllTrim(dst->cchar))
            Assert.Equal(123.45, (REAL8) dst->nnum, 2)
            Assert.True(dst->llog)
            Assert.Equal(2024, (INT) Year(dst->ddate))
            Assert.Equal(42, (INT) dst->iint)
            Assert.Equal("memo text", AllTrim(dst->mmemo))
            SELF:Close("dst")
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD SchemaHandlesTheEmptyDateVfpWrites AS VOID
            VAR cXml := SELF:TypesXml(TRUE)
            SELF:Close("dst")
            Assert.Equal(2, (INT) XmlToCursor(cXml, "dst", 0))
            DbSelectArea("dst")
            DbGoBottom()
            Assert.True(Empty(dst->ddate))
            SELF:Close("dst")
        END METHOD

        #endregion
        #region inference when there is no schema

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD NoSchemaInfersFromTheValues AS VOID
            VAR cXml := SELF:TypesXml(FALSE)
            SELF:Close("dst")
            Assert.Equal(2, (INT) XmlToCursor(cXml, "dst", 0))
            Assert.Equal("CCHAR C(5,0) NNUM N(6,2) LLOG L(1,0) DDATE C(10,0) TTIME C(19,0) " + ;
                "YCUR N(7,4) IINT N(2,0) MMEMO C(9,0) BDBL N(5,3)", SELF:Stru("dst"))
            SELF:Close("dst")
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD WidthIsTheLongestValueSeen AS VOID
            Assert.Equal("V C(10,0)", SELF:Infer("abc|abcdefghij"))
            Assert.Equal("V N(7,3)",  SELF:Infer("1|333.456"))
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD BooleanValuesBecomeLogical AS VOID
            Assert.Equal("V L(1,0)", SELF:Infer("true|false"))
            Assert.Equal("V L(1,0)", SELF:Infer("TRUE|FALSE"))
            Assert.Equal("V L(1,0)", SELF:Infer("1|0"))
            Assert.Equal("V L(1,0)", SELF:Infer("1|true"))
            Assert.Equal("V C(4,0)", SELF:Infer("true|xyz"))
            Assert.Equal("V N(1,0)", SELF:Infer("1|2"))
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD DatesAndDateTimesAreInferred AS VOID
            Assert.Equal("V D(8,0)", SELF:Infer("2024-01-15|2024-02-20"))
            Assert.Equal("V T(8,0)", SELF:Infer("2024-01-15T10:30:45"))
            Assert.Equal("V C(19,0)", SELF:Infer("2024-01-15|2024-01-15T10:30:45"))
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD LongTextBecomesMemo AS VOID
            Assert.Equal("V C(254,0)", SELF:Infer(Replicate("y", 254)))
            Assert.Equal("V M(4,0)",   SELF:Infer(Replicate("z", 255)))
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD EmptyColumnBecomesCharacterOfOne AS VOID
            Assert.Equal("V C(1,0)", SELF:Infer("|"))
        END METHOD

        #endregion

        #region inference quirks reproduced from VFP on purpose

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD SignedNumbersAreInferredAsCharacter AS VOID
            Assert.Equal("V C(2,0)", SELF:Infer("-5"))
            Assert.Equal("V C(2,0)", SELF:Infer("+5"))
            Assert.Equal("V C(7,0)", SELF:Infer("-123.45"))
            Assert.Equal("V C(4,0)", SELF:Infer("-5|1234"))
            Assert.Equal("V N(4,0)", SELF:Infer("1234"))
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD ExponentsAreInferredAsCharacter AS VOID
            Assert.Equal("V C(3,0)", SELF:Infer("1e3|25"))
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD LeadingAndTrailingDotsAreStillNumeric AS VOID
            Assert.Equal("V N(2,1)", SELF:Infer(".5|.7"))
            Assert.Equal("V N(2,0)", SELF:Infer("5.|7."))
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD AnEmptyValueRulesOutLogicalButNotNumeric AS VOID
            Assert.Equal("V N(1,0)", SELF:Infer("1|0|"))
            Assert.Equal("V C(4,0)", SELF:Infer("true|"))
            Assert.Equal("V N(4,1)", SELF:Infer("12.5|"))
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD AnEmptyValueRulesOutDate AS VOID
            Assert.Equal("V C(10,0)", SELF:Infer("2024-01-15|"))
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD InferredWidthCanOverflow AS VOID
            Assert.Equal("V N(5,1)", SELF:Infer("99999|0.1"))
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD LeadingZerosAreLost AS VOID
            Assert.Equal("V N(3,0)", SELF:Infer("007|12"))
            Assert.Equal("V C(3,0)", SELF:Infer("007|ab"))
        END METHOD

        #endregion
        #region cursor name, work area and return value

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD MissingNameCreatesXmlResult AS VOID
            SELF:Close("XMLRESULT")
            Assert.Equal(1, (INT) XmlToCursor(SELF:Doc("<t><a>1</a></t>")))
            Assert.True(Used("XMLRESULT"))
            Assert.Equal("XMLRESULT", Alias())
            SELF:Close("XMLRESULT")
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD EmptyNameCreatesXmlResult AS VOID
            SELF:Close("XMLRESULT")
            XmlToCursor(SELF:Doc("<t><a>1</a></t>"), "", 0)
            Assert.True(Used("XMLRESULT"))
            SELF:Close("XMLRESULT")
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD TheNewCursorBecomesTheCurrentArea AS VOID
            SELF:Close("parked")
            SELF:Close("landed")
            CREATE CURSOR parked (zz C(3))
            DbSelectArea("parked")
            XmlToCursor(SELF:Doc("<t><a>1</a></t>"), "landed", 0)
            Assert.Equal("LANDED", Alias())
            Assert.Equal(1, (INT) RecNo())
            SELF:Close("landed")
            SELF:Close("parked")
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD AnExistingCursorIsReplaced AS VOID
            SELF:Close("reuse")
            CREATE CURSOR reuse (zz C(3))
            INSERT INTO reuse VALUES ("aaa")
            XmlToCursor(SELF:Doc("<t><a>1</a></t>"), "reuse", 0)
            // "1" on its own is Logical, not Numeric: it is inside {0,1,true,false}
            Assert.Equal("A L(1,0)", SELF:Stru("reuse"))
            SELF:Close("reuse")
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD EmptyDocumentCreatesNoCursor AS VOID
            SELF:Close("nothing")
            SELF:Close("norecords")
            // the return is RecCount() of the current area, so it has to be a known one
            CREATE CURSOR norecords (zz C(1))
            DbSelectArea("norecords")
            Assert.Equal(0, (INT) XmlToCursor("<VFPData></VFPData>", "nothing", 0))
            Assert.False(Used("nothing"))
            SELF:Close("norecords")
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD ReturnIsRecCountOfTheCurrentArea AS VOID
            SELF:Close("target")
            SELF:Close("parked")
            CREATE CURSOR target (cchar C(20))
            INSERT INTO target VALUES ("pre1")
            INSERT INTO target VALUES ("pre2")
            CREATE CURSOR parked (zz C(3))
            DbSelectArea("parked")
            Assert.Equal(0, (INT) XmlToCursor(SELF:Doc("<t><cchar>a</cchar></t><t><cchar>b</cchar></t>"), "target", 8192))
            Assert.Equal("PARKED", Alias())
            Assert.Equal(4, (INT) RecCount("target"))
            SELF:Close("target")
            SELF:Close("parked")
        END METHOD

        #endregion

        #region append mode

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD AppendAddsRowsAndKeepsTheStructure AS VOID
            SELF:Close("tgt")
            CREATE CURSOR tgt (cchar C(20), nnum N(10,2), zzz C(5))
            INSERT INTO tgt VALUES ("preexisting", 9.99, "keep")
            DbSelectArea("tgt")
            Assert.Equal(3, (INT) XmlToCursor(SELF:TypesXml(TRUE), "tgt", 8192))
            Assert.Equal("CCHAR C(20,0) NNUM N(10,2) ZZZ C(5,0)", SELF:Stru("tgt"))
            DbSelectArea("tgt")
            DbGoTop()
            Assert.Equal("preexisting", AllTrim(tgt->cchar))
            DbSkip(1)
            Assert.Equal("hello", AllTrim(tgt->cchar))
            Assert.Equal(123.45, (REAL8) tgt->nnum, 2)
            Assert.Equal("", AllTrim(tgt->zzz))
            SELF:Close("tgt")
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD AppendMatchesFieldNamesCaseInsensitively AS VOID
            SELF:Close("tgt6")
            CREATE CURSOR tgt6 (myfield C(5))
            DbSelectArea("tgt6")
            XmlToCursor(SELF:Doc("<t><MYFIELD>up</MYFIELD></t>"), "tgt6", 8192)
            DbSelectArea("tgt6")
            DbGoTop()
            Assert.Equal("up", AllTrim(tgt6->myfield))
            SELF:Close("tgt6")
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD AppendWithNoMatchingColumnStillAddsBlankRows AS VOID
            SELF:Close("tgt2")
            CREATE CURSOR tgt2 (qqq C(5))
            DbSelectArea("tgt2")
            Assert.Equal(2, (INT) XmlToCursor(SELF:Doc("<t><a>1</a></t><t><a>2</a></t>"), "tgt2", 8192))
            Assert.Equal(2, (INT) RecCount("tgt2"))
            SELF:Close("tgt2")
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD AppendCoercesToTheTargetFieldType AS VOID
            SELF:Close("tgt4")
            CREATE CURSOR tgt4 (ddate D, llog L, nnum N(6,2), cchar C(4))
            DbSelectArea("tgt4")
            XmlToCursor(SELF:Doc("<t><ddate>2024-01-15</ddate><llog>true</llog>" + ;
                "<nnum>12.3456</nnum><cchar>abcdefgh</cchar></t>"), "tgt4", 8192)
            DbSelectArea("tgt4")
            DbGoTop()
            Assert.Equal(2024, (INT) Year(tgt4->ddate))
            Assert.Equal(1, (INT) Month(tgt4->ddate))
            Assert.True(tgt4->llog)
            Assert.Equal(12.35, (REAL8) tgt4->nnum, 2)   // rounded to the field scale
            Assert.Equal("abcd", tgt4->cchar)            // truncated to the field width
            SELF:Close("tgt4")
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD AppendWithEmptyNameUsesTheCurrentArea AS VOID
            SELF:Close("cur3")
            CREATE CURSOR cur3 (cchar C(20))
            DbSelectArea("cur3")
            Assert.Equal(2, (INT) XmlToCursor(SELF:Doc("<t><cchar>a</cchar></t><t><cchar>b</cchar></t>"), "", 8192))
            Assert.Equal(2, (INT) RecCount("cur3"))
            SELF:Close("cur3")
        END METHOD

        #endregion

        #region source flavours

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD Flag512ReadsFromAFile AS VOID
            VAR cFile := Path.Combine(Path.GetTempPath(), "X2CIN_" + Guid.NewGuid():ToString("N") + ".xml")
            File.WriteAllText(cFile, SELF:TypesXml(TRUE))
            SELF:Close("fromfile")
            Assert.Equal(2, (INT) XmlToCursor(cFile, "fromfile", 512))
            SELF:Close("fromfile")
            File.Delete(cFile)
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD AttributeFormatIsRead AS VOID
            SELF:Close("attr")
            Assert.Equal(1, (INT) XmlToCursor(SELF:Doc("<src2 cc=""ab"" nn=""1.5""/>"), "attr", 0))
            Assert.Equal("CC C(2,0) NN N(3,1)", SELF:Stru("attr"))
            SELF:Close("attr")
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD AttributesAreDroppedWhenTheRowAlsoHasElements AS VOID
            SELF:Close("mix")
            XmlToCursor(SELF:Doc("<t a=""1""><b>x</b></t>"), "mix", 0)
            Assert.Equal("B C(1,0)", SELF:Stru("mix"))
            SELF:Close("mix")
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD Flag65536CreatesVarChar AS VOID
            SELF:Close("vc")
            XmlToCursor(SELF:TypesXml(TRUE), "vc", 65536)
            DbSelectArea("vc")
            Assert.Equal("V", DbFieldInfo(DBS_TYPE, FieldPos("cchar")))
            SELF:Close("vc")
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD EntitiesAndAccentsSurvive AS VOID
            SELF:Close("ent")
            XmlToCursor(SELF:Doc("<t><a>a&amp;b</a><b>ni&#241;o</b></t>"), "ent", 0)
            DbSelectArea("ent")
            DbGoTop()
            Assert.Equal("a&b", AllTrim(ent->a))
            Assert.Equal("ni" + Chr(241) + "o", AllTrim(ent->b))
            SELF:Close("ent")
        END METHOD

        #endregion

        #region errors

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD InvalidXmlThrows AS VOID
            Assert.ThrowsAny<Exception>({ => XmlToCursor("this is not xml", "bad1", 0) })
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD MissingFileThrows AS VOID
            VAR cFile := Path.Combine(Path.GetTempPath(), "X2CMISSING_" + Guid.NewGuid():ToString("N") + ".xml")
            Assert.ThrowsAny<Exception>({ => XmlToCursor(cFile, "bad2", 512) })
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD DocumentWithoutARowLevelThrows AS VOID
            Assert.ThrowsAny<Exception>({ => XmlToCursor("<VFPData><a>1</a><b>2</b></VFPData>", "bad3", 0) })
        END METHOD

        [Fact, Trait("Category", "XmlToCursor")];
        METHOD AppendToAnUnknownAliasThrows AS VOID
            Assert.ThrowsAny<Exception>({ => XmlToCursor(SELF:Doc("<t><a>1</a></t>"), "nosuchalias", 8192) })
        END METHOD

        #endregion

    END CLASS

END NAMESPACE
