// 896. Various FoxPro commands
// https://github.com/X-Sharp/XSharpPublic/issues/1409

FUNCTION Start( ) AS VOID
    test1()
    test2()
RETURN

PROCEDURE Test1()
	CREATE TABLE Test FREE (Object C(10), Color C(16), SqFt n(6,2))
	SCATTER MEMVAR BLANK
	m.Object="Box"
	m.Color="Red"
	m.SqFt=12.5
	APPEND BLANK
	GATHER MEMVAR
	? _FIELD->OBJECT
	? _FIELD->Color
	? _FIELD->SqFt

PROCEDURE Test2()
	CREATE TABLE Test FREE ;
	   (OBJECT C(10), Color C(16), SqFt n(6,2))

	SCATTER NAME oTest BLANK
	oTest.Object="Box"
	oTest.Color="Red"
	oTest.SqFt=12.5
	APPEND BLANK
	GATHER NAME oTest
	RELEASE oTest
	? _FIELD->OBJECT
	? _FIELD->Color
	? _FIELD->SqFt

PROCEDURE Test3()
	USE Customer
	SCATTER NAME Customer
	? Customer.Company  && Returns the table value
	? m.Customer.Company  && Returns the OBJECT property value

PROCEDURE Test4()
	SELECT Customer
	SCATTER NAME oCustomer
	SELECT CreditHistory
	SCATTER FIELDS ReportDate, Rating NAME oCustomer ADDITIVE
	SELECT MySessionTable
	SCATTER FIELDS CookieText, SessionId NAME oCustomer ADDITIVE
