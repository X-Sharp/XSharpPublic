// Program.prg
// Created by    : robert
// Creation Date : 11/8/2023 5:05:33 PM
// Created for   :
// WorkStation   : NYX

using System.Diagnostics
USING SYstem
USING SYstem.Collections.Generic
USING SYstem.Linq
USING SYstem.Text
USING XSharp.Parsers
USING XSharp.RDD.Enums
USING XSharp.Data
USING XSharp.Data.Query
USING XSharp.VFP

#include "foxcmd.xh"

// comments:
// insert into <table> opens the table when it is not already open
// select * from <table> opens the table when it is not already open

GLOBAL AllTests := List<TestInfo>{} AS List<TestInfo>
GLOBAL Verbose := FALSE AS LOGIC

CLASS TestInfo
    PUBLIC Name AS STRING
    PUBLIC TestFunc AS Action
    PUBLIC Result AS STRING
    PUBLIC DisableSqlOptimizations AS LOGIC

    CONSTRUCTOR(name AS STRING, testFunc AS Action, disableSqlOptimizations := FALSE AS LOGIC)
        ::Name := name
        ::TestFunc := testFunc
        ::Result := "NotRun"
        ::DisableSqlOptimizations := disableSqlOptimizations
END CLASS

FUNCTION RegisterTest(name AS STRING, testFunc AS Action, disableSqlOptimizations := FALSE AS LOGIC) AS VOID
    AllTests:Add(TestInfo{name, testFunc, disableSqlOptimizations})
    RETURN
FUNCTION SetVerbose(v AS LOGIC) AS VOID
    Verbose := v
FUNCTION RunTests(tests := NULL AS List<TestInfo>) AS TUPLE(INT, INT)
    LOCAL passed := 0 AS INT
    LOCAL failed := 0 AS INT

    IF tests == NULL
        tests := AllTests
    ENDIF

    FOREACH VAR test IN tests
        TestRunner(test)
        IF test:Result == "Passed"
            passed++
        ELSE
            failed++
        ENDIF
    NEXT

    RETURN TUPLE(passed, failed)

FUNCTION TestRunner(test AS TestInfo) AS VOID
    LOCAL switchName := EmbeddedSqlRuntimeOptions.DisableOptimizationsSwitch AS STRING
    LOCAL previousSwitch := FALSE AS LOGIC
    LOCAL switchWasSet := AppContext.TryGetSwitch(switchName, OUT previousSwitch) AS LOGIC
    LOCAL previousFloatDelta := RuntimeState.FloatDelta AS REAL8
    AppContext.SetSwitch(switchName, test:DisableSqlOptimizations)
    // Run the regular optimizer/baseline comparisons with exact numeric
    // equality. Dedicated FLOATDELTA cases verify the conservative fallback.
    RuntimeState.FloatDelta := 0
    TRY
        TRY
            ?
            ? "Starting test:", test:Name
            test:TestFunc()
            test:Result := "Passed"
        CATCH e AS Exception
            test:Result := "Failed: " + e:ToString()
        END TRY
    FINALLY
        AppContext.SetSwitch(switchName, IIF(switchWasSet, previousSwitch, FALSE))
        RuntimeState.FloatDelta := previousFloatDelta
    END

    IF test:Result != "Passed" .OR. Verbose
        ? "Result:", test:Result
    ENDIF

    RETURN

FUNCTION Start(args AS STRING[]) AS VOID STRICT
    // Check for /verbose option
    FOREACH VAR arg IN args
        LOCAL normalizedArg := Upper(arg):Replace(c'-', c'/') AS STRING
        IF normalizedArg == "/VERBOSE" .OR. normalizedArg == "/V"
            SetVerbose(TRUE)
        ENDIF
    NEXT

    RegisterTest("TestCreate", { => TestCreate() })
    RegisterTest("TestSqlParser", { => TestSqlParser() })
    RegisterTest("TestValueComparison", { => TestValueComparison() })
    RegisterTest("TestFieldResolution", { => TestFieldResolution() })
    RegisterTest("TestTableResolution", { => TestTableResolution() })
    RegisterTest("TestNewSqlFeatures", { => TestNewSqlFeatures() })
    RegisterTest("TestUpdateFrom [optimized]", { => TestUpdateFrom() })
    RegisterTest("TestUpdateFrom [baseline]", { => TestUpdateFrom() }, TRUE)
    RegisterTest("TestDeleteJoin [optimized]", { => TestDeleteJoin() })
    RegisterTest("TestDeleteJoin [baseline]", { => TestDeleteJoin() }, TRUE)
    RegisterTest("TestDeleteInSubquery [optimized]", { => TestDeleteInSubquery() })
    RegisterTest("TestDeleteInSubquery [baseline]", { => TestDeleteInSubquery() }, TRUE)
    RegisterTest("TestDropTable [optimized]", { => TestDropTable() })
    RegisterTest("TestDropTable [baseline]", { => TestDropTable() }, TRUE)
    RegisterTest("TestInsertFromSelect [optimized]", { => TestInsertFromSelect() })
    RegisterTest("TestInsertFromSelect [baseline]", { => TestInsertFromSelect() }, TRUE)
    RegisterTest("TestSelectFunctionality [optimized]", { => TestSelectFunctionality() })
    RegisterTest("TestSelectFunctionality [baseline]", { => TestSelectFunctionality() }, TRUE)
    RegisterTest("TestSelectIntoArray [optimized]", { => TestSelectIntoArray() })
    RegisterTest("TestSelectIntoArray [baseline]", { => TestSelectIntoArray() }, TRUE)
    RegisterTest("TestSelectIntoTable [optimized]", { => TestSelectIntoTable() })
    RegisterTest("TestSelectIntoTable [baseline]", { => TestSelectIntoTable() }, TRUE)
    RegisterTest("TestSelectCursorOptions [optimized]", { => TestSelectCursorOptions() })
    RegisterTest("TestSelectCursorOptions [baseline]", { => TestSelectCursorOptions() }, TRUE)
    RegisterTest("TestNestedSubquery [optimized]", { => TestNestedSubqueryFunctionality() })
    RegisterTest("TestNestedSubquery [baseline]", { => TestNestedSubqueryFunctionality() }, TRUE)
    RegisterTest("TestCrossJoinFunctionality [optimized]", { => TestCrossJoinFunctionality() })
    RegisterTest("TestCrossJoinFunctionality [baseline]", { => TestCrossJoinFunctionality() }, TRUE)
    RegisterTest("TestUnionFunctionality [optimized]", { => TestUnionFunctionality() })
    RegisterTest("TestUnionFunctionality [baseline]", { => TestUnionFunctionality() }, TRUE)
    RegisterTest("TestDeletedCandidateRows [optimized]", { => TestDeletedCandidateRows() })
    RegisterTest("TestDeletedCandidateRows [baseline]", { => TestDeletedCandidateRows() }, TRUE)
    RegisterTest("TestHashJoin [optimized]", { => TestHashJoin() })
    RegisterTest("TestHashJoin [baseline]", { => TestHashJoin() }, TRUE)
    RegisterTest("TestLeftOuterJoin [optimized]", { => TestLeftOuterJoin() })
    RegisterTest("TestLeftOuterJoin [baseline]", { => TestLeftOuterJoin() }, TRUE)
    RegisterTest("TestQueryOptimizer", { => TestQueryOptimizer() })
    RegisterTest("TestOptimizerBitmap", { => TestOptimizerBitmap() })
    RegisterTest("TestMembershipBitmap [optimized]", { => TestMembershipBitmap() })
    RegisterTest("TestMembershipBitmap [baseline]", { => TestMembershipBitmap() }, TRUE)
    RegisterTest("TestMultiTableOptimizer [optimized]", { => TestMultiTableOptimizer() })
    RegisterTest("TestMultiTableOptimizer [baseline]", { => TestMultiTableOptimizer() }, TRUE)
    RegisterTest("TestSparseOptimizationBenchmark", { => TestSparseOptimizationBenchmark() })
    RegisterTest("TestCorrelatedInOptimizationBenchmark", { => TestCorrelatedInOptimizationBenchmark() })
    RegisterTest("TestMembershipBitmapBenchmark", { => TestMembershipBitmapBenchmark() })

    LOCAL results := RunTests()

    ? ""
    ? "=== Test Summary ==="
    ? "Passed:", results:Item1
    ? "Failed:", results:Item2

    Environment.ExitCode := IIF(results.Item2 > 0, 1, 0)

    IF results.Item2 > 0
        ? ""
        ? "=== Failed Tests ==="
        FOREACH VAR test IN AllTests
            IF test:Result != "Passed"
                ? test:Name + ": " + test:Result
            ENDIF
        NEXT
    ENDIF

    RETURN

FUNCTION TestCreate() AS VOID
    TRY
    CREATE TABLE Salesman ;
        (SalesID c(6) PRIMARY KEY, ;
        SaleName Character(20) )

    IF Used()
        INSERT INTO Salesman VALUES ("1", "Robert")
        INSERT INTO Salesman VALUES ("2", "Fabrice")
        INSERT INTO Salesman VALUES ("3", "Chris")
        INSERT INTO Salesman VALUES ("4", "Nikos")

        // Verify data was inserted
        GO TOP
        IF Alias() != "SALESMAN"
            THROW Exception{"Failed: Expected alias SALESMAN"}
        ENDIF
        VerifyTable(4, "Failed: Expected 4 records in Salesman table", TRUE)
    ELSE
        THROW Exception{"Failed: Table not open"}
    ENDIF

    CREATE CURSOR employee ;
        (EmpID N(5), Name Character(20), DOB D, Address c( 30 ) , City c(30), ;
        PostalCode c(10), OfficeNo c(8) NULL, Salary Numeric(10 , 2 ),  Specialty Memo)

    IF Used()
        INSERT INTO Employee(EmpId, Name) VALUES (1, "Nikos")
        m.Address = "Zuukerweg"
        m.EmpId = 10
        m.Name  = "Robert"
        INSERT INTO employee FROM Memvar

        LOCAL obj as object
        OBJ = CREATEOBJECT("Empty")
        = AddProperty(obj,"EmpId", 42)
        = AddProperty(obj,"Name", "Fabrice")
        INSERT INTO employee FROM NAME obj

        Dimension values[3]
        values[1] = 100
        values[2] = "Chris"
        values[3] = ToDay()
        INSERT INTO employee FROM ARRAY values

        // Verify data
        GO TOP
        IF Alias() != "EMPLOYEE"
            THROW Exception{"Failed: Expected alias EMPLOYEE"}
        ENDIF
        VerifyTable(4, "Failed: Expected 4 records in Employee table", TRUE)
    ELSE
        THROW Exception{"Failed: Table not open"}
    ENDIF

    FINALLY
        CleanupSalesmanTable()
    END TRY

    RETURN

FUNCTION CleanupSalesmanTable() AS VOID
    IF Used("SALESMAN")
        DbCloseArea("SALESMAN")
    ENDIF

    LOCAL tablePath := "Salesman.DBF" AS STRING
    LOCAL defaultPath := SetDefault() AS STRING
    IF !String.IsNullOrEmpty(defaultPath)
        tablePath := System.IO.Path.Combine(defaultPath, tablePath)
    ENDIF
    tablePath := System.IO.Path.GetFullPath(tablePath)

    FOREACH VAR extension IN <STRING>{".DBF", ".FPT", ".CDX"}
        VAR filePath := System.IO.Path.ChangeExtension(tablePath, extension)
        IF System.IO.File.Exists(filePath)
            System.IO.File.Delete(filePath)
        ENDIF
    NEXT
    RETURN

FUNCTION TestDelete() AS VOID
/*
        DELETE FROM employee where _FIELD->EmpID = 1

        //DELETE FROM Database!employee where EmpID = 1

        DELETE  MyProducts FROM MSRPList; 
            WHERE MSRPList.ProdID = MyProducts.ProdID;
            AND MSRPList.discontinued = .t.
*/

    //         DELETE  DB!MyProducts FROM DB!MSRPList ;
    //            WHERE MSRPList.ProdID = MyProducts.ProdID;
    //             AND MSRPList.discontinued = .t.

    /*
        UPDATE MyProducts SET MSRP=MyUpdates.MSRP FROM MyUpdates WHERE MyProducts.ProdID=MyUpdates.ProdID
        UPDATE products ;
            SET unitprice = ;
            (SELECT (msrp*.90) ;
            FROM mfg_msrp ;
            WHERE mfg_msrp.productID = products.productID ;
            AND mfg_msrp.discontinued = .f.)

        UPDATE products ;
            SET unitprice = mfg_msrp.msrp*.90 ;
            FROM mfg_msrp ;
            WHERE mfg_msrp.productID = products.productID;
            AND mfg_msrp.discontinued = .f.
*/

FUNCTION TestCreateTables()
        CREATE TABLE Customers ;
            (CustId i PRIMARY KEY, ;
            CustName c(20) )
        INSERT INTO Customers(CustId,CustName) VALUES (1, "Microsoft")
        INSERT INTO Customers(CustId,CustName) VALUES (2, "Google")

        IF Verbose
            ? ALIAS()
        ENDIF

        CREATE TABLE Orders ;
            (OrderId i PRIMARY KEY, ;
            CustId i REFERENCES customer TAG CustId, ;
            CustName c(10), ;
            OrderAmt y(4), ;
            OrderQty i ;
            DEFAULT 10 ;
            CHECK (OrderQty > 9) ;
            ERROR "Order Quantity must be at least 10", ;
            DiscPercent n(6,2) NULL ;
            DEFAULT .NULL., ;
            CHECK (OrderAmt > 0) ERROR "Order Amount must be > 0" )
        local oTest := XSharp.VFP.Empty{}
        AddProperty(oTest, "LastName", "Hulst")
        AddProperty(oTest, "FirstName", "Robert")
        INSERT INTO Orders(CustId,OrderAmt, OrderQty,OrderCode) VALUES (1, 100, 10,"19010101")
        INSERT INTO Orders(CustId,OrderAmt, OrderQty,OrderCode) VALUES (1, 200, 20,"19010202")
        INSERT INTO Orders(CustId,OrderAmt, OrderQty,OrderCode) VALUES (2, 200, 20,"19010202")

        IF Verbose
            ? ALIAS()
        ENDIF

        UPDATE Orders ;
            SET OrderAmt = OrderAmt*1.1 ;
            FROM Customers ;
            WHERE Orders->CustId = Customers->CustId AND Customers->CustName = "Microsoft"

        IF Verbose
            Browse()
        ENDIF

        ALTER TABLE Orders Add OrderDate Date NULL

        IF Verbose
            Browse()
        ENDIF

        ALTER TABLE Orders Add COLUMN DeliveryDate DateTime

        IF Verbose
            Browse()
        ENDIF

        ALTER TABLE Orders DROP COLUMN OrderAmt

        IF Verbose
            Browse()
        ENDIF

        ALTER TABLE Orders Alter COLUMN OrderCode C(5)

        IF Verbose
            Browse()
        ENDIF

//         CREATE SQL VIEW MyView AS SELECT * FROM Northwind!Customers;
//             WHERE Country="Mexico
    RETURN
