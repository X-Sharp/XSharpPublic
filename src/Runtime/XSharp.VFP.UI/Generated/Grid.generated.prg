
// Class Grid  BaseClass   Grid  Class  Grid
BEGIN NAMESPACE XSharp.VFP.UI
    PARTIAL CLASS Grid

        PROPERTY AllowAddRows AS LOGIC AUTO
        PROPERTY AllowDeleteRows AS LOGIC AUTO
        PROPERTY ChildOrder AS STRING AUTO
        // GridLines, GridLineColor, HeaderHeight, NullDisplay — implemented as real properties in Grid.prg
        PROPERTY IntegralHeight AS LOGIC AUTO
        PROPERTY LinkMaster AS STRING AUTO
        PROPERTY Partition AS INT AUTO
        PROPERTY RelationalExpr AS STRING AUTO

    END CLASS
END NAMESPACE
