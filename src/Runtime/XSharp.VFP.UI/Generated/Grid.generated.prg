
// Class Grid  BaseClass   Grid  Class  Grid
BEGIN NAMESPACE XSharp.VFP.UI
    PARTIAL CLASS Grid

        PROPERTY ChildOrder AS STRING AUTO
        // GridLines, GridLineColor, HeaderHeight, NullDisplay — implemented as real properties in Grid.prg
        PROPERTY HScrollSmallChange AS INT AUTO
        PROPERTY InputDateFormat AS INT AUTO
        PROPERTY IntegralHeight AS LOGIC AUTO
        PROPERTY LinkMaster AS STRING AUTO
        PROPERTY Partition AS INT AUTO
        PROPERTY RelationalExpr AS STRING AUTO
        PROPERTY ShowTips AS LOGIC AUTO

    END CLASS
END NAMESPACE
