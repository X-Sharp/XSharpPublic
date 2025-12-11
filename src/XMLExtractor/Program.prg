// Program.prg
// Created by    : robert
// Creation Date : 12/23/2024 5:00:33 PM
// Created for   :
// WorkStation   : LEDA

USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using System.Data.Common

FUNCTION Start() AS VOID STRICT
    var oFact := Microsoft.Data.Sqlite.SqliteFactory.Instance
    var oConn := oFact.CreateConnection()
    oConn.ConnectionString := "Data Source=:memory:;Pooling=False;"
    var cFile := "c:\temp\Runtime.xsdb"
    var oDiskDb := oFact.CreateConnection()
    oDiskDb.ConnectionString := "Data Source="+cFile+";Pooling=False;"
    oDiskDb:Open()
    oConn:Open()
    oDiskDb:BackupDatabase(oConn, "main")
    oDiskDb:Close()
    var oCmd := oFact.CreateCommand()
    oCmd.Connection := oConn
    oCmd:CommandText := "Select Name, XmlComments from ProjectMembers where ProjectFileName like '%xsharp.core%' and Length(XmlComments) > 0 and Kind = 23"
    var oReader := oCmd:ExecuteReader()
    var outFile := System.IO.StreamWriter{"c:\temp\XSharp.CoreDefines.xml"}
    outFile:WriteLine("﻿<?xml version=""1.0"" encoding=""utf-8"">")
    outFile:WriteLine("<defines>")
    while oReader:Read()
        var cName := oReader:GetString(0)
        var cXml := oReader:GetString(1)
        cXML := cXml:Replace("<doc> ",""):Replace(" </doc>","")
        outFile:WriteLine("<"+cName+">")
        outFile:WriteLine(cXml)
        outFile:WriteLine("</"+cName+">")
        ? cName
    end while
    outFile:WriteLine("</defines>")
    oReader:Close()
    oCmd:CommandText := "Select TypeName, Name, XmlComments from TypeMembers  where Length(XmlComments) > 0 and not XmlComments like '%<exclude>%' and Kind = 19 and IdFile in (select idFile from ProjectFiles where ProjectFileName like '%xsharp.core%')"
    oReader := oCmd:ExecuteReader()
    outFile:WriteLine("<enums>")
    while oReader:Read()
        var cType := oReader:GetString(0)
        var cName := oReader:GetString(1)
        var cXml := oReader:GetString(2)
        cXML := cXml:Replace("<doc> ",""):Replace(" </doc>","")
        outFile:WriteLine("<"+cType+"."+cName+">")
        outFile:WriteLine(cXml)
        outFile:WriteLine("</"+cType+"."+cName+">")
        ? cType+"."+cName
    end while

    outFile:WriteLine("</enums>")

    outFile:Close()
    oReader:Close()
    oConn:Close()
    Console.WriteLine("Done")
    Console.ReadLine()
END FUNCTION




static class Extensions
    STATIC METHOD BackupDatabase(SELF oConn as DbConnection, oDest as DbConnection, name as string) AS VOID
        var conn := (Microsoft.Data.Sqlite.SqliteConnection) oConn
        var dest := (Microsoft.Data.Sqlite.SqliteConnection) oDest
        conn:BackupDatabase(dest, name, name)
end class
