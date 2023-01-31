//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root f` license information.
//

USING System.Data.SQLite
USING System.IO
USING System.Data
USING System.Linq
USING System.Data.Common
USING System.Collections.Generic
USING System.ComponentModel
USING File := System.IO.File

BEGIN NAMESPACE XSharpModel
STATIC CLASS XDatabase
    STATIC PRIVATE oConn   AS SQLiteConnection     // In memory database !
    STATIC PRIVATE lastWritten := DateTime.MinValue AS DateTime
    STATIC PRIVATE currentFile AS STRING
    STATIC PROPERTY FileName as STRING GET currentFile
    STATIC PROPERTY DeleteOnClose as LOGIC AUTO
    PRIVATE CONST CurrentDbVersion := 1.7 AS System.Double

    STATIC METHOD CreateOrOpenDatabase(cFileName AS STRING) AS VOID
        LOCAL lValid := FALSE AS LOGIC
        LOCAL oDiskDb AS SQLiteConnection
        currentFile := cFileName
        IF File.Exists(cFileName)
            oDiskDb := OpenFile(cFileName)
            IF ! ValidateSchema(oDiskDb)
                oDiskDb:Close()
                oDiskDb:Dispose()
                File.Delete(cFileName)
            ELSE
                lValid := TRUE
            ENDIF
        ENDIF
        oConn := SQLiteConnection{"Data source=:Memory:"}
        oConn:Open()
        SetPragmas(oConn)
        IF ! lValid
            CreateSchema(oConn)
            SaveToDisk(oConn,cFileName )
        ELSE
            RestoreFromDisk(oDiskDb, oConn)
            DeleteOrphanFiles()
            oDiskDb:Close()
        ENDIF
        RETURN

    STATIC METHOD SaveDatabase(cFile AS STRING) AS LOGIC
        IF IsDbOpen
            SaveToDisk(oConn, cFile)
            RETURN TRUE
        ENDIF
        RETURN FALSE

    STATIC METHOD CloseDatabase(cFile AS STRING) AS LOGIC
        IF IsDbOpen
            SaveToDisk(oConn, cFile)
            oConn:Close()
            if DeleteOnClose
                File.Delete(cFile)
                DeleteOnClose := FALSE
            endif
            RETURN TRUE
        ENDIF
        oConn := NULL
        RETURN FALSE

    STATIC METHOD SetPragmas(oConn AS SQLiteConnection) AS VOID
        IF ! IsDbOpen
            RETURN
        ENDIF
        BEGIN LOCK oConn
            USING VAR oCmd := SQLiteCommand{"PRAGMA foreign_keys = ON", oConn}
            oCmd:ExecuteNonQuery()
            oCmd:CommandText := "VACUUM"
            oCmd:ExecuteNonQuery()
        END LOCK
        RETURN

    STATIC METHOD OpenFile(cFile AS STRING) AS SQLiteConnection
        VAR db := SQLiteConnection{"Data Source="+cFile+";Version=3;"}
        db:Open()
        SetPragmas(db)
        RETURN db

    STATIC METHOD SaveToDisk(oConn AS SQLiteConnection, cFile AS STRING) AS VOID
        IF ! IsDbOpen
            RETURN
        ENDIF
        IF File.Exists(cFile)
            File.SetAttributes(cFile, FileAttributes.Normal)
            File.Delete(cFile)
        ENDIF
        VAR diskdb := OpenFile(cFile)
        oConn:BackupDatabase(diskdb, "main", "main", -1, NULL, 0)
        USING VAR oCmd := SQLiteCommand{"VACUUM", diskdb}
        oCmd:ExecuteNonQuery()
        diskdb:Close()
        lastWritten := DateTime.Now
        RETURN

    STATIC METHOD RestoreFromDisk(oDiskDb AS SQLiteConnection, oConn AS SQLiteConnection) AS VOID
        IF ! IsDbOpen
            RETURN
        ENDIF
        oDiskDb:BackupDatabase(oConn, "main", "main", -1, NULL, 0)
        lastWritten := DateTime.Now
        RETURN

    STATIC METHOD CommitWhenNeeded() AS VOID
        VAR ts := DateTime.Now - lastWritten
        // Save to disk every 5 minutes
        Log(i"Time since last backup {ts}")
        IF ts:Minutes >= 5 .OR. ts:Hours > 0
            LOCAL oBW AS BackgroundWorker
            oBW := BackgroundWorker{}
            oBW:DoWork += BackupInBackground
            oBW:RunWorkerAsync()

        ENDIF

    STATIC METHOD BackupInBackground(sender AS OBJECT , args AS DoWorkEventArgs ) AS VOID
        IF ! IsDbOpen
            RETURN
        ENDIF
        BEGIN LOCK oConn
            TRY
                Log("Starting backup to "+currentFile)
                SaveToDisk(oConn, currentFile )
            CATCH e AS Exception
                XSettings.LogException(e, __FUNCTION__)
            FINALLY
                Log("Completed backup to "+currentFile)
            END TRY
        END LOCK
        RETURN

    STATIC METHOD CreateSchema(connection AS SQLiteConnection) AS VOID
        BEGIN LOCK connection
            VAR cmd := SQLiteCommand{"SELECT 1",connection}
            Log("Creating new database schema")
#region Drop Existing Tables
            cmd:CommandText := "DROP TABLE IF EXISTS Projects ;"
            cmd:CommandText += "DROP TABLE IF EXISTS FilesPerProject ;"
            cmd:CommandText += "DROP TABLE IF EXISTS Files ;"
            cmd:CommandText += "DROP TABLE IF EXISTS IncludeFiles ;"
            cmd:CommandText += "DROP TABLE IF EXISTS IncludeFilesPerFile ;"
            cmd:CommandText += "DROP TABLE IF EXISTS Types ;"
            cmd:CommandText += "DROP TABLE IF EXISTS Members ;"
            cmd:CommandText += "DROP TABLE IF EXISTS Assemblies ;"
            cmd:CommandText += "DROP TABLE IF EXISTS ReferencedGlobals ;"
            cmd:CommandText += "DROP TABLE IF EXISTS ReferencedTypes ;"
            cmd:CommandText += "DROP TABLE IF EXISTS CommentTasks ;"
            cmd:CommandText += "DROP TABLE IF EXISTS ExtensionMethods ;"
            cmd:ExecuteNonQuery()
#endregion
#region Table Projects
            VAR stmt	:= "Create Table Projects ("
            stmt     	+= " Id integer NOT NULL PRIMARY KEY, ProjectFileName text NOT NULL COLLATE NOCASE "
            stmt		+= " ) ;"
            stmt	    += "CREATE UNIQUE INDEX Projects_Pk     ON Projects (Id) ;"
            stmt	    += "CREATE UNIQUE INDEX Projects_Name   ON Projects (ProjectFileName); "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()
#endregion
#region Table Files
            stmt  		:= "CREATE TABLE Files ("
            stmt     	+= " Id integer NOT NULL PRIMARY KEY, FileName text NOT NULL COLLATE NOCASE,  "
            stmt     	+= " FileType integer NOT NULL, LastChanged DateTime NOT NULL, Size integer, Usings text, StaticUsings text"
            stmt		+= " ) ;"
            stmt	    += "CREATE UNIQUE INDEX Files_Pk    ON Files (Id) ;"
            stmt	    += "CREATE UNIQUE INDEX Files_Name  ON Files (FileName); "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()
#endregion
#region Table IncludeFiles
            stmt  		:= "CREATE TABLE IncludeFiles ("
            stmt     += " Id integer NOT NULL PRIMARY KEY, FileName text NOT NULL COLLATE NOCASE "
            stmt		 += " ) ;"
            stmt	    += "CREATE UNIQUE INDEX IncludeFiles_Pk    ON IncludeFiles (Id) ;"
            stmt	    += "CREATE UNIQUE INDEX IncludeFiles_Name  ON IncludeFiles (FileName); "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()
#endregion

#region Table IncludeFilesPerFile

            stmt    := "CREATE TABLE IncludeFilesPerFile ("
            stmt	+= " idFile integer NOT NULL, IdInclude integer NOT NULL, "
            stmt    += " PRIMARY KEY (idFile, IdInclude), "
            stmt	+= " FOREIGN KEY (idFile) 	 REFERENCES Files (Id)    ON DELETE CASCADE ON UPDATE CASCADE, "
            stmt    += " FOREIGN KEY (IdInclude) REFERENCES IncludeFiles (Id) ON DELETE CASCADE ON UPDATE CASCADE "
            stmt	+= " ) ;"
            stmt	+= "CREATE UNIQUE INDEX IncludeFilesPerFile_Pk  ON IncludeFilesPerFile (IdInclude, idFile); "
            stmt	+= "CREATE INDEX IncludeFilesPerFile_File       ON IncludeFilesPerFile (idFile) ;"
            stmt	+= "CREATE INDEX IncludeFilesPerFile_Include    ON IncludeFilesPerFile (IdInclude); "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()
#endregion

#region Table FilesPerProject

            stmt    := "CREATE TABLE FilesPerProject ("
            stmt	+= " idFile integer NOT NULL, idProject integer NOT NULL, "
            stmt    += " PRIMARY KEY (idFile, idProject), "
            stmt	+= " FOREIGN KEY (idFile) 	 REFERENCES Files (Id)    ON DELETE CASCADE ON UPDATE CASCADE, "
            stmt    += " FOREIGN KEY (idProject) REFERENCES Projects (Id) ON DELETE CASCADE ON UPDATE CASCADE "
            stmt	+= " ) ;"
            stmt	+= "CREATE UNIQUE INDEX FilesPerProject_Pk  ON FilesPerProject (idFile, idProject); "
            stmt	+= "CREATE INDEX FilesPerProject_File       ON FilesPerProject (idFile) ;"
            stmt	+= "CREATE INDEX FilesPerProject_Project    ON FilesPerProject (idProject); "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()

#endregion
#region Table Types

            stmt  	:= "CREATE TABLE Types ("
            stmt	+= " Id integer NOT NULL PRIMARY KEY, idFile integer NOT NULL, Name text NOT NULL COLLATE NOCASE, Namespace text NOT NULL COLLATE NOCASE, "
            stmt    += " Kind integer NOT NULL, BaseTypeName text COLLATE NOCASE, Attributes integer NOT NULL, "
            stmt    += " StartLine integer , StartColumn integer, EndLine integer , EndColumn integer, Start integer , Stop integer,  "
            stmt	+= " Sourcecode text , XmlComments text, ClassType integer NOT NULL, "
            stmt    += " FOREIGN KEY (idFile) REFERENCES Files (Id) ON DELETE CASCADE ON UPDATE CASCADE"
            stmt	+= ") ;"
            stmt	+= "CREATE UNIQUE INDEX Types_Pk    ON Types (Id); "
            stmt	+= "CREATE INDEX Types_Name         ON Types (Name); "
            stmt	+= "CREATE INDEX Types_BaseTypeName ON Types (BaseTypeName); "
            stmt	+= "CREATE INDEX Types_Kind         ON Types (Kind); "
            stmt	+= "CREATE INDEX Types_Namespace    ON Types (Namespace); "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()

#endregion
#region Table Members

            stmt    := "CREATE TABLE Members ("
            stmt	+= " Id integer NOT NULL PRIMARY KEY, IdType integer NOT NULL , IdFile integer NOT NULL, "
            stmt	+= " Name text COLLATE NOCASE, Kind integer, Attributes integer NOT NULL, StartLine integer , StartColumn integer ,  "
            stmt	+= " EndLine integer , EndColumn integer , Start integer , Stop integer , Sourcecode text , XmlComments text,  ReturnType text, "
            stmt	+= " FOREIGN KEY (idType) REFERENCES Types (Id) ON DELETE CASCADE ON UPDATE CASCADE, "
            stmt    += " FOREIGN KEY (idFile) REFERENCES Files (Id) ON DELETE CASCADE ON UPDATE CASCADE"
            stmt	+= ");"
            stmt	+= "CREATE UNIQUE INDEX Members_Pk  ON Members (Id); "
            stmt	+= "CREATE INDEX Members_Name       ON Members (Name); "
            stmt	+= "CREATE INDEX Members_Type       ON Members (idType); "
            stmt	+= "CREATE INDEX Members_File       ON Members (idFile); "
            stmt	+= "CREATE INDEX Members_Kind       ON Members (Kind); "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()

#endregion


#region Table Assemblies
            stmt    := "CREATE TABLE Assemblies ("
            stmt    += " Id integer NOT NULL PRIMARY KEY, Name text NOT NULL COLLATE NOCASE, AssemblyFileName text NOT NULL COLLATE NOCASE, "
            stmt    += " LastChanged DateTime NOT NULL, Size integer "
            stmt    += ") ;"
            stmt    += "CREATE UNIQUE INDEX Assemblies_Pk   ON Assemblies (Id);"
            stmt    += "CREATE INDEX Assemblies_Name        ON Assemblies (Name);"
            stmt    += "CREATE INDEX Assemblies_FileName    ON Assemblies (AssemblyFileName); "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()

#endregion
#region Table ReferencedTypes
            stmt    := "CREATE TABLE ReferencedTypes ("
            stmt    += " Id integer NOT NULL PRIMARY KEY, idAssembly integer NOT NULL, Name text NOT NULL COLLATE NOCASE, Namespace text NOT NULL COLLATE NOCASE, "
            stmt    += " FullName text NOT NULL COLLATE NOCASE, Kind integer NOT NULL, BaseTypeName text COLLATE NOCASE, Attributes integer NOT NULL, "
            stmt    += " FOREIGN KEY (idAssembly) REFERENCES Assemblies (Id) ON DELETE CASCADE ON UPDATE CASCADE"
            stmt    += ") ;"
            stmt    += "CREATE UNIQUE INDEX ReferencedTypes_Pk      ON ReferencedTypes (Id); "
            stmt    += "CREATE INDEX ReferencedTypes_Name           ON ReferencedTypes (Name); "
            stmt    += "CREATE INDEX ReferencedTypes_BaseTypeName   ON ReferencedTypes (BaseTypeName); "
            stmt    += "CREATE INDEX ReferencedTypes_FullName       ON ReferencedTypes (FullName); "
            stmt    += "CREATE INDEX ReferencedTypes_Kind           ON ReferencedTypes (Kind); "
            stmt    += "CREATE INDEX ReferencedTypes_Namespace      ON ReferencedTypes (Namespace); "

            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()
#endregion

#region Table ReferencedGlobals
            stmt    := "CREATE TABLE ReferencedGlobals ("
            stmt    += " Id integer NOT NULL PRIMARY KEY, idAssembly integer NOT NULL, Name text NOT NULL COLLATE NOCASE, "
            stmt    += " FullName text NOT NULL COLLATE NOCASE, Kind integer NOT NULL, Attributes integer NOT NULL, Sourcecode text, ReturnType text, "
            stmt    += " FOREIGN KEY (idAssembly) REFERENCES Assemblies (Id) ON DELETE CASCADE ON UPDATE CASCADE"
            stmt    += ") ;"
            stmt    += "CREATE UNIQUE INDEX ReferencedGlobals_Pk      ON ReferencedGlobals (Id); "
            stmt    += "CREATE INDEX ReferencedGlobals_Name           ON ReferencedGlobals (Name); "
            stmt    += "CREATE INDEX ReferencedGlobals_Kind           ON ReferencedGlobals (Kind); "

            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()
#endregion


#region Table CommentTasks
            stmt    :=  "CREATE TABLE CommentTasks ("
            stmt    +=  " Id integer NOT NULL PRIMARY KEY, idFile integer NOT NULL, Line integer, Column integer, Priority integer,  "
            stmt    +=  " Comment text NOT NULL, "
            stmt    +=  " FOREIGN KEY (idFile) REFERENCES Files (Id) ON DELETE CASCADE ON UPDATE CASCADE"
            stmt    += ") ;"
            stmt    += "CREATE UNIQUE INDEX CommentTasks_Pk ON CommentTasks (Id); "
            stmt    += "CREATE INDEX CommentTasks_File      ON CommentTasks (idFile); "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()
#endregion

#region Table ExtensionMethods
            stmt    :=  "CREATE TABLE ExtensionMethods ("
            stmt    +=  " Id integer NOT NULL PRIMARY KEY, idMember integer NOT NULL, FullName text NOT NULL COLLATE NOCASE, "
            stmt    +=  " FOREIGN KEY (idMember) REFERENCES Members (Id) ON DELETE CASCADE ON UPDATE CASCADE"
            stmt    += ") ;"
            stmt    += "CREATE UNIQUE INDEX ExtensionMethods_Pk ON ExtensionMethods (Id); "
            stmt    += "CREATE INDEX ExtensionMethods_Type      ON ExtensionMethods (FullName); "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()
#endregion
            cmd:Parameters:Clear()

#region views
            stmt := " CREATE VIEW ProjectFiles AS SELECT fp.IdFile, f.FileName, f.FileType, f.LastChanged, f.Size, fp.IdProject, p.ProjectFileName " + ;
                " FROM Files f JOIN FilesPerProject fp ON f.Id = fp.IdFile JOIN Projects p ON fp.IdProject = P.Id"
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()


            stmt := " CREATE VIEW FileIncludeFiles AS SELECT distinct iff.IdFile, f.FileName, i.FileName as Include " + ;
                " from Files f " + ;
                " JOIN IncludeFilesPerFile iff on f.id = iff.IdFile " + ;
                " JOIN IncludeFiles i ON iff.IdInclude = i.Id "

            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()

            stmt := " CREATE VIEW ProjectIncludeFiles AS SELECT distinct fp.IdProject, iff.IdInclude, i.FileName " + ;
                " FROM FilesPerProject fp " + ;
                " JOIN IncludeFilesPerFile iff ON fp.IdFile = iff.IdFile " + ;
                " JOIN IncludeFiles i ON iff.IdInclude = i.Id "

            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()

            stmt := "CREATE VIEW TypeMembers AS SELECT m.*, t.Name AS TypeName, t.Namespace, t.BaseTypeName, t.ClassType " + ;
                "FROM members m JOIN Types t ON m.IdType = t.Id"
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()

            stmt := "CREATE VIEW ProjectTypes AS SELECT t.*, p.IdProject, p.FileName, p.ProjectFileName " +;
                " FROM Types t  JOIN ProjectFiles p ON t.IdFile = p.IdFile "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()

            stmt := " CREATE VIEW ProjectNamespaces AS Select distinct p.idProject, t.Namespace FROM Types t  Inner JOIN Files f ON t.idfile = f.id inner JOIN FilesPerProject p ON p.idFile = f.Id WHERE t.namespace != '' "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()

            stmt := "CREATE VIEW ProjectMembers AS SELECT m.*, p.IdProject, p.FileName, p.ProjectFileName " +;
                " FROM TypeMembers m  JOIN ProjectFiles p ON m.IdFile = p.IdFile "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()

            stmt := "CREATE VIEW AssemblyTypes AS SELECT t.*, t.IdAssembly, a.AssemblyFileName " +;
                " FROM ReferencedTypes t  JOIN Assemblies a ON t.IdAssembly = a.Id "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()

            stmt := "CREATE VIEW AssemblyGlobals AS SELECT g.*, g.IdAssembly, a.AssemblyFileName " +;
                " FROM ReferencedGlobals g  JOIN Assemblies a ON g.IdAssembly = a.Id "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()

            stmt := "CREATE VIEW AssemblyNamespaces AS select distinct T.IdAssembly, T.AssemblyFileName, T.Namespace FROM AssemblyTypes T "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()

            stmt := " CREATE VIEW ProjectCommentTasks AS SELECT c.*, pf.IdProject, pf.ProjectFileName, pf.FileName FROM CommentTasks c" + ;
                " JOIN ProjectFiles pf ON c.IdFile = pf.IdFile JOIN Projects p ON pf.IdProject = p.Id"
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()

            stmt := " CREATE VIEW ProjectExtensionMethods as SELECT em.FullName, m.*  FROM ExtensionMethods em" + ;
                " JOIN ProjectMembers m ON em.IdMember = m.Id"
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()
#endregion

#region Table DB_Version
            stmt  	:= 	"CREATE TABLE Db_Version ("
            stmt	   += 	" Id integer NOT NULL PRIMARY KEY, Version REAL"
            stmt	   +=  ")"
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()

            stmt	:=  "INSERT INTO Db_version(Version) values ($version)"
            cmd:CommandText := stmt
            cmd:Parameters:Clear()
            cmd:Parameters:AddWithValue("$version",CurrentDbVersion)
            cmd:ExecuteNonQuery()
#endregion

        END LOCK
        RETURN

    STATIC METHOD ValidateSchema( connection AS SQLiteConnection) AS LOGIC
        LOCAL lOk AS LOGIC
        lOk := TRUE
        BEGIN LOCK connection
            DO WHILE lOk
                USING VAR cmd  := SQLiteCommand{"SELECT 1", connection}
                VAR stmt := "SELECT count(name) from sqlite_master WHERE type='table' AND name=$table"
                cmd:CommandText := stmt
                VAR tables := <STRING> {"Projects","FilesPerProject","Files", "Types", "Members", "Db_Version","Assemblies","ReferencedTypes","CommentTasks", "ReferencedGlobals"}
                FOREACH VAR table IN tables
                    cmd:Parameters:Clear()
                    cmd:Parameters:AddWithValue("$table",table)
                    VAR num := (INT64) cmd:ExecuteScalar()
                    IF num != 1
                        lOk := FALSE
                        EXIT
                    ENDIF
                NEXT
                IF lOk
                    cmd:CommandText := "SELECT Max(Version) from Db_version"
                    VAR vers := (System.Double) 0.0
                    TRY
                        vers := (System.Double) cmd:ExecuteScalar()
                    CATCH  as Exception
                        vers := (System.Double) 0.0
                    END TRY
                    IF vers != CurrentDbVersion
                        lOk := FALSE
                    ENDIF
                ENDIF
                EXIT
            ENDDO
        END LOCK
        Log(i"Validate database schema: {lOk}")
        RETURN lOk

    STATIC METHOD DeleteOrphanFiles() AS List<STRING>
        VAR result := List<STRING>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                var project := XSolution.OrphanedFilesProject
                Read(project)
                USING VAR cmd := SQLiteCommand{"Delete from FilesPerProject where IdProject = "+project:Id:ToString(), oConn}
                cmd:ExecuteScalar()
                cmd:CommandText := "Delete from Files where Id not in (select IdFile from FilesPerProject)"
                cmd:ExecuteScalar()

                cmd:CommandText := "Delete from IncludeFiles where Id not in (select IdInclude from IncludeFilesPerFile)"
                cmd:ExecuteScalar()

            END LOCK
        ENDIF
        Log(i" Deleted OrphanFiles")
        RETURN result


    STATIC METHOD GetProjectFileNames() AS List<STRING>
        VAR result := List<STRING>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                USING VAR cmd := SQLiteCommand{"SELECT ProjectFileName from Projects", oConn}
                USING VAR rdr := cmd:ExecuteReader()
                DO WHILE rdr:Read()
                    VAR name := rdr:GetString(0)
                    result:Add(name)
                ENDDO
            END LOCK
        ENDIF
        Log(i"GetProjectNames returned {result.Count} names")
        RETURN result

#region CRUD projects
    STATIC METHOD Read(oProject AS XProject) AS VOID
        IF ! IsDbOpen .OR. String.IsNullOrEmpty(oProject:FileName)
            RETURN
        ENDIF
        VAR file    := oProject:FileName
        VAR lUpdated := FALSE
        Log(i"Read Project info for project {file}")
        BEGIN LOCK oConn
            USING VAR cmd := SQLiteCommand{"", oConn}
            cmd:CommandText := "SELECT Id, ProjectFileName from Projects WHERE ProjectFileName = $file"
            cmd:Parameters:AddWithValue("$file",file)
            VAR lOk := FALSE
            BEGIN USING VAR rdr := cmd:ExecuteReader()
                IF rdr:Read()
                    oProject:Id := rdr:GetInt64(0)
                    lOk := TRUE
                ENDIF
            END USING
            IF ! lOk
                cmd:CommandText := "INSERT INTO Projects( ProjectFileName ) values ($file); SELECT last_insert_rowid() "
                VAR id := (INT64) cmd:ExecuteScalar()
                oProject:Id := id
                lUpdated := TRUE
            ENDIF
        END LOCK
        IF lUpdated
            CommitWhenNeeded()
        ENDIF
        RETURN

    STATIC METHOD GetFileNames(oProject AS XProject) AS List<STRING>
        VAR result := List<STRING>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                USING VAR cmd := SQLiteCommand{"SELECT FileName from ProjectFiles where idProject = "+oProject:Id:ToString(), oConn}
                USING VAR rdr := cmd:ExecuteReader()
                DO WHILE rdr:Read()
                    VAR name := rdr:GetString(0)
                    result:Add(name)
                ENDDO
            END LOCK
        ENDIF
        Log(i"GetFileNames returned {result.Count} names")
        RETURN result


    STATIC METHOD GetProjectIncludeFiles(oProject AS XProject) AS List<STRING>
        VAR result := List<STRING>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                USING VAR cmd := SQLiteCommand{"SELECT FileName from ProjectIncludeFiles where idProject = "+oProject:Id:ToString(), oConn}
                USING VAR rdr := cmd:ExecuteReader()
                DO WHILE rdr:Read()
                    VAR name := rdr:GetString(0)
                    result:Add(name)
                ENDDO
            END LOCK
        ENDIF
        Log(i"GetFileNames returned {result.Count} names")
        RETURN result

    STATIC METHOD DeleteProject(cFileName AS STRING) AS VOID
        IF ! IsDbOpen .OR. String.IsNullOrEmpty(cFileName)
            RETURN
        ENDIF
        VAR file := cFileName
        BEGIN LOCK oConn
            USING VAR cmd := SQLiteCommand{"", oConn}
            cmd:CommandText := "delete from Projects where ProjectFileName = $file"
            cmd:Parameters:AddWithValue("$file",file)
            cmd:ExecuteNonQuery()
        END LOCK
        CommitWhenNeeded()
#endregion

#region CRUD files

    STATIC METHOD DeleteFile(cFileName AS STRING) AS VOID
        IF ! IsDbOpen .OR. String.IsNullOrEmpty(cFileName)
            RETURN
        ENDIF
        VAR file    := cFileName
        Log(i"Delete Project info for project {cFileName}")
        BEGIN LOCK oConn
            USING VAR cmd := SQLiteCommand{"", oConn}
            cmd:CommandText := "DELETE FROM Files WHERE FileName = $file"
            cmd:Parameters:AddWithValue("$file",file)
            cmd:ExecuteNonQuery()
        END LOCK
        CommitWhenNeeded()

    STATIC METHOD GetFilename(idFile as Int64) AS STRING
        local result := "" as string
        BEGIN LOCK oConn
            TRY
                USING VAR cmd := SQLiteCommand{"", oConn}
                cmd:CommandText := "SELECT FileName from Files where id = "+idFile:ToString()
                USING VAR rdr := cmd:ExecuteReader()
                if rdr:Read()
                    result := rdr:GetString(0)
                endif
              CATCH e AS Exception
                Log("File   : "+idFile:ToString())
                XSettings.LogException(e, __FUNCTION__)

            END TRY
        END LOCK
        return result


    STATIC METHOD Read(oFile AS XFile) AS VOID
        IF ! IsDbOpen .OR. String.IsNullOrEmpty(oFile:FullPath)
            RETURN
        ENDIF
        VAR file    := oFile:FullPath
        Log(i"Read File info for file {file}")
        /*
        "Create Table Files ("
        " Id integer NOT NULL PRIMARY KEY, FileName text NOT NULL COLLATE NOCASE "
        " FileType integer NOT NULL, LastChanged DateTime NOT NULL, Size integer, Usings text, StaticUsings text"
        " )"

        "Create Table FilesPerProject ("
        " idFile integer NOT NULL, idProject integer NOT NULL, "
        " PRIMARY KEY (idFile, idProject), "
        " FOREIGN KEY (idFile) 	  REFERENCES Files (Id)    ON DELETE CASCADE ON UPDATE CASCADE, "
        " FOREIGN KEY (idProject) REFERENCES Projects (Id) ON DELETE CASCADE ON UPDATE CASCADE "
        " )"


        */
        LOCAL lUpdated := FALSE AS LOGIC
        BEGIN LOCK oConn
            TRY
                USING VAR cmd := SQLiteCommand{"", oConn}
                cmd:CommandText := "SELECT Id, LastChanged, Size, Usings,StaticUsings FROM Files WHERE FileName = $file"
                cmd:Parameters:AddWithValue("$file",file)
                VAR lOk := FALSE
                BEGIN USING VAR rdr := cmd:ExecuteReader()
                    IF rdr:Read()
                        oFile:Id                := rdr:GetInt64(0)
                        oFile:LastChanged       := rdr:GetDateTime(1)
                        // these can be NULL
                        oFile:Size              := DbToInt(rdr[2])
                        oFile:UsingsStr         := DbToString(rdr[3])
                        oFile:StaticUsingsStr := DbToString(rdr[4])
                        lOk := TRUE
                    ENDIF
                END USING
                IF ! lOk
                    cmd:Parameters:AddWithValue("$type",(INT) oFile:XFileType)
                    cmd:Parameters:AddWithValue("$last", DateTime.MinValue)
                    cmd:CommandText := "INSERT INTO Files( FileName, FileType, LastChanged, Size ) VALUES ( $file, $type, $last, 0); "+ ;
                        "SELECT last_insert_rowid() "
                    VAR id := (INT64) cmd:ExecuteScalar()
                    oFile:Id := id
                    oFile:LastChanged := DateTime.MinValue
                    oFile:Size        := 0
                    lUpdated := TRUE
                ENDIF
                cmd:CommandText := "SELECT count(idFile) FROM FilesPerProject WHERE idFile = $idFile and idProject = $idProject"
                cmd:Parameters:Clear()
                cmd:Parameters:AddWithValue("$idFile",oFile:Id)
                cmd:Parameters:AddWithValue("$idProject",oFile:Project:Id)
                VAR count := (INT64) cmd:ExecuteScalar()
                IF (count == 0)
                    cmd:CommandText := "INSERT INTO FilesPerProject(idFile, idProject) VALUES ($idFile, $idProject)"
                    cmd:ExecuteNonQuery()
                ENDIF
            CATCH e AS Exception
                Log("File   : "+oFile:FullPath+" "+oFile:Id:ToString())
                XSettings.LogException(e, __FUNCTION__)

            END TRY
        END LOCK
        IF lUpdated
            CommitWhenNeeded()
        ENDIF
        RETURN

    STATIC PRIVATE METHOD UpdateFileData(oFile AS XFile) AS VOID
        IF ! IsDbOpen
            RETURN
        ENDIF
        Log(i"Update File info for file {oFile.FullPath}")
        BEGIN LOCK oConn
            TRY
                IF File.Exists(oFile:FullPath)  // for files from SCC the physical file does not always exist
                    USING VAR oCmd := SQLiteCommand{"SELECT 1", oConn}
                    oCmd:CommandText := "UPDATE Files SET LastChanged = $last, Size = $size, Usings = $usings, StaticUsings = $staticUsings WHERE id = "+oFile:Id:ToString()
                    VAR fi            := FileInfo{oFile:FullPath}
                    oFile:LastChanged := fi:LastWriteTime
                    oFile:Size        := fi:Length
                    oCmd:Parameters:Clear()
                    oCmd:Parameters:AddWithValue("$last", oFile:LastChanged)
                    oCmd:Parameters:AddWithValue("$size", oFile:Size)
                    oCmd:Parameters:AddWithValue("$usings", oFile:UsingsStr)
                    oCmd:Parameters:AddWithValue("$staticUsings", oFile:StaticUsingsStr)
                    oCmd:ExecuteNonQuery()
                ENDIF
            CATCH e AS Exception
                Log("File   : "+oFile:FullPath+" "+oFile:Id:ToString())
                XSettings.LogException(e, __FUNCTION__)
            END TRY
        END LOCK
        RETURN

    STATIC METHOD Update(oFile AS XFile) AS VOID
        // No need to do this in the background.
        // It is called on a background thread while editing
        // and also called on a background thread when scanning at startup
        IF ! IsDbOpen
            RETURN
        ENDIF
        IF oFile:Id == -1
            XDatabase.Read(oFile)
        ENDIF
        IF oFile:HasCode
            UpdateFileContents(oFile)
        ENDIF
        UpdateFileData(oFile)
        CommitWhenNeeded()
        RETURN


    STATIC PRIVATE METHOD UpdateFileContents(oFile AS XFile) AS VOID
        /*
        "Create Table Types ("
        " Id integer NOT NULL PRIMARY KEY, idFile integer NOT NULL, Name text NOT NULL COLLATE NOCASE, Namespace text NOT NULL COLLATE NOCASE, "
        " Kind integer NOT NULL, BaseTypeName text COLLATE NOCASE, Attributes integer NOT NULL, "
        " Sourcecode text , XmlComments text"
        " StartLine integer , StartColumn integer, EndLine integer , EndColumn integer,   "
        " Start integer , Stop integer,  ClassType integer NOT NULL, "
        " FOREIGN KEY (idFile) REFERENCES Files (Id) ON DELETE CASCADE ON UPDATE CASCADE"
        ")"
        */
        IF ! IsDbOpen .or. oFile:TypeList == null
            RETURN
        ENDIF

        // update parameters for extension methods before we lock the database
        // This may also trigger another lock from another thread!
        VAR xtypes := oFile:TypeList:Values:Where({ t=> t.Kind != Kind.Namespace })
        FOREACH VAR typedef IN xtypes
            FOREACH VAR xmember IN typedef:XMembers
                if xmember:Signature:IsExtension .and. xmember:Parameters:Count > 0
                    local parameter := (XSourceParameterSymbol) xmember:Parameters[0] as XSourceParameterSymbol
                    if parameter:ResolvedType == null
                            parameter:Resolve()
                    endif
                endif
            NEXT
        NEXT
        Log(i"Update File contents for file {oFile.FullPath}")
        BEGIN LOCK oConn
            TRY
                /*
                 CREATE TABLE ExtensionMethods ("
                  Id integer NOT NULL PRIMARY KEY, idMember integer NOT NULL, FullName text NOT NULL COLLATE NOCASE "
                  FOREIGN KEY (idMember) REFERENCES Members (Id) ON DELETE CASCADE ON UPDATE CASCADE)
                 "
                */
                USING VAR oCmd2 := SQLiteCommand{"select 1", oConn}
                oCmd2:CommandText := "insert into ExtensionMethods (IdMember, FullName) Values ($idmember,$fullname)"
                USING VAR oCmd := SQLiteCommand{"DELETE FROM Members WHERE IdFile = "+oFile:Id:ToString(), oConn}
                oCmd:ExecuteNonQuery()

                oCmd:CommandText  := "DELETE FROM CommentTasks WHERE IdFile = "+oFile:Id:ToString()
                oCmd:ExecuteNonQuery()

                oCmd:CommandText  := "DELETE FROM Types WHERE IdFile = "+oFile:Id:ToString()
                oCmd:ExecuteNonQuery()

                oCmd:CommandText  := "DELETE FROM IncludeFilesPerFile WHERE IdFile = "+oFile:Id:ToString()
                oCmd:ExecuteNonQuery()

                oCmd:CommandText := "INSERT INTO Types (Name, IdFile, Namespace, Kind,  BaseTypeName, Attributes,  Sourcecode, XmlComments, " + ;
                    "                 StartLine,  StartColumn,  EndLine,  EndColumn,  Start,  Stop, ClassType) " +;
                    " VALUES ($name, $file, $namespace, $kind, $baseTypeName,  $attributes, $sourcecode, $xmlcomments, " +;
                    "           $startline, $startcolumn, $endline, $endcolumn, $start, $stop, $classtype) ;" +;
                    " SELECT last_insert_rowid()"
                VAR pars := List<SQLiteParameter>{} { ;
                    oCmd:Parameters:AddWithValue("$name", ""),;
                    oCmd:Parameters:AddWithValue("$file", 0),;
                    oCmd:Parameters:AddWithValue("$namespace", ""),;
                    oCmd:Parameters:AddWithValue("$kind", 0),;
                    oCmd:Parameters:AddWithValue("$baseTypeName", ""),;
                    oCmd:Parameters:AddWithValue("$attributes", 0),;
                    oCmd:Parameters:AddWithValue("$sourcecode", ""),;
                    oCmd:Parameters:AddWithValue("$xmlcomments", ""),;
                    oCmd:Parameters:AddWithValue("$startline", 0),;
                    oCmd:Parameters:AddWithValue("$startcolumn", 0),;
                    oCmd:Parameters:AddWithValue("$endline", 0),;
                    oCmd:Parameters:AddWithValue("$endcolumn", 0),;
                    oCmd:Parameters:AddWithValue("$start", 0),;
                    oCmd:Parameters:AddWithValue("$stop", 0),;
                    oCmd:Parameters:AddWithValue("$classtype", 0)}
                VAR types := oFile:TypeList:Values:Where({ t=> t.Kind != Kind.Namespace })
                FOREACH VAR typedef IN types
                    TRY
                        pars[0]:Value := typedef:Name
                        pars[1]:Value := oFile:Id
                        pars[2]:Value := typedef:Namespace
                        pars[3]:Value := (INT) typedef:Kind
                        pars[4]:Value := typedef:BaseTypeName
                        pars[5]:Value := (INT) typedef:Attributes
                        pars[6]:Value := typedef:SourceCode
                        pars[7]:Value := typedef:XmlComments
                        pars[8]:Value := typedef:Range:StartLine
                        pars[9]:Value := typedef:Range:StartColumn
                        pars[10]:Value := typedef:Range:EndLine
                        pars[11]:Value := typedef:Range:EndColumn
                        pars[12]:Value := typedef:Interval:Start
                        pars[13]:Value := typedef:Interval:Stop
                        pars[14]:Value := (INT) typedef:ClassType
                        VAR id := (INT64) oCmd:ExecuteScalar()
                        typedef:Id := id
                    CATCH e AS Exception
                        Log("File   : "+oFile:FullPath+" "+oFile:Id:ToString())
                        Log("Typedef: "+typedef:Name)
                        Log("Kind   : "+typedef:Kind:ToString())
                        XSettings.LogException(e, __FUNCTION__)
                    END TRY
                NEXT
                /*
                "Create Table Members ("
                " Id integer NOT NULL PRIMARY KEY, IdType integer NOT NULL , IdFile integer NOT NULL, "
                " Name text COLLATE NOCASE, Kind integer , Attributes integer , "
                " SourceCode text, XmlComments text, StartLine integer , StartColumn integer ,  "
                " EndLine integer , EndColumn integer , Start integer , Stop integer , ReturnType text "
                " FOREIGN KEY (idType) REFERENCES Types (Id) ON DELETE CASCADE ON UPDATE CASCADE, "
                " FOREIGN KEY (idFile) REFERENCES Files (Id) ON DELETE CASCADE ON UPDATE CASCADE"
                ")"
                */
                oCmd:CommandText := "INSERT INTO Members (idFile, idType, Name, Kind, Attributes, Sourcecode , XMLComments, " + ;
                    " StartLine, StartColumn, EndLine, EndColumn, Start, Stop, ReturnType) " +;
                    " VALUES ($file, $type, $name, $kind, $attributes,$sourcecode, $xmlcomments," + ;
                    "           $startline, $startcolumn, $endline, $endcolumn, $start, $stop, $returntype) ;" +;
                    " SELECT last_insert_rowid()"
                oCmd:Parameters:Clear()
                pars := List<SQLiteParameter>{} { ;
                    oCmd:Parameters:AddWithValue("$file", oFile:Id),;
                    oCmd:Parameters:AddWithValue("$type", 0),;
                    oCmd:Parameters:AddWithValue("$name", ""),;
                    oCmd:Parameters:AddWithValue("$kind", 0),;
                    oCmd:Parameters:AddWithValue("$attributes", 0),;
                    oCmd:Parameters:AddWithValue("$startline", 0),;
                    oCmd:Parameters:AddWithValue("$startcolumn", 0),;
                    oCmd:Parameters:AddWithValue("$endline", 0),;
                    oCmd:Parameters:AddWithValue("$endcolumn", 0),;
                    oCmd:Parameters:AddWithValue("$start", 0),;
                    oCmd:Parameters:AddWithValue("$stop", 0),;
                    oCmd:Parameters:AddWithValue("$sourcecode", ""),;
                    oCmd:Parameters:AddWithValue("$xmlcomments", ""),;
                    oCmd:Parameters:AddWithValue("$returntype", "")}
                FOREACH VAR typedef IN types
                    FOREACH VAR xmember IN typedef:XMembers
                        TRY
                            // file is constant
                            pars[ 0]:Value := oFile:Id
                            pars[ 1]:Value := typedef:Id
                            pars[ 2]:Value := xmember:Name
                            pars[ 3]:Value := (INT) xmember:Kind
                            pars[ 4]:Value := (INT) xmember:Attributes
                            pars[ 5]:Value := xmember:Range:StartLine
                            pars[ 6]:Value := xmember:Range:StartColumn
                            pars[ 7]:Value := xmember:Range:EndLine
                            pars[ 8]:Value := xmember:Range:EndColumn
                            pars[ 9]:Value := xmember:Interval:Start
                            pars[10]:Value := xmember:Interval:Stop
                            pars[11]:Value := xmember:SourceCode
                            pars[12]:Value := xmember:XmlComments
                            pars[13]:Value := xmember:ReturnType
                            VAR id := (INT64) oCmd:ExecuteScalar()
                            xmember:Id := id
                            if xmember:Signature:IsExtension .and. xmember:Parameters:Count > 0
                                oCmd2:Parameters:Clear()
                                local parameter := (XSourceParameterSymbol) xmember:Parameters[0] as XSourceParameterSymbol
                                oCmd2:Parameters:AddWithValue("$idmember", xmember:Id)
                                oCmd2:Parameters:AddWithValue("$fullname", parameter:FullName)
                                oCmd2:ExecuteNonQuery()
                            endif
                        CATCH e AS Exception

                            Log("File   : "+oFile:FullPath+" "+oFile:Id:ToString())
                            Log("Typedef: "+typedef:Name+" "+typedef:Id:ToString())
                            Log("Member : "+xmember:Name)
                            Log("Kind   : "+xmember:Kind:ToString())
                            Log("Line :   "+xmember:Range:StartLine:ToString())
                            Log("Column : "+xmember:Range:StartColumn:ToString())
                            XSettings.LogException(e, __FUNCTION__)
                        END TRY
                    NEXT
                NEXT
                // Save local functions and procedure
                FOREACH xmember as XSourceMemberSymbol in oFile:EntityList:Where ( {m => m.Kind.IsLocal() } )
                    TRY
                        // file is constant
                        pars[ 0]:Value := oFile:Id
                        pars[ 1]:Value := ((XSourceTypeSymbol) xmember:ParentType):Id
                        pars[ 2]:Value := xmember:Name
                        pars[ 3]:Value := (INT) xmember:Kind
                        pars[ 4]:Value := (INT) xmember:Attributes
                        pars[ 5]:Value := xmember:Range:StartLine
                        pars[ 6]:Value := xmember:Range:StartColumn
                        pars[ 7]:Value := xmember:Range:EndLine
                        pars[ 8]:Value := xmember:Range:EndColumn
                        pars[ 9]:Value := xmember:Interval:Start
                        pars[10]:Value := xmember:Interval:Stop
                        pars[11]:Value := xmember:SourceCode
                        pars[12]:Value := xmember:XmlComments
                        pars[13]:Value := xmember:ReturnType
                        VAR id := (INT64) oCmd:ExecuteScalar()
                        xmember:Id := id
                    CATCH e AS Exception
                        Log("File   : "+oFile:FullPath+" "+oFile:Id:ToString())
                        Log("Member : "+xmember:Name)
                        Log("Kind   : "+xmember:Kind:ToString())
                        Log("Line :   "+xmember:Range:StartLine:ToString())
                        Log("Column : "+xmember:Range:StartColumn:ToString())
                        XSettings.LogException(e, __FUNCTION__)

                    END TRY
                NEXT

                if oFile:CommentTasks != NULL
                    oCmd:CommandText := "INSERT INTO CommentTasks (idFile, Line, Column, Priority,Comment) " +;
                        " VALUES ($file, $line, $column, $priority, $comment) ;" +;
                        " SELECT last_insert_rowid()"
                    oCmd:Parameters:Clear()
                    pars := List<SQLiteParameter>{} { ;
                        oCmd:Parameters:AddWithValue("$file", oFile:Id),;
                        oCmd:Parameters:AddWithValue("$line", 0),;
                        oCmd:Parameters:AddWithValue("$column", 0),;
                        oCmd:Parameters:AddWithValue("$priority", 0),;
                        oCmd:Parameters:AddWithValue("$comment", "")}
                    FOREACH task AS XCommentTask IN oFile:CommentTasks
                        pars[ 0]:Value := oFile:Id
                        pars[ 1]:Value := task:Line
                        pars[ 2]:Value := task:Column
                        pars[ 3]:Value := task:Priority
                        pars[ 4]:Value := task:Comment
                        oCmd:ExecuteScalar()
                    NEXT
                endif
                // Update Includefile IDs and write to disk
                foreach include as XInclude in oFile:IncludeFiles:ToArray()
                    if include:Id == -1
                        oCmd:CommandText := "SELECT Id from IncludeFiles where fileName = $fileName"
                        oCmd:Parameters:Clear()
                        var fn := include:FileName
                        if fn:Contains(".\")
                            fn := System.IO.Path.GetFullPath(fn)
                        endif
                        oCmd:Parameters:AddWithValue("$fileName",fn)
                        using var rdr := oCmd:ExecuteReader()
                        if rdr:Read()
                            include:Id := rdr.GetInt64(0)
                        else
                            rdr:Close()
                            oCmd:CommandText := "INSERT INTO IncludeFiles (FileName) " +;
                                " VALUES ($fileName) ;" +;
                                " SELECT last_insert_rowid()"
                            oCmd:Parameters:Clear()
                            oCmd:Parameters:AddWithValue("$fileName", fn)
                            VAR id := (INT64) oCmd:ExecuteScalar()
                            include:Id := id
                        endif
                    endif
                    oCmd:CommandText := "INSERT INTO IncludeFilesPerFile (IdInclude, IdFile) " +;
                        " VALUES ($idInclude, $idFile) "
                    oCmd:Parameters:Clear()
                    oCmd:Parameters:AddWithValue("$idInclude", include:Id)
                    oCmd:Parameters:AddWithValue("$idFile", oFile:Id)
                    oCmd:ExecuteNonQuery()
                next
                oCmd:CommandText := "Delete from IncludeFiles where Id not in (select IdInclude from IncludeFilesPerFile)"
                oCmd:Parameters:Clear()
                oCmd:ExecuteScalar()

            CATCH e AS Exception
                Log("File   : "+oFile:FullPath+" "+oFile:Id:ToString())
                XSettings.LogException(e, __FUNCTION__)

            END TRY

        END LOCK
        RETURN



#endregion


#region CRUD Assemblies
    STATIC METHOD Read(oAssembly AS XAssembly) AS VOID
        IF ! IsDbOpen .OR. String.IsNullOrEmpty(oAssembly:FullName) .OR. String.IsNullOrEmpty(oAssembly:FileName)
            RETURN
        ENDIF
        /*
        stmt  	:=  "Create Table Assemblies ("
        stmt	   +=  " Id integer NOT NULL PRIMARY KEY, Name text NOT NULL COLLATE NOCASE, FileName text NOT NULL COLLATE NOCASE, "
        stmt     +=  " LastChanged DateTime NOT NULL, Size integer "
        stmt	   += ")"
        */
        Log(i"Read Assembly info for assembly {oAssembly.FileName}")
        VAR file    := oAssembly:FileName
        VAR name    := oAssembly:FullName
        VAR lUpdated := FALSE
        BEGIN LOCK oConn
            TRY
                USING VAR cmd := SQLiteCommand{"", oConn}
                cmd:CommandText := "SELECT Id, Name, AssemblyFileName, LastChanged, Size from Assemblies where  AssemblyFileName = $file"
                cmd:Parameters:AddWithValue("$file",file)
                VAR lOk := FALSE
                BEGIN USING VAR rdr := cmd:ExecuteReader()
                    IF rdr:Read()
                        oAssembly:Id            := rdr:GetInt64(0)
                        oAssembly:LastChanged   := rdr:GetDateTime(3)
                        oAssembly:Size          := rdr:GetInt64(4)
                        lOk := TRUE
                    ENDIF
                END USING
                IF ! lOk
                    cmd:CommandText := "INSERT INTO Assemblies(Name, AssemblyFileName,LastChanged, Size ) values ($name, $file,$last,0); SELECT last_insert_rowid() "
                    cmd:Parameters:AddWithValue("$name",name)
                    cmd:Parameters:AddWithValue("$last", DateTime.MinValue)
                    VAR id := (INT64) cmd:ExecuteScalar()
                    oAssembly:Id := id
                    lUpdated := TRUE
                ENDIF
            CATCH e AS Exception
                Log("Assembly : "+oAssembly:FileName+" "+oAssembly:Id:ToString())
                XSettings.LogException(e, __FUNCTION__)
            END TRY

        END LOCK
        IF lUpdated
            CommitWhenNeeded()
        ENDIF
        RETURN

    STATIC METHOD Delete(oAssembly as XAssembly) AS VOID
        IF ! IsDbOpen .OR. String.IsNullOrEmpty(oAssembly:FullName) .OR. String.IsNullOrEmpty(oAssembly:FileName)
            RETURN
        ENDIF
        BEGIN LOCK oConn
            TRY
                USING VAR oCmd := SQLiteCommand{"Delete From Assemblies where Id = "+oAssembly:Id:ToString(), oConn}
                oCmd:ExecuteNonQuery()
            CATCH e AS Exception
                Log("Assembly : "+oAssembly:FileName+" "+oAssembly:Id:ToString())
                XSettings.LogException(e, __FUNCTION__)

            END TRY
        END LOCK
        RETURN


    STATIC METHOD Update(oAssembly AS XAssembly) AS VOID
        IF ! IsDbOpen .OR. String.IsNullOrEmpty(oAssembly:FullName) .OR. String.IsNullOrEmpty(oAssembly:FileName)
            RETURN
        ENDIF
        Log(i"Update Assembly info for assembly {oAssembly.FileName}")
        BEGIN LOCK oConn
            TRY
                USING VAR oCmd := SQLiteCommand{"SELECT 1", oConn}
                LOCAL globalType := NULL as XPETypeSymbol
                LOCAL hasGlobalClass as LOGIC
                hasGlobalClass := !String.IsNullOrEmpty(oAssembly:GlobalClassName)
                oCmd:CommandText := "Delete From ReferencedTypes Where idAssembly = "+oAssembly:Id:ToString()
                oCmd:ExecuteNonQuery()
                oCmd:CommandText := "Delete From ReferencedGlobals Where idAssembly = "+oAssembly:Id:ToString()
                oCmd:ExecuteNonQuery()
                // Updated TypeReferences
                //	   "Create Table ReferencedTypes ("
                //	   " Id integer NOT NULL PRIMARY KEY, idAssembly integer NOT NULL, Name text NOT NULL COLLATE NOCASE, Namespace text NOT NULL COLLATE NOCASE, "
                //	   " FullName text NOT NULL COLLATE NOCASE, Kind integer NOT NULL, BaseTypeName text COLLATE NOCASE, Attributes integer NOT NULL, "
                //	   " FOREIGN KEY (idAssembly) REFERENCES Assemblies (Id) ON DELETE CASCADE ON UPDATE CASCADE"
                //	  ")"
                oCmd:CommandText := "INSERT INTO ReferencedTypes (idAssembly, Name, Namespace, Kind, BaseTypeName, Attributes,FullName) " + ;
                    " values ($id, $name, $namespace, $kind, $basetypename, $attributes,$fullname) "
                oCmd:Parameters:Clear()
                VAR pars := List<SQLiteParameter>{} { ;
                    oCmd:Parameters:AddWithValue("$id", 0),;
                    oCmd:Parameters:AddWithValue("$name", ""),;
                    oCmd:Parameters:AddWithValue("$namespace", ""),;
                    oCmd:Parameters:AddWithValue("$fullname", ""),;
                    oCmd:Parameters:AddWithValue("$kind", 0),;
                    oCmd:Parameters:AddWithValue("$basetypename", ""),;
                    oCmd:Parameters:AddWithValue("$attributes",0)}
                FOREACH VAR typeref IN oAssembly:Types:Values
                    pars[0]:Value := oAssembly:Id
                    pars[1]:Value := typeref:TickedName // when generic then the name followed with `<n>
                    pars[2]:Value := typeref:Namespace
                    pars[3]:Value := typeref:FullTickedName
                    pars[4]:Value := (INT) typeref:Kind
                    pars[5]:Value := typeref:BaseType
                    pars[6]:Value := (INT) typeref:Attributes
                    oCmd:ExecuteNonQuery()
                    IF hasGlobalClass .and. typeref:FullName == oAssembly:GlobalClassName
                        globalType := typeref
                    ENDIF
                NEXT
                IF oAssembly:GlobalMembers:Count > 0
                    // "CREATE TABLE ReferencedGlobals ("
                    // " Id integer NOT NULL PRIMARY KEY, idAssembly integer NOT NULL, Name text NOT NULL COLLATE NOCASE, "
                    // " FullName text NOT NULL, Kind integer NOT NULL, Attributes integer NOT NULL, Sourcecode text, ReturnType text, "
                    oCmd:CommandText := "INSERT INTO ReferencedGlobals(IdAssembly, Name, FullName, Kind, Attributes, Sourcecode, ReturnType) " + ;
                        " Values ($id, $name, $fullname, $kind, $attributes, $source, $return) "

                    oCmd:Parameters:Clear()
                    pars := List<SQLiteParameter>{} { ;
                        oCmd:Parameters:AddWithValue("$id", 0),;
                        oCmd:Parameters:AddWithValue("$name", ""),;
                        oCmd:Parameters:AddWithValue("$fullname", ""),;
                        oCmd:Parameters:AddWithValue("$kind", 0),;
                        oCmd:Parameters:AddWithValue("$attributes", 0),;
                        oCmd:Parameters:AddWithValue("$source", ""),;
                        oCmd:Parameters:AddWithValue("$return", "")}
                    FOREACH VAR item IN oAssembly:GlobalMembers
                        var xmember := item:Value
                        pars[0]:Value := oAssembly:Id
                        pars[1]:Value := xmember:Name
                        pars[2]:Value := xmember:FullName
                        pars[3]:Value := (int) xmember:Kind
                        pars[4]:Value := (INT) xmember:Attributes
                        pars[5]:Value := xmember:GetProtoType()
                        pars[6]:Value := xmember:TypeName
                        oCmd:ExecuteNonQuery()
                    NEXT
                ENDIF

                oCmd:CommandText := "Update Assemblies set LastChanged = $last, Size = $size where id = "+oAssembly:Id:ToString()
                VAR fi            := FileInfo{oAssembly:FileName}
                oAssembly:LastChanged := fi:LastWriteTime
                oAssembly:Size        := fi:Length
                oCmd:Parameters:Clear()
                oCmd:Parameters:AddWithValue("$last", oAssembly:LastChanged)
                oCmd:Parameters:AddWithValue("$size", oAssembly:Size)
                oCmd:ExecuteNonQuery()
            CATCH e AS Exception
                Log("Assembly : "+oAssembly:FileName+" "+oAssembly:Id:ToString())
                XSettings.LogException(e, __FUNCTION__)

            END TRY

        END LOCK
        RETURN
#endregion

    PRIVATE STATIC METHOD _FindFunctionWorker(sName AS STRING, sProjectIds AS STRING, lUseLike as LOGIC) AS IList<XDbResult>
        VAR result := List<XDbResult>{}
        VAR sLike := sName
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{"SELECT 1", oConn}
                    var crit := "name = $name"
                    if lUseLike
                        sLike += "%"
                        crit := "name like $name"
                    ENDIF
                    oCmd:CommandText := "SELECT * FROM ProjectMembers WHERE " + crit +  ;
                        " and typeName = $typename AND Kind in ($kind1, $kind2, $kind3, $kind4,$kind5,$kind6) AND IdProject in ("+sProjectIds+") "+ ;
                        " Order by FileName"
                    oCmd:Parameters:AddWithValue("$name", sLike)
                    oCmd:Parameters:AddWithValue("$typename", XLiterals.GlobalName)
                    oCmd:Parameters:AddWithValue("$kind1", (INT) Kind.Function)
                    oCmd:Parameters:AddWithValue("$kind2", (INT) Kind.Procedure)
                    oCmd:Parameters:AddWithValue("$kind3", (INT) Kind.Method)
                    oCmd:Parameters:AddWithValue("$kind4", (INT) Kind.VODLL)
                    oCmd:Parameters:AddWithValue("$kind5", (INT) Kind.LocalFunc)
                    oCmd:Parameters:AddWithValue("$kind6", (INT) Kind.LocalProc)
                    USING VAR rdr := oCmd:ExecuteReader()
                    DO WHILE rdr:Read() .and. result:Count < XEditorSettings.MaxCompletionEntries
                        var mem := CreateMemberInfo(rdr)
                        if lUseLike
                            if mem:MemberName:StartsWith(sName, StringComparison.OrdinalIgnoreCase)
                                result:Add(mem)
                            endif
                        else
                            result:Add(mem)
                        endif
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)

                END TRY
            END LOCK
        ENDIF
        RETURN result

    STATIC METHOD FindFunction(sName AS STRING, sProjectIds AS STRING) AS IList<XDbResult>
        // search class members in the Types list
        var result := _FindFunctionWorker(sName, sProjectIds, FALSE)
        Log(i"FindFunction '{sName}' returns {result.Count} matches")
        RETURN result

    STATIC METHOD FindFunctionLike(sName AS STRING, sProjectIds AS STRING) AS IList<XDbResult>
        // search class members in the Types list
        var result := _FindFunctionWorker(sName, sProjectIds, TRUE)
        Log(i"FindFunctionLike '{sName}' returns {result.Count} matches")
        RETURN result

    STATIC METHOD _FindGlobalOrDefineWorker(sName AS STRING, sProjectIds AS STRING, lUseLike as LOGIC) AS IList<XDbResult>
        // search class members in the Types list
        VAR result := List<XDbResult>{}
        VAR sLike  := sName
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{"SELECT 1", oConn}
                    var crit := "name = $name"
                    if lUseLike
                        sLike += "%"
                        crit := "name like $name"
                    ENDIF
                    oCmd:CommandText := "SELECT * FROM ProjectMembers WHERE "+crit+"  AND TypeName = $typename " + ;
                        " AND Kind in ($kind1, $kind2, $kind3) AND IdProject in ("+sProjectIds+")" +;
                        " Order by FileName"
                    oCmd:Parameters:AddWithValue("$name", sLike)
                    oCmd:Parameters:AddWithValue("$kind1", (INT) Kind.VOGlobal)
                    oCmd:Parameters:AddWithValue("$kind2", (INT) Kind.VODefine)
                    oCmd:Parameters:AddWithValue("$kind3", (INT) Kind.Define)
                    oCmd:Parameters:AddWithValue("$typename", XLiterals.GlobalName)
                    USING VAR rdr := oCmd:ExecuteReader()
                    DO WHILE rdr:Read() .and. result:Count < XEditorSettings.MaxCompletionEntries
                        var mem := CreateMemberInfo(rdr)
                        if lUseLike
                            if mem:MemberName:StartsWith(sName, StringComparison.OrdinalIgnoreCase)
                                result:Add(mem)
                            endif
                        else
                            result:Add(mem)
                        endif
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY
            END LOCK
        ENDIF
        RETURN result



    STATIC METHOD FindProjectGlobalOrDefine(sName AS STRING, sProjectIds AS STRING) AS IList<XDbResult>
        // search class members in the Types list
        VAR result := _FindGlobalOrDefineWorker(sName, sProjectIds, FALSE)
        Log(i"FindGlobalOrDefine '{sName}' returns {result.Count} matches")
        RETURN result


    STATIC METHOD FindProjectGlobalOrDefineLike(sName AS STRING, sProjectIds AS STRING) AS IList<XDbResult>
        // search class members in the Types list
        VAR result := _FindGlobalOrDefineWorker(sName, sProjectIds, TRUE)
        Log(i"FindGlobalOrDefineLike '{sName}' returns {result.Count} matches")
        RETURN result

    STATIC METHOD _FindAssemblyWorker(sName AS STRING, sAssemblyIds AS STRING, nKind1 as Kind, nKind2 as Kind,nKind3 as Kind,like as LOGIC) AS IList<XDbResult>
        // search class members in the Types list
        VAR result := List<XDbResult>{}
        VAR sLike  := sName
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    IF like .and. ! sLike:EndsWith("%")
                        sLike += "%"
                    ENDIF
                    USING VAR oCmd := SQLiteCommand{"SELECT 1", oConn}
                    oCmd:CommandText := "SELECT * FROM AssemblyGlobals WHERE name like $name " + ;
                        " AND Kind in ($kind1,$kind2,$kind3) AND IdAssembly in ("+sAssemblyIds+")"
                    oCmd:Parameters:AddWithValue("$name", sLike)
                    oCmd:Parameters:AddWithValue("$kind1", (INT) nKind1)
                    oCmd:Parameters:AddWithValue("$kind2", (INT) nKind2)
                    oCmd:Parameters:AddWithValue("$kind3", (INT) nKind3)
                    USING VAR rdr := oCmd:ExecuteReader()
                    DO WHILE rdr:Read() .and. result:Count < XEditorSettings.MaxCompletionEntries
                        // extra filter because they may be seeking for "FO_" and we do not want the _ character to be seen as wildcard
                        var mem := CreateAssemblyMemberInfo(rdr)
                        if mem:MemberName:StartsWith(sName, StringComparison.OrdinalIgnoreCase)
                            result:Add(mem)
                        endif
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY
            END LOCK
        ENDIF
        RETURN result


    STATIC METHOD FindAssemblyGlobalOrDefine(sName AS STRING, sAssemblyIds AS STRING, lLike as LOGIC) AS IList<XDbResult>
        // search class members in the Types list
        VAR result := _FindAssemblyWorker(sName, sAssemblyIds, Kind.Field, Kind.VODefine, Kind.VOGlobal, lLike)
        Log(i"FindAssemblyGlobalOrDefine '{sName}' returns {result.Count} matches")

        RETURN result

    STATIC METHOD FindAssemblyFunction(sName AS STRING, sAssemblyIds AS STRING, lLike as LOGIC) AS IList<XDbResult>
        // search class members in the Types list
        VAR result := _FindAssemblyWorker(sName, sAssemblyIds, Kind.Function, Kind.Procedure, Kind.Procedure, lLike)
        Log(i"FindAssemblyFunction '{sName}' returns {result.Count} matches")
        RETURN result

    STATIC METHOD GetProjectTypes(sName AS STRING, sProjectIds AS STRING) AS IList<XDbResult>
        VAR stmt := "Select * from ProjectTypes where name = $name AND IdProject in ("+sProjectIds+")"
        VAR result := List<XDbResult>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    oCmd:Parameters:AddWithValue("$name", sName)
                    USING VAR rdr := oCmd:ExecuteReader()
                    DO WHILE rdr:Read() .and. result:Count < XEditorSettings.MaxCompletionEntries
                        result:Add(CreateTypeInfo(rdr))
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY
            END LOCK
        ENDIF
        Log(i"GetProjectTypes '{sName}' returns {result.Count} matches")
        RETURN result

    STATIC METHOD GetProjectTypesLike(sName AS STRING, sProjectIds AS STRING) AS IList<XDbResult>
        VAR stmt := "Select * from ProjectTypes where name like $name or namespace like $name AND IdProject in ("+sProjectIds+")"
        VAR result := List<XDbResult>{}
        var sLike := sName + "%"
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    oCmd:Parameters:AddWithValue("$name", sLike)
                    USING VAR rdr := oCmd:ExecuteReader()
                    DO WHILE rdr:Read() .and. result:Count < XEditorSettings.MaxCompletionEntries
                        var type := CreateTypeInfo(rdr)
                        if type:FullName:StartsWith(sName, StringComparison.OrdinalIgnoreCase) .or. type:TypeName:StartsWith(sName, StringComparison.OrdinalIgnoreCase)
                            result:Add(type)
                        endif
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY
            END LOCK
        ENDIF
        Log(i"GetProjectTypesLike '{sName}' returns {result.Count} matches")
        RETURN result

    STATIC METHOD GetProjectTypesInNamespace(sName AS STRING, sProjectIds AS STRING) AS IList<XDbResult>
        VAR stmt := "Select * from ProjectTypes where namespace = $name AND idProject in ("+sProjectIds+")"
        VAR result := List<XDbResult>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    oCmd:Parameters:AddWithValue("$name", sName)
                    USING VAR rdr := oCmd:ExecuteReader()
                    DO WHILE rdr:Read() .and. result:Count < XEditorSettings.MaxCompletionEntries
                        var type := CreateTypeInfo(rdr)
                        result:Add(type)
                    ENDDO

                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY
            END LOCK
        ENDIF
        Log(i"GetProjectTypesInNamespace '{sName}' returns {result.Count} matches")
        RETURN result

    STATIC METHOD GetAssemblyTypes(sName AS STRING, sAssemblyIds AS STRING) AS IList<XDbResult>
        VAR stmt := "Select * from AssemblyTypes where name = $name or fullname = $name AND IdAssembly in ("+sAssemblyIds+")"
        VAR result := List<XDbResult>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    oCmd:Parameters:AddWithValue("$name", sName)
                    USING VAR rdr := oCmd:ExecuteReader()
                    DO WHILE rdr:Read() .and. result:Count < XEditorSettings.MaxCompletionEntries
                        result:Add(CreateRefTypeInfo(rdr))
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY
            END LOCK
        ENDIF
        Log(i"GetAssemblyTypes '{sName}' returns {result.Count} matches")
        RETURN result
    STATIC METHOD GetAssemblyTypesLike(sName AS STRING, sAssemblyIds AS STRING) AS IList<XDbResult>
        VAR stmt := "Select * from AssemblyTypes where name like $name or fullname like $name AND IdAssembly in ("+sAssemblyIds+")"
        VAR result := List<XDbResult>{}
        var sLike := sName + "%"
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    oCmd:Parameters:AddWithValue("$name", sLike)
                    USING VAR rdr := oCmd:ExecuteReader()
                    DO WHILE rdr:Read() .and. result:Count < XEditorSettings.MaxCompletionEntries
                        var type := CreateRefTypeInfo(rdr)
                        if type:FullName:StartsWith(sName, StringComparison.OrdinalIgnoreCase) .or. type:TypeName:StartsWith(sName, StringComparison.OrdinalIgnoreCase)
                            result:Add(type)
                        endif
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY
            END LOCK
        ENDIF
        Log(i"GetAssemblyTypesLike '{sName}' returns {result.Count} matches")
        RETURN result
    STATIC METHOD GetAssemblyTypesInNamespace(sName AS STRING, sAssemblyIds AS STRING) AS IList<XDbResult>
        VAR stmt := "Select * from AssemblyTypes where namespace = $name AND idAssembly in ("+sAssemblyIds+")"
        VAR result := List<XDbResult>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    oCmd:Parameters:AddWithValue("$name", sName)
                    USING VAR rdr := oCmd:ExecuteReader()
                    DO WHILE rdr:Read() .and. result:Count < XEditorSettings.MaxCompletionEntries
                        var type := CreateRefTypeInfo(rdr)
                        if type:TypeName:StartsWith(sName, StringComparison.OrdinalIgnoreCase)
                            result:Add(type)
                        elseif type:FullName:StartsWith(sName, StringComparison.OrdinalIgnoreCase)
                            result:Add(type)
                        endif
                    ENDDO

                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY
            END LOCK
        ENDIF
        Log(i"GetAssemblyTypesInNamespace '{sName}' returns {result.Count} matches")
        RETURN result

    STATIC METHOD GetCommentTasks(sProjectIds AS STRING) AS IList<XDbResult>
        VAR stmt := "Select Line, Column, Priority, Comment, FileName from " + ;
            " ProjectCommentTasks where IdProject in ("+sProjectIds+") Order BY IdProject, IdFile "
        VAR result := List<XDbResult>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    USING VAR rdr := oCmd:ExecuteReader()
                    DO WHILE rdr:Read()
                        result:Add(CreateCommentTask(rdr))
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY
            END LOCK
        ENDIF
        Log(i"GetCommentTasks returns {result.Count} matches")
        RETURN result

    STATIC METHOD GetFilesOfType(type as XFileType, sProjectIds as STRING) AS IList<STRING>
        VAR stmt := "Select distinct FileName from ProjectFiles where FileType = $type AND IdProject in ("+sProjectIds+")"
        VAR result := List<String>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    oCmd:Parameters:AddWithValue("$type",(INT) type)
                    USING VAR rdr := oCmd:ExecuteReader()
                    DO WHILE rdr:Read() .and. result:Count < XEditorSettings.MaxCompletionEntries
                        result:Add(DbToString(rdr[0]))
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY
            END LOCK
        ENDIF
        RETURN result

    STATIC METHOD GetProjectNamespaces(sProjectIds AS STRING) AS IList<STRING>
        VAR stmt := "Select Namespace from ProjectNamespaces where IdProject in ("+sProjectIds+")"
        VAR result := List<STRING>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    USING VAR rdr := oCmd:ExecuteReader()
                    // No limit on the # of Namespaces
                    DO WHILE rdr:Read()
                        VAR ns := DbToString(rdr[0])
                        IF ! String.IsNullOrEmpty(ns)
                            result:Add(ns)
                        ENDIF
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY
            END LOCK
        ENDIF
        Log(i"GetProjectNamespaces returns {result.Count} matches")
        RETURN result

    STATIC METHOD GetAssemblyNamespaces(sAssemblyIds AS STRING) AS IList<STRING>
        VAR stmt := "Select distinct Namespace from AssemblyNamespaces where IdAssembly in ("+sAssemblyIds+")"
        VAR result := List<STRING>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    USING VAR rdr := oCmd:ExecuteReader()
                    // No limit on the # of Namespaces
                    DO WHILE rdr:Read()
                        VAR ns := DbToString(rdr[0])
                        IF ! String.IsNullOrEmpty(ns)
                            result:Add(ns)
                        ENDIF
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY
            END LOCK
        ENDIF
        Log(i"GetAssemblyNamespaces returns {result.Count} matches")
        RETURN result


    STATIC METHOD GetAssembliesContainingNamespace(sNamespace as string, sAssemblyIds as string) AS IList<String>
        VAR stmt := "Select distinct AssemblyFileName from AssemblyNamespaces where Namespace='"+sNamespace+;
            "' and idAssembly in ("+sAssemblyIds+")"
        var result := List<string>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    USING VAR rdr := oCmd:ExecuteReader()
                    // No limit on the # of Namespaces
                    DO WHILE rdr:Read()
                        result:Add(rdr:GetString(0))
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY
            END LOCK
        ENDIF
        Log(i"GetAssemblyNamespaceFiles returns {result.Count} matches")
        RETURN result


    STATIC METHOD GetNamespacesInFile(sFileId AS STRING) AS IList<XDbResult>
        RETURN GetNamespacesInFile( sFileId, FALSE )

    STATIC METHOD GetNamespacesInFile(sFileId AS STRING, keepEmptyName AS LOGIC ) AS IList<XDbResult>
        VAR stmt := "Select distinct Namespace from ProjectTypes where Namespace != '' and IdFile = "+sFileId
        VAR result := List<XDbResult>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    USING VAR rdr := oCmd:ExecuteReader()
                    // No limit on the # of Namespaces
                    DO WHILE rdr:Read()
                        VAR res := XDbResult{}
                        IF rdr[0] != DBNull.Value
                            res:Kind         := Kind.Namespace
                            res:Namespace    := DbToString(rdr[0])
                            res:TypeName     := res:Namespace
                            IF !String.IsNullOrEmpty(res:Namespace) .OR. keepEmptyName
                                result:Add(res)
                            ENDIF
                        ENDIF
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY
            END LOCK
        ENDIF
        Log(i"GetNamespacesInFile returns {result.Count} matches")
        RETURN result

    STATIC METHOD GetTypesInFile(sFileId AS STRING) AS IList<XDbResult>
        VAR stmt := "Select * from ProjectTypes where IdFile = "+sFileId
        VAR result := List<XDbResult>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    USING VAR rdr := oCmd:ExecuteReader()
                    DO WHILE rdr:Read() .and. result:Count < XEditorSettings.MaxCompletionEntries
                        result:Add(CreateTypeInfo(rdr))
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY
            END LOCK
        ENDIF
        Log(i"GetTypesInFile returns {result.Count} matches")
        RETURN result


    STATIC METHOD GetExtensionMethods (sProjectIds AS STRING, typeName as STRING) AS IList<XDbResult>
       VAR stmt := "Select * from ProjectExtensionMethods where idProject IN ("+sProjectIds+") AND fullName = '"+typeName+"'"
       VAR result := List<XDbResult>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    USING VAR rdr := oCmd:ExecuteReader()
                    DO WHILE rdr:Read() .and. result:Count < XEditorSettings.MaxCompletionEntries
                        result:Add(CreateMemberInfo(rdr))
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY

            END LOCK
        ENDIF
        Log(i"GetExtensionMethods '{typeName}' returns {result.Count} matches")
        RETURN result

    STATIC METHOD GetMembers(idType AS INT64) AS IList<XDbResult>
        VAR stmt := "Select * from ProjectMembers where IdType ="+idType:ToString()
        stmt     += " order by idFile, idType"
        VAR result := List<XDbResult>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    USING VAR rdr := oCmd:ExecuteReader()
                    DO WHILE rdr:Read() .and. result:Count < XEditorSettings.MaxCompletionEntries
                        result:Add(CreateMemberInfo(rdr))
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY

            END LOCK
        ENDIF
        Log(i"GetMembers '{idType}' returns {result.Count} matches")
        RETURN result


    STATIC METHOD HasRCFiles(projectFile as string) AS LOGIC
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    var fileType := (Int) XFileType.NativeResource
                    var stmt := "select count(*) from ProjectFiles where fileType = "+fileType:ToString()+" and ProjectFileName like '%" +projectFile+"'"
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    var count := (Int64) oCmd:ExecuteScalar()
                    RETURN count > 0
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY
            END LOCK
        ENDIF
        RETURN FALSE

    STATIC METHOD GetStartupClasses(projectFile as string) AS List<String>
        var result := List<String>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    var stmt := "select TypeName, Name, Attributes, Kind from ProjectMembers where Name like 'Start%' and ProjectFileName like '%" +projectFile+"'"
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    USING VAR rdr := oCmd:ExecuteReader()
                    DO WHILE rdr:Read()
                        var atts := (Modifiers) (INT64) rdr["Attributes"]
                        var kind := (Kind) (INT64) rdr["Kind"]
                        var m := rdr["Name"]:ToString()
                        if m:Trim():ToLower() == "start" .and. atts:HasFlag(Modifiers.Static) .and. kind:HasFlag(Kind.Method)
                            result:Add(rdr["TypeName"]:ToString())
                        endif
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY

            END LOCK
        ENDIF
        return result

    STATIC METHOD GetMembers(idTypes AS STRING) AS IList<XDbResult>
        VAR stmt := "Select * from ProjectMembers where IdType IN ("+idTypes+") "
        stmt     += " order by idFile, idType"
        VAR result := List<XDbResult>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{stmt, oConn}
                    USING VAR rdr := oCmd:ExecuteReader()
                    DO WHILE rdr:Read() .and. result:Count < XEditorSettings.MaxCompletionEntries
                        result:Add(CreateMemberInfo(rdr))
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY

            END LOCK
        ENDIF
        Log(i"GetMembers '{idTypes}' returns {result.Count} matches")
        RETURN result


    STATIC METHOD GetFunctions( sFileId AS STRING) AS IList<XDbResult>
        //
        VAR result := List<XDbResult>{}
        IF IsDbOpen
            BEGIN LOCK oConn
                TRY
                    USING VAR oCmd := SQLiteCommand{"SELECT 1", oConn}
                    oCmd:CommandText := "SELECT * FROM ProjectMembers WHERE IdFile = $idfile AND TypeName = $typename " + ;
                        " AND Kind in ($kind1, $kind2, $kind3, $kind4, $kind5)"
                    oCmd:Parameters:AddWithValue("$idfile", sFileId)
                    oCmd:Parameters:AddWithValue("$kind1", (INT) Kind.Function)
                    oCmd:Parameters:AddWithValue("$kind2", (INT) Kind.Procedure)
                    oCmd:Parameters:AddWithValue("$kind3", (INT) Kind.VODLL)
                    oCmd:Parameters:AddWithValue("$kind4", (INT) Kind.VOGlobal)
                    oCmd:Parameters:AddWithValue("$kind5", (INT) Kind.VODefine)
                    oCmd:Parameters:AddWithValue("$typename", XLiterals.GlobalName)
                    USING VAR rdr := oCmd:ExecuteReader()
                    DO WHILE rdr:Read() .and. result:Count < XEditorSettings.MaxCompletionEntries
                        result:Add(CreateMemberInfo(rdr))
                    ENDDO
                CATCH e AS Exception
                    XSettings.LogException(e, __FUNCTION__)
                END TRY
            END LOCK
        ENDIF
        Log(i"GetFunctions returns {result.Count} matches")
        RETURN result



    STATIC METHOD CreateTypeInfo(rdr AS SQLiteDataReader) AS XDbResult
        VAR res := XDbResult{}
        res:TypeName     := DbToString(rdr["Name"])
        res:Namespace    := DbToString(rdr["NameSpace"])
        res:BaseTypeName := DbToString(rdr["BaseTypeName"])
        res:Kind         := (Kind) (INT64) rdr["Kind"]
        res:ClassType    := DbToInt( rdr["ClassType"])
        res:Attributes   := (Modifiers) (INT64) rdr["Attributes"]
        res:FileName     := DbToString(rdr["FileName"])
        res:Project      := DbToString(rdr["ProjectFileName"])
        res:StartLine    := DbToInt(rdr["StartLine"])
        res:StartColumn  := DbToInt(rdr["StartColumn"])
        res:EndLine      := DbToInt(rdr["EndLine"])
        res:EndColumn    := DbToInt(rdr["EndColumn"])
        res:Start        := DbToInt(rdr["Start"])
        res:Stop         := DbToInt(rdr["Stop"])
        res:SourceCode   := DbToString(rdr["SourceCode"])
        res:XmlComments  := DbToString(rdr["XmlComments"])
        res:IdType       := (INT64) rdr["Id"]
        res:IdFile       := (INT64) rdr["IdFile"]
        res:IdProject    := (INT64) rdr["IdProject"]
        RETURN res

    STATIC METHOD CreateRefTypeInfo(rdr AS SQLiteDataReader) AS XDbResult
        VAR res := XDbResult{}
        res:TypeName     := DbToString(rdr["Name"])
        res:Namespace    := DbToString(rdr["NameSpace"])
        res:Kind         := (Kind) (INT64) rdr["Kind"]
        res:Attributes   := (Modifiers) (INT64) rdr["Attributes"]
        res:FullName     := DbToString(rdr["FullName"])
        res:Assembly     := DbToString(rdr["AssemblyFileName"])
        res:IdType       := (INT64) rdr["Id"]
        res:IdAssembly   := (INT64) rdr["IdAssembly"]
        RETURN res

    STATIC METHOD CreateCommentTask(rdr AS SQLiteDataReader) AS XDbResult
        VAR res := XDbResult{}
        res:Line         := DbToInt(rdr["Line"])
        res:Column       := DbToInt(rdr["Column"])
        res:Priority     := DbToInt(rdr["Priority"])
        res:Comment      := DbToString(rdr["Comment"])
        res:FileName     := DbToString(rdr["FileName"])
        RETURN res


    STATIC METHOD CreateMemberInfo(rdr AS SQLiteDataReader) AS XDbResult
        VAR res := XDbResult{}
        res:TypeName     := DbToString(rdr["TypeName"])
        res:MemberName   := DbToString(rdr["Name"])
        res:ClassType    := DbToInt( rdr["ClassType"])
        res:Kind         := (Kind) (INT64) rdr["Kind"]
        res:Attributes   := (Modifiers) (INT64) rdr["Attributes"]
        res:FileName     := DbToString(rdr["FileName"])
        res:Project      := DbToString(rdr["ProjectFileName"])
        res:StartLine    := DbToInt(rdr["StartLine"])
        res:StartColumn  := DbToInt(rdr["StartColumn"])
        res:EndLine      := DbToInt(rdr["EndLine"])
        res:EndColumn    := DbToInt(rdr["EndColumn"])
        res:Start        := DbToInt(rdr["Start"])
        res:Stop         := DbToInt(rdr["Stop"])
        res:SourceCode   := DbToString(rdr["SourceCode"])
        res:XmlComments  := DbToString(rdr["XmlComments"])
        res:IdType       := (INT64) rdr["IdType"]
        res:IdFile       := (INT64) rdr["IdFile"]
        res:IdProject    := (INT64) rdr["IdProject"]
        res:ReturnType   := DbToString(rdr["ReturnType"])
        RETURN res

    STATIC METHOD CreateAssemblyMemberInfo(rdr AS SQLiteDataReader) AS XDbResult
        VAR res := XDbResult{}
        res:MemberName   := DbToString(rdr["Name"])
        res:Kind         := (Kind) (INT64) rdr["Kind"]
        res:Attributes   := (Modifiers) (INT64) rdr["Attributes"]
        res:FullName     := DbToString(rdr["FullName"])
        res:Assembly     := DbToString(rdr["AssemblyFileName"])
        res:IdAssembly   := (INT64) rdr["IdAssembly"]
        res:SourceCode   := DbToString(rdr["SourceCode"])
        res:ReturnType   := DbToString(rdr["ReturnType"])
        RETURN res

    STATIC METHOD DbToString(oValue AS OBJECT) AS STRING
        IF oValue == DBNull.Value
            RETURN ""
        ENDIF
        IF oValue IS STRING
            RETURN (STRING) oValue
        ENDIF
        RETURN ""
    STATIC METHOD DbToInt(oValue AS OBJECT) AS LONG
        IF oValue == DBNull.Value
            RETURN 0
        ENDIF
        IF oValue IS INT64 VAR i64
            RETURN (INT) i64
        ENDIF
        IF oValue IS LONG VAR l
            RETURN l
        ENDIF
        RETURN 0


    STATIC PROPERTY IsDbOpen AS LOGIC GET oConn != NULL_OBJECT .AND.  oConn:State == ConnectionState.Open
    STATIC PROPERTY Connection as SQLiteConnection GET oConn

    STATIC METHOD Log(cMessage AS STRING) AS VOID
        IF XSettings.EnableDatabaseLog .AND. XSettings.EnableLogging
            XSolution.WriteOutputMessage("XDatabase: "+cMessage)
        ENDIF
        RETURN

END CLASS

END NAMESPACE
