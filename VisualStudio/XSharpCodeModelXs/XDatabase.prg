//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Data.SQLite
USING System.IO
USING System.Data
USING System.Data.Common
USING System.Collections.Generic

BEGIN NAMESPACE XSharpModel
   STATIC CLASS XDatabase
      STATIC PRIVATE oConn   AS SQLiteConnection     // In memory database !
      PRIVATE CONST CurrentDbVersion := 0.1 AS System.Double
      
      STATIC METHOD CreateOrOpenDatabase(cFileName AS STRING) AS VOID
         LOCAL lValid := FALSE AS LOGIC
         LOCAL oDiskDb AS SQLiteConnection
         
         IF System.IO.File.Exists(cFileName)
            oDiskDb := OpenFile(cFileName)
            IF ! ValidateSchema(oDiskDb)
               oDiskDb:Close()
               oDiskDb:Dispose()
               System.IO.File.Delete(cFileName)
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
            oDiskDb:Close()
         ENDIF
         RETURN 
         
      STATIC METHOD SaveDatabase(cFile AS STRING) AS LOGIC
         IF oConn != NULL_OBJECT .AND. oConn:State == ConnectionState.Open
            SaveToDisk(oConn, cFile)
            RETURN TRUE
         ENDIF
         RETURN FALSE
      
      STATIC METHOD CloseDatabase(cFile AS STRING) AS LOGIC
         IF oConn != NULL_OBJECT .AND. oConn:State == ConnectionState.Open
            SaveToDisk(oConn, cFile)
            oConn:Close()
            RETURN TRUE
         ENDIF
         oConn := NULL
         RETURN FALSE
      
      STATIC METHOD SetPragmas(oConn AS SQLiteConnection) AS VOID
         BEGIN LOCK oConn
            BEGIN USING VAR oCmd := SQLiteCommand{"PRAGMA foreign_keys = ON", oConn}
               oCmd:ExecuteNonQuery()
            END USING
         END LOCK
         RETURN
      
      STATIC METHOD OpenFile(cFile AS STRING) AS SQLiteConnection
         VAR db := SQLiteConnection{"Data Source="+cFile+"; Version=3;"}
         db:Open()    
         SetPragmas(db)
         RETURN db
      
      STATIC METHOD SaveToDisk(oConn AS SQLiteConnection, cFile AS STRING) AS VOID
         IF System.IO.File.Exists(cFile)
            System.IO.File.SetAttributes(cFile, FileAttributes.Normal)
            System.IO.File.Delete(cFile)
         ENDIF
         VAR diskdb := OpenFile(cFile)
         oConn:BackupDatabase(diskdb, "main", "main", -1, NULL, 0)
         diskdb:Close()
         RETURN         
      
      STATIC METHOD RestoreFromDisk(oDiskDb AS SQLiteConnection, oConn AS SQLiteConnection) AS VOID
         oDiskDb:BackupDatabase(oConn, "main", "main", -1, NULL, 0)
         RETURN         
         
      
      STATIC METHOD CreateSchema(connection AS SQLiteConnection) AS VOID
         BEGIN LOCK connection
            VAR cmd := SQLiteCommand{"SELECT 1",connection}
            
            #region Drop Existing Tables
            cmd:CommandText := "Drop table if exists Projects"
            cmd:ExecuteNonQuery()		
            cmd:CommandText := "Drop table if exists FilesPerProject"
            cmd:ExecuteNonQuery()		
            cmd:CommandText := "Drop table if exists Files"
            cmd:ExecuteNonQuery()		
            cmd:CommandText := "Drop table if exists Types"
            cmd:ExecuteNonQuery()		
            cmd:CommandText := "Drop table if exists Members"
            cmd:ExecuteNonQuery()		
            cmd:CommandText := "Drop table if exists Kinds"
            cmd:ExecuteNonQuery()		
            cmd:CommandText := "Drop table if exists Assemblies"
            cmd:ExecuteNonQuery()		
            cmd:CommandText := "Drop table if exists ReferencedTypes"
            cmd:ExecuteNonQuery()		
            #endregion
            #region Table Projects
            VAR stmt	:= "Create Table Projects ("
            stmt     	+= " Id integer NOT NULL PRIMARY KEY, ProjectFileName text NOT NULL "
            stmt		+= " )"
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()				
            
            stmt	:= "CREATE INDEX Project_Name on Projects (ProjectFileName) "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()				
            #endregion
            #region Table Files
            stmt  		:= "Create Table Files ("
            stmt     	+= " Id integer NOT NULL PRIMARY KEY, FileName text NOT NULL COLLATE NOCASE,  "
            stmt     	+= " FileType integer NOT NULL, LastChanged DateTime NOT NULL, Size integer"
            stmt		   += " )"
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()		
            
            stmt	:= "CREATE INDEX File_Name on Files (FileName) "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()				
            #endregion
            #region Table FilesPerProject
            
            stmt  	:= 	"Create Table FilesPerProject ("
            stmt	   +=  " idFile integer NOT NULL, idProject integer NOT NULL, " 
            stmt     +=  " PRIMARY KEY (idFile, idProject), " 
            stmt	   +=  " FOREIGN KEY (idFile) 	  REFERENCES Files (Id)    ON DELETE CASCADE ON UPDATE CASCADE, " 
            stmt     +=  " FOREIGN KEY (idProject) REFERENCES Projects (Id) ON DELETE CASCADE ON UPDATE CASCADE "
            stmt	   +=  " )"
            
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()				
            
            stmt	:= "CREATE INDEX FilesPerProject_File on FilesPerProject (idFile) "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()				
            
            stmt	:= "CREATE INDEX FilesPerProject_Project on FilesPerProject (idProject) "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()				
            
            #endregion
            #region Table Types
            
            stmt  	:=  "Create Table Types ("
            stmt	   +=  " Id integer NOT NULL PRIMARY KEY, idFile integer NOT NULL, Name text NOT NULL COLLATE NOCASE, Namespace text NOT NULL COLLATE NOCASE, "
            stmt     +=  " Kind integer NOT NULL, IsPartial BOOLEAN, BaseTypeName text COLLATE NOCASE, Attributes integer NOT NULL, "
            stmt     +=  " StartLine integer , StartColumn integer, EndLine integer , EndColumn integer,   "
            stmt     +=  " Start integer , Stop integer,  "
            stmt     +=  " FOREIGN KEY (idFile) REFERENCES Files (Id) ON DELETE CASCADE ON UPDATE CASCADE"
            stmt	   += ")"
            
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()	
            
            stmt	:= "CREATE INDEX Type_Name on Types (Name) "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()				
            stmt	:= "CREATE INDEX Type_BaseTypeName on Types (BaseTypeName) "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()				
            
            stmt	:= "CREATE INDEX Type_Kind on Types (Kind) "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()				
            
            #endregion
            #region Table Members
            
            stmt  	:=  "Create Table Members ("
            stmt	   +=  " Id integer NOT NULL PRIMARY KEY, IdType integer NOT NULL , IdFile integer NOT NULL, "
            stmt	   +=  " Name text COLLATE NOCASE, Kind integer , Attributes integer , "
            stmt	   +=  " Value text, ReturnType text, StartLine integer , StartColumn integer ,  "
            stmt	   +=  " EndLine integer , EndColumn integer , Start integer , Stop integer , "
            stmt	   +=  " FOREIGN KEY (idType) REFERENCES Types (Id) ON DELETE CASCADE ON UPDATE CASCADE, " 
            stmt     +=  " FOREIGN KEY (idFile) REFERENCES Files (Id) ON DELETE CASCADE ON UPDATE CASCADE"
            stmt	   += ")"
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()		
            
            
            stmt	:= "CREATE INDEX Member_Name on Members (Name) "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()		
            
            stmt	:= "CREATE INDEX Member_Type on Members (idType) "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()		
            
            
            stmt	:= "CREATE INDEX Member_File on Members (idFile) "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()		
            
            stmt	:= "CREATE INDEX Member_Kind on Members (Kind) "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()		
            
            #endregion
            #region Table Kinds
            
            
            stmt  	:= 	"Create Table Kinds ("
            stmt	   += 	" Id integer NOT NULL PRIMARY KEY, Kind text not null "
            stmt	   +=  ")"
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()		
            cmd:Parameters:Clear()
            cmd:Parameters:AddWithValue("$id",0)
            cmd:Parameters:AddWithValue("$kind","")
            cmd:CommandText := "INSERT INTO Kinds (Id, Kind) values ($id, $kind)"
            
            FOREACH VAR evalue IN Enum.GetValues(typeof(Kind))
               cmd:Parameters[0]:Value := evalue
               cmd:Parameters[1]:Value := Enum.GetName(typeof(Kind), evalue)
               cmd:ExecuteNonQuery()		
            NEXT
            #endregion
            
            
            #region Table Assemblies
            stmt  	:=  "Create Table Assemblies ("
            stmt	   +=  " Id integer NOT NULL PRIMARY KEY, Name text NOT NULL COLLATE NOCASE, AssemblyFileName text NOT NULL COLLATE NOCASE, "
            stmt     +=  " LastChanged DateTime NOT NULL, Size integer "
            stmt	   += ")"
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()			
            
            stmt	:= "CREATE INDEX Assemblies_Name on Assemblies (Name) "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()				
            
            stmt	:= "CREATE INDEX Assemblies_FileName on Assemblies (AssemblyFileName) "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()				
            
            #endregion
            #region Table ReferencedTypes
            stmt  	:=  "Create Table ReferencedTypes ("
            stmt	   +=  " Id integer NOT NULL PRIMARY KEY, idAssembly integer NOT NULL, Name text NOT NULL COLLATE NOCASE, Namespace text NOT NULL COLLATE NOCASE, "
            stmt     +=  " Kind integer NOT NULL, BaseTypeName text COLLATE NOCASE, Attributes integer NOT NULL, "
            stmt     +=  " FOREIGN KEY (idAssembly) REFERENCES Assemblies (Id) ON DELETE CASCADE ON UPDATE CASCADE"
            stmt	   += ")"
            
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()	
            
            stmt	:= "CREATE INDEX ReferencedTypes_Name on ReferencedTypes (Name) "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()				
            
            stmt	:= "CREATE INDEX ReferencedTypes_BaseTypeName on ReferencedTypes (BaseTypeName) "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()				
            
            stmt	:= "CREATE INDEX ReferencedTypes_Kind on ReferencedTypes (Kind) "
            cmd:CommandText := stmt
            cmd:ExecuteNonQuery()		         
            #endregion
            
            
            #region views
            stmt := " CREATE VIEW ProjectFiles AS SELECT f.*, fp.IdProject, fp.IdFile, p.ProjectFileName " + ;
                     " FROM Files f JOIN FilesPerProject fp ON f.Id = fp.IdFile JOIN Projects p ON fp.IdProject = P.Id"
            cmd:CommandText := stmt
            cmd:Parameters:Clear()
            cmd:ExecuteNonQuery()		
            
            
            stmt := "CREATE VIEW TypeMembers AS SELECT m.*, t.Name AS TypeName, t.Namespace, t.BaseTypeName " + ;
                     "FROM members m JOIN Types t ON m.IdType = t.Id"
            cmd:CommandText := stmt
            cmd:Parameters:Clear()
            cmd:ExecuteNonQuery()		

            stmt := "CREATE VIEW ProjectTypes AS SELECT t.*, p.IdProject, p.ProjectFileName " +;
                     " FROM Types t  JOIN ProjectFiles p ON t.IdFile = p.IdFile "
            cmd:CommandText := stmt
            cmd:Parameters:Clear()
            cmd:ExecuteNonQuery()		

            
            stmt := "CREATE VIEW ProjectMembers AS SELECT m.*, p.IdProject, p.ProjectFileName " +;
                     " FROM TypeMembers m  JOIN ProjectFiles p ON m.IdFile = p.IdFile "
            cmd:CommandText := stmt
            cmd:Parameters:Clear()
            cmd:ExecuteNonQuery()		

            stmt := "CREATE VIEW AssemblyTypes AS SELECT t.*, t.IdAssembly, a.AssemblyFileName " +;
                     " FROM ReferencedTypes t  JOIN Assemblies a ON t.IdAssembly = a.Id "
            cmd:CommandText := stmt
            cmd:Parameters:Clear()
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
               BEGIN USING VAR cmd  := SQLiteCommand{"SELECT 1", connection}
                  VAR stmt := "SELECT count(name) from sqlite_master WHERE type='table' AND name=$table"
                  cmd:CommandText := stmt
                  VAR tables := <STRING> {"Projects","FilesPerProject","Files", "Types", "Members", "Db_Version","Assembly", "ReferenceType"}
                  FOREACH VAR table IN tables    
                     cmd:Parameters:Clear()
                     cmd:Parameters:AddWithValue("$table",table)
                     VAR num := (INT64) cmd:ExecuteScalar()
                     IF num != 1
                        lOk := FALSE
                        EXIT
                     ENDIF
                  NEXT
                  cmd:CommandText := "SELECT Max(Version) from Db_version"
                  VAR vers := (System.Double) cmd:ExecuteScalar()
                  IF vers != CurrentDbVersion
                     lOk := FALSE
                  ENDIF			
               END USING
               EXIT
            ENDDO		
         END LOCK
         RETURN lOk	         
         
         
      
      STATIC METHOD GetProjectNames() AS List<STRING>
         VAR result := List<STRING>{}
         BEGIN LOCK oConn
            BEGIN USING VAR cmd := SQLiteCommand{"SELECT ProjectFileName from Projects", oConn}
               BEGIN USING VAR rdr := cmd:ExecuteReader()
                  DO WHILE rdr:Read()
                     VAR name := rdr:GetString(0)
                     result:Add(name)
                  ENDDO
               END USING
            END USING
         END LOCK         
         RETURN result
         
         #region CRUD projects      
         STATIC METHOD Read(oProject AS XProject) AS VOID
            IF String.IsNullOrEmpty(oProject:FileName)
               RETURN
            ENDIF
            VAR file    := oProject:FileName
            BEGIN LOCK oConn
               BEGIN USING VAR cmd := SQLiteCommand{"", oConn}
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
                  ENDIF
               END USING 
            END LOCK
            RETURN
         
         STATIC METHOD DeleteProject(cFileName AS STRING) AS VOID
            IF String.IsNullOrEmpty(cFileName)
               RETURN
            ENDIF
            VAR file := cFileName
            BEGIN LOCK oConn
               BEGIN USING VAR cmd := SQLiteCommand{"", oConn}
                  cmd:CommandText := "delete from Projects where FileName = $file" 
                  cmd:Parameters:AddWithValue("$file",file)
                  cmd:ExecuteNonQuery()
               END USING
            END LOCK
         #endregion        
         
         #region CRUD files
         
         STATIC METHOD DeleteFile(cFileName AS STRING) AS VOID
            IF String.IsNullOrEmpty(cFileName)
               RETURN
            ENDIF
            VAR file    := cFileName
            BEGIN LOCK oConn
               BEGIN USING VAR cmd := SQLiteCommand{"", oConn}
                  cmd:CommandText := "DELETE FROM Files WHERE FileName = $file" 
                  cmd:Parameters:AddWithValue("$file",file)
                  cmd:ExecuteNonQuery()
               END USING
            END LOCK
         
         STATIC METHOD Read(oFile AS XFile) AS VOID
            IF String.IsNullOrEmpty(oFile:FullPath)
               RETURN
            ENDIF
            VAR file    := oFile:FullPath
            /*
            "Create Table Files ("
            " Id integer NOT NULL PRIMARY KEY, FileName text NOT NULL COLLATE NOCASE "
            " FileType integer NOT NULL, LastChanged DateTime NOT NULL "
            " )"
            
            "Create Table FilesPerProject ("
            " idFile integer NOT NULL, idProject integer NOT NULL, " 
            " PRIMARY KEY (idFile, idProject), " 
            " FOREIGN KEY (idFile) 	  REFERENCES Files (Id)    ON DELETE CASCADE ON UPDATE CASCADE, " 
            " FOREIGN KEY (idProject) REFERENCES Projects (Id) ON DELETE CASCADE ON UPDATE CASCADE "
            " )"
            
            
            */
            
            BEGIN LOCK oConn
               BEGIN USING VAR cmd := SQLiteCommand{"", oConn}
                  cmd:CommandText := "SELECT Id, LastChanged, Size FROM Files WHERE FileName = $file"
                  cmd:Parameters:AddWithValue("$file",file)
                  VAR lOk := FALSE 
                  BEGIN USING VAR rdr := cmd:ExecuteReader()
                     IF rdr:Read()
                        oFile:Id          := rdr:GetInt64(0)
                        oFile:LastChanged := rdr:GetDateTime(1)
                        oFile:Size        := rdr:GetInt64(2)
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
               END USING 
            END LOCK
            RETURN
         
         STATIC PRIVATE METHOD UpdateFileData(oFile AS XFile) AS VOID
            BEGIN LOCK oConn
               BEGIN USING VAR oCmd := SQLiteCommand{"SELECT 1", oConn}
                  oCmd:CommandText := "UPDATE Files SET LastChanged = $last, Size = $size WHERE id = "+oFile:Id:ToString()
                  VAR fi            := FileInfo{oFile:FullPath}
                  oFile:LastChanged := fi:LastWriteTime
                  oFile:Size        := fi:Length
                  oCmd:Parameters:Clear()
                  oCmd:Parameters:AddWithValue("$last", oFile:LastChanged)
                  oCmd:Parameters:AddWithValue("$size", oFile:Size)
                  oCmd:ExecuteNonQuery()
               END USING
            END LOCK
            RETURN
         
         STATIC METHOD Update(oFile AS XFile) AS VOID
            // determine File ID
            IF oFile:Id == -1
               XDatabase.Read(oFile)
            ENDIF
            IF oFile:IsSource
               UpdateFileContents(oFile)
            ENDIF
            UpdateFileData(oFile)
            RETURN
            
         
         STATIC PRIVATE METHOD UpdateFileContents(oFile AS XFile) AS VOID
            /*
            "Create Table Types ("
            " Id integer NOT NULL PRIMARY KEY, idFile integer NOT NULL, Name text NOT NULL COLLATE NOCASE, Namespace text NOT NULL COLLATE NOCASE, "
            " Kind integer NOT NULL, IsPartial BOOLEAN, BaseTypeName text COLLATE NOCASE, Attributes integer NOT NULL, "
            " Interfaces text , TypeParameters text, TypeConstraints text, "
            " StartLine integer , StartColumn integer, EndLine integer , EndColumn integer,   "
            " Start integer , Stop integer,  "
            " FOREIGN KEY (idFile) REFERENCES Files (Id) ON DELETE CASCADE ON UPDATE CASCADE"
            ")"
            */
            BEGIN LOCK oConn
               BEGIN USING VAR oCmd := SQLiteCommand{"DELETE FROM Members WHERE IdFile = "+oFile:Id:ToString(), oConn}
                  oCmd:ExecuteNonQuery()
                  oCmd:CommandText  := "DELETE FROM Types WHERE IdFile = "+oFile:Id:ToString()
                  oCmd:ExecuteNonQuery()
                  oCmd:CommandText := "INSERT INTO Types (Name, IdFile, Namespace, Kind,  IsPartial,  BaseTypeName, " + ;
                  " Attributes,  StartLine,  StartColumn,  EndLine,  EndColumn,  Start,  Stop) " +;
                  "         VALUES ($name, $file, $namespace, $kind, $isPartial, $baseTypeName,  " +;
                  "  $attributes, $startline, $startcolumn, $endline, $endcolumn, $start, $stop) ;" +;
                  " SELECT last_insert_rowid()"
                  VAR pars := List<SQLiteParameter>{} { ;
                  oCmd:Parameters:AddWithValue("$name", ""),;
                  oCmd:Parameters:AddWithValue("$file", 0),;
                  oCmd:Parameters:AddWithValue("$namespace", ""),;
                  oCmd:Parameters:AddWithValue("$kind", 0),;
                  oCmd:Parameters:AddWithValue("$isPartial", FALSE),;
                  oCmd:Parameters:AddWithValue("$baseTypeName", ""),;
                  oCmd:Parameters:AddWithValue("$attributes", 0),;
                  oCmd:Parameters:AddWithValue("$startline", 0),;
                  oCmd:Parameters:AddWithValue("$startcolumn", 0),;
                  oCmd:Parameters:AddWithValue("$endline", 0),;
                  oCmd:Parameters:AddWithValue("$endcolumn", 0),;
                  oCmd:Parameters:AddWithValue("$start", 0),;
                  oCmd:Parameters:AddWithValue("$stop", 0)}
                  FOREACH VAR typedef IN oFile:TypeList:Values
                     TRY
                        pars[0]:Value := typedef:Name
                        pars[1]:Value := oFile:Id
                        pars[2]:Value := typedef:Namespace
                        pars[3]:Value := (INT) typedef:Kind
                        pars[4]:Value := typedef:IsPartial
                        pars[5]:Value := typedef:BaseType
                        pars[6]:Value := (INT) typedef:Attributes
                        pars[7]:Value := typedef:Range:StartLine
                        pars[8]:Value := typedef:Range:StartColumn
                        pars[9]:Value := typedef:Range:EndLine
                        pars[10]:Value := typedef:Range:EndColumn
                        pars[11]:Value := typedef:Interval:Start
                        pars[12]:Value := typedef:Interval:Stop
                        VAR id := (INT64) oCmd:ExecuteScalar()
                        typedef:Id := id
                     CATCH e AS Exception
                        XSolution.WriteOutputMessage("Exception: "+e:ToString())
                        XSolution.WriteOutputMessage("File   : "+oFile:FullPath+" "+oFile:Id:ToString())
                        XSolution.WriteOutputMessage("Typedef: "+typedef:Name)
                     END TRY
                  NEXT
                  /*
                  "Create Table Members ("
                  " Id integer NOT NULL PRIMARY KEY, IdType integer NOT NULL , IdFile integer NOT NULL, "
                  " Name text COLLATE NOCASE, Kind integer , Attributes integer , "
                  " Value text, ReturnType text, Parameters text, StartLine integer , StartColumn integer ,  "
                  " EndLine integer , EndColumn integer , Start integer , Stop integer , "
                  " FOREIGN KEY (idType) REFERENCES Types (Id) ON DELETE CASCADE ON UPDATE CASCADE, " 
                  " FOREIGN KEY (idFile) REFERENCES Files (Id) ON DELETE CASCADE ON UPDATE CASCADE"
                  ")"
                  */
                  oCmd:CommandText := "INSERT INTO Members (idFile, idType, Name, Kind, Attributes,  " + ;
                  " StartLine, StartColumn, EndLine, EndColumn, Start, Stop) " +;
                  " VALUES ($file, $type, $name, $kind, $attributes,  " + ;
                  " $startline, $startcolumn, $endline, $endcolumn, $start, $stop) ;" +;
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
                  oCmd:Parameters:AddWithValue("$stop", 0)}
                  VAR list := List<XMemberDefinition>{}
                  FOREACH VAR typedef IN oFile:TypeList:Values
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
                           VAR id := (INT64) oCmd:ExecuteScalar()
                           xmember:Id := id
                        CATCH e AS Exception
                           XSolution.WriteOutputMessage("Exception: "+e:ToString())
                           XSolution.WriteOutputMessage("File   : "+oFile:FullPath+" "+oFile:Id:ToString())
                           XSolution.WriteOutputMessage("Typedef: "+typedef:Name+" "+typedef:Id:ToString())
                           XSolution.WriteOutputMessage("Member : "+xmember:Name)
                           XSolution.WriteOutputMessage("Line :   "+xmember:Range:StartLine:ToString())
                           XSolution.WriteOutputMessage("Column : "+xmember:Range:StartColumn:ToString())
                        END TRY
                     NEXT
                  NEXT
               END USING
            END LOCK
            RETURN
            
         #endregion
         
         
      #region CRUD Assemblies
      STATIC METHOD Read(oAssembly AS XAssembly) AS VOID
         IF String.IsNullOrEmpty(oAssembly:FullName) .OR. String.IsNullOrEmpty(oAssembly:FileName)
            RETURN
         ENDIF
         /*
         stmt  	:=  "Create Table Assemblies ("
         stmt	   +=  " Id integer NOT NULL PRIMARY KEY, Name text NOT NULL COLLATE NOCASE, FileName text NOT NULL COLLATE NOCASE, "
         stmt     +=  " LastChanged DateTime NOT NULL, Size integer "
         stmt	   += ")"
         */
         VAR file    := oAssembly:FileName
         VAR name    := oAssembly:FullName
         BEGIN LOCK oConn
            BEGIN USING VAR cmd := SQLiteCommand{"", oConn}
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
               ENDIF
            END USING 
         END LOCK
         RETURN
      
      STATIC METHOD Update(oAssembly AS XAssembly) AS VOID
         IF String.IsNullOrEmpty(oAssembly:FullName) .OR. String.IsNullOrEmpty(oAssembly:FileName)
            RETURN
         ENDIF   
         BEGIN LOCK oConn
            BEGIN USING VAR oCmd := SQLiteCommand{"SELECT 1", oConn}
               // Updated TypeReferences
               //	   "Create Table ReferencedTypes ("
               //	   " Id integer NOT NULL PRIMARY KEY, idAssembly integer NOT NULL, Name text NOT NULL COLLATE NOCASE, Namespace text NOT NULL COLLATE NOCASE, "
               //	   " Kind integer NOT NULL, BaseTypeName text COLLATE NOCASE, Attributes integer NOT NULL, "
               //	   " FOREIGN KEY (idAssembly) REFERENCES Assemblies (Id) ON DELETE CASCADE ON UPDATE CASCADE"
               //	  ")"
               oCmd:CommandText := "INSERT INTO ReferencedTypes (idAssembly, Name, Namespace, Kind, BaseTypeName, Attributes) " + ;
               " values ($id, $name, $namespace, $kind, $basetypename, $attributes) "
               oCmd:Parameters:Clear()
               VAR pars := List<SQLiteParameter>{} { ;
               oCmd:Parameters:AddWithValue("$id", 0),;
               oCmd:Parameters:AddWithValue("$name", ""),;
               oCmd:Parameters:AddWithValue("$namespace", ""),;
               oCmd:Parameters:AddWithValue("$kind", 0),;
               oCmd:Parameters:AddWithValue("$basetypename", ""),;
               oCmd:Parameters:AddWithValue("$attributes",0)}
               FOREACH VAR typeref IN oAssembly:TypeList:Values
                  pars[0]:Value := oAssembly:Id
                  pars[1]:Value := typeref:Name
                  pars[2]:Value := typeref:Namespace
                  pars[3]:Value := (INT) typeref:Kind
                  pars[4]:Value := typeref:BaseType
                  pars[5]:Value := (INT) typeref:Attributes
                  oCmd:ExecuteNonQuery()
               NEXT
               
               oCmd:CommandText := "Update Assemblies set LastChanged = $last, Size = $size where id = "+oAssembly:Id:ToString()
               VAR fi            := FileInfo{oAssembly:FileName}
               oAssembly:LastChanged := fi:LastWriteTime
               oAssembly:Size        := fi:Length
               oCmd:Parameters:Clear()
               oCmd:Parameters:AddWithValue("$last", oAssembly:LastChanged)
               oCmd:Parameters:AddWithValue("$size", oAssembly:Size)
               oCmd:ExecuteNonQuery()
            END USING
         END LOCK         
         RETURN
      #endregion
      
      STATIC METHOD FindFunction(sName AS STRING, sProjectIds AS STRING, sAssemblyIDs AS STRING, functionClasses AS List<STRING>) AS IList<XDbResult>
         // search class members in the Types list
         VAR whereclause := "name = $name AND TypeName = $typename " + ;
         " AND Kind in ($kind1, $kind2, $kind3) AND IdProject in ("+sProjectIds+")"
         
         VAR result := List<XDbResult>{}
         BEGIN USING VAR oCmd := SQLiteCommand{"SELECT 1", oConn}
            VAR stmt := XMemberDefinition.DbSelectClause
            oCmd:CommandText := stmt:Replace("%whereclause%", whereclause)
            oCmd:Parameters:AddWithValue("$name", sName)
            oCmd:Parameters:AddWithValue("$kind1", (INT) Kind.Function)
            oCmd:Parameters:AddWithValue("$kind2", (INT) Kind.Procedure)
            oCmd:Parameters:AddWithValue("$kind3", (INT) Kind.Method)
            oCmd:Parameters:AddWithValue("$typename", XLiterals.GlobalName)
            BEGIN USING VAR rdr := oCmd:ExecuteReader()
               DO WHILE rdr:Read()
                  // create XDbResult objects
               ENDDO
            END USING
         END USING
         RETURN result
      
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
      
   END CLASS
   
END NAMESPACE   