//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

///  <exclude/>
FUNCTION _cmdDirMake(cDir as STRING) AS VOID
    System.IO.Directory.CreateDirectory(cDir)

///  <exclude/>
FUNCTION _cmdDirChange(cDir as STRING) AS VOID
    System.IO.Directory.SetCurrentDirectory(cDir)

///  <exclude/>
FUNCTION _cmdDirRemove(cDir as STRING) AS VOID
    System.IO.Directory.Delete(cDir,false)


