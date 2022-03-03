//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


FUNCTION _cmdDirMake(cDir as STRING) AS VOID
    System.IO.Directory.CreateDirectory(cDir)


FUNCTION _cmdDirChange(cDir as STRING) AS VOID
    System.IO.Directory.SetCurrentDirectory(cDir)

FUNCTION _cmdDirRemove(cDir as STRING) AS VOID
    System.IO.Directory.Delete(cDir,false)


