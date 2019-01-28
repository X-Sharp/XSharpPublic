//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
FUNCTION ClassCreate(cClassName AS STRING, aSuperClasses:= NULL_ARRAY AS ARRAY, aMember:= NULL_ARRAY AS ARRAY, aMethod:= NULL_ARRAY AS ARRAY) AS USUAL
    Throw NotImplementedException{}


FUNCTION ClassObject(cClassName as STRING) AS OBJECT
    Throw NotImplementedException{}


FUNCTION ClassDestroy(cName as STRING) AS LOGIC
    Throw NotImplementedException{}

FUNCTION ClassDestroy(oObject as OBJECT) AS LOGIC
    Throw NotImplementedException{}
