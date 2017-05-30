// Dummy.prg
// Created by    : robert
// Creation Date : 5/30/2017 5:55:04 PM
// Created for   : 
// WorkStation   : ZEUS


// dummy functions
function DbInfo(iInfo as INT) as OBJECT
RETURN NULL

function RddInfo(iInfo as INT) as OBJECT
RETURN NULL

function RddInfo(iInfo as INT, oParam as OBJECT) as OBJECT
RETURN NULL

function DBORDERINFO(iInfo as INT) AS OBJECT
RETURN NULL

function DBORDERINFO(iInfo as INT,oParam1 as OBJECT) AS OBJECT
RETURN NULL

function DBORDERINFO(iInfo as INT,oParam1 as OBJECT, oParam2 as OBJECT) AS OBJECT
RETURN NULL


STATIC CLASS RuntimeState
    STATIC PROPERTY Optimize as LOGIC AUTO
    STATIC PROPERTY Deleted  as LOGIC AUTO
    STATIC PROPERTY Exact    as LOGIC AUTO
    STATIC PROPERTY Decimals as WORD AUTO
    STATIC PROPERTY Epoch    as WORD AUTO
    STATIC PROPERTY DateFormat as STRING AUTO
END CLASS