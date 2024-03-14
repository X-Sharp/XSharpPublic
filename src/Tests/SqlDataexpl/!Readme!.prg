/*
TEXTBLOCK Readme

/-*

Copyright (C) 1993-1999 Computer Associates International Inc.
Created     : 02/99 Michael Zech
Last Changed: 03/29/99

The purpose of this sample is to show how to build a Dataexplorer by using the new DataListView
and a manual created DataTreeView class. This sample connect two databases with a manual created
relation by orderscope.

Unlike the Explorer sample the most parameters are not hardcoded, so that you can change
the view of the data, when changing some flags and paramters at the initialization of the
Explorer window. For this, please look at the comments in the start method.

Please note that SetDeleted() has no effect in the DataListView. This is the reason,
why it's neccessary to empty the record before delete it.

*-/


*/
