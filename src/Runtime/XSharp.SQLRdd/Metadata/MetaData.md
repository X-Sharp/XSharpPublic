# Retrieving Metadata
The RDD needs information to decide what to do when opening 
a table or how the update command needs to be generated when the information in the local result set was changed.
This metadata needs to be provided by the application.

The X# SQL RDD supports the following ways to provide this metadata:
- From an INI file
- From tables in the SQL Database
- From a callback function in the app
- A custom provider created by the app


Each SqlDbConnection has a MetadataProvider property. 
This is used by the RDD to query the information.

The default MetadataProvider is the the IniMetaDataProvider class, that reads the information from an INI file.
The default Ini file name is SQLRDD.INI.
If you create a SqlDbConnection with a callback, then the CallBackDataProvider class is used.

There is also a DatabaseMetadataProvider class that reads the information from tables in the SQL Database.
The provider will create the necessary tables, but you need to fill these yourselve.
The tables involved are:
- xs_tableinfo
- xs_indexinfo

You can also create your own MetadataProvider class by implementing the IMetadataProvider interface, 
or by inheriting from the AbstractMetaDataProvider class. 
You can assign ao object of this class to the MetadataProvider property of the SqlDbConnection.

