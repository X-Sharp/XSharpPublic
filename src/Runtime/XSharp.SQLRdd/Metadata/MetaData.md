# Retrieving Metadata
The RDD needs information to decide what to do when opening 
a table or how the update command needs to be generated when the information in the local result set was changed.
This metadata needs to be provided by the application.

The X# SQL RDD supports the following ways to provide this metadata:
- From an INI file
- From tables in the SQL Database
- From a callback function in the app

Each SqlDbConnection has a MetadataProvider property. 
This is used by the RDD to query the information.

The default MetadataProvider is the the IniMetaDataProvider class, that reads the information from an INI file.









