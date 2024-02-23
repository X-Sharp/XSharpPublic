//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using XSharp.RDD.SqlRDD
using XSharp.RDD.SqlRDD.Providers

partial static class XSharp.RDD.SqlRDD.Functions
/// <summary>
/// Set the default ISqlDbProvider to use for the SqlDb Connections. The default SQLDbProvider is ODBC
/// </summary>
/// <param name="ProviderName">Name of the provider to use. This is case insensitive
/// Built in providers are: <br/>
/// <see cref="T:XSharp.RDD.SqlRDD.Providers.SqlDbProviderMySql">MySql</see><br/>
/// <see cref="T:XSharp.RDD.SqlRDD.Providers.SqlDbProviderODBC">ODBC</see><br/>
/// <see cref="T:XSharp.RDD.SqlRDD.Providers.SqlDbProviderOleDb">OleDb</see><br/>
/// <see cref="T:XSharp.RDD.SqlRDD.Providers.SqlDbProviderOracle">Oracle</see><br/>
/// <see cref="T:XSharp.RDD.SqlRDD.Providers.SqlDbProviderPostgresSql">PostgresSql</see><br/>
/// <see cref="T:XSharp.RDD.SqlRDD.Providers.SqlDbProviderSqlServer">SqlServer</see><br/>
/// </param>
/// <returns>TRUE when a provider was found matching the name.</returns>
/// <include file="SqlDbExamples.xml" path="doc/NorthWind1/*" />
static method SqlDbSetProvider(ProviderName as string) as logic
    SqlDbProvider.SetDefaultProvider(ProviderName)
    return SqlDbProvider.Current != null
end method


/// <summary>
/// Retrieve the default SqlDbProvider object
/// </summary>
/// <returns>The SqlDbProvider object or NULL when the ProviderName has not been registered.</returns>
static method SqlDbGetProvider() as ISqlDbProvider
    return SqlDbProvider.Current
end method


/// <summary>
/// Register a Custom SqlDbProvider with the SqlRDD system
/// </summary>
/// <param name="ProviderName">Name to use for the provider. </param>
/// <param name="ProviderClass">Type of the class that implements the provider</param>
/// <returns>TRUE when the provider was registered.</returns>
static method SqlDbRegisterProvider(ProviderName as string, ProviderClass as System.Type) as logic
    return SqlDbProvider.RegisterProvider(ProviderName, ProviderClass)
end method


/// <summary>
/// Unregister a Custom SqlDbProvider from the SqlRDD system
/// </summary>
/// <param name="ProviderName">Name of the provider to unregister.</param>
/// <returns>TRUE when the provider was unregistered.</returns>
static method SqlDbUnRegisterProvider(ProviderName as string) as logic
    return SqlDbProvider.UnRegisterProvider(ProviderName)
end method


end class

