// Program
using System.Linq
using System.Runtime.CompilerServices
using Microsoft.AspNetCore.Builder
using Microsoft.Extensions.DependencyInjection
using System.Linq

class RESTful

	METHOD Run( args AS STRING[] ) AS VOID
        //
		local builder as WebApplicationBuilder
		local app as WebApplication
		// local context as AppDbContext
		//
		builder := WebApplication.CreateBuilder(args)
		// builder:Services:AddDbContext<AppDbContext>({ |options as DbContextOptionsBuilder| ;
		// 	options:UseInMemoryDatabase("UserDb") ;
		// })
		builder:Services:AddControllers()
		app := builder:Build()
		app:UseRouting()
		app:MapControllers()
		app:Run()

    END METHOD


end class