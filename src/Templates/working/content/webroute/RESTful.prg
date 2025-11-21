// Program
using System.Linq
using System.Runtime.CompilerServices
using Microsoft.AspNetCore.Builder
using Microsoft.AspNetCore.Cors.Infrastructure
using Microsoft.Extensions.DependencyInjection
using System.Linq

CLASS RESTful

	METHOD Run( args AS STRING[] ) AS VOID
        //
		local builder as WebApplicationBuilder
		local app as WebApplication
		//
		builder := WebApplication.CreateBuilder(args)
		// To Convert property names to lower case
		builder:Services:AddControllers():AddJsonOptions( { opts AS Microsoft.AspNetCore.Mvc.JsonOptions =>
			opts:JsonSerializerOptions:PropertyNamingPolicy := LowerCaseNamingPolicy{}
			opts:JsonSerializerOptions:DictionaryKeyPolicy := LowerCaseNamingPolicy{}
			opts:JsonSerializerOptions:WriteIndented := true
		} )
		// Add CORS policy, to allow all origins, methods and headers
		// Ok in Development, but not for Production
		builder:Services:AddCors( { opts AS CorsOptions  =>
			opts:AddPolicy("AllowAll", { policy AS CorsPolicyBuilder =>
				policy:AllowAnyOrigin();
					:AllowAnyMethod();
					:AllowAnyHeader()
			})
		})
		// Build the app
		app := builder:Build()
		// Use CORS
		app:UseCors("AllowAll")
		app:UseRouting()
		app:MapControllers()
		app:Run()

    END METHOD

END CLASS