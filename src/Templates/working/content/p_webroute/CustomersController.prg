// UsersController
using System.Threading.Tasks
using Microsoft.AspNetCore.Mvc
using XSharp.RDD
using XSharp.RDD.Support
using System.Collections.Generic

[ApiController];
[Route("api/[controller]")];
public class CustomersController inherit ControllerBase
	PUBLIC CONSTRUCTOR()

    [HttpGet];
	public async method GetCustomers() as Task<IActionResult>
        //
		VAR message := "GetCustomers called."
        Console.WriteLine( message )
		//
        VAR oneCustomer := Customer{}
        oneCustomer:FirstName := "Fabrice"
        oneCustomer:LastName := "FORAY"
        oneCustomer:Zip := "07200"
        oneCustomer:City := "Lachapelle"
        //
		VAR customers := List<Customer>{}
        customers:add(oneCustomer)
		return Ok(customers)
	END METHOD

    [HttpPost];
    public async method PostCustomers([FromBody] custs AS List<Customer> ) as Task<IActionResult>
        //
        VAR message := "PostCustomers called with values: " + Environment.NewLine
        FOREACH VAR cust IN custs
            message += cust:FirstName + " " + cust:LastName + Environment.NewLine
        NEXT
        Console.WriteLine( message )
		return Ok( message )
	END METHOD

    [HttpPost];
    [Route("/api/Customer")];
    public async method SingleCustomer([FromBody] cust AS Customer ) as Task<IActionResult>
        //
        VAR message := "Single Customer called with value: " + cust:FirstName + " " + cust:LastName
        Console.WriteLine( message )
		return Ok( message )
	END METHOD

end class
