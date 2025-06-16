// UsersController
using System.Threading.Tasks
using Microsoft.AspNetCore.Mvc
using XSharp.RDD
using XSharp.RDD.Support
using System.Collections.Generic

[ApiController];
[Route("api/[controller]")];
public class CustomersController inherit ControllerBase
    private dbInfo AS DbOpenInfo

	public constructor() //(context as AppDbContext )
		//self:_context := context
        dbInfo := DbOpenInfo{ "customer.dbf", "customer", 1, FALSE, FALSE }


	[HttpGet];
	public async method GetCustomers() as Task<IActionResult>
        // open DBF
        VAR oDbfCdx := DbfCdx{}
        oDbfCdx:Open(dbInfo)
        //
        VAR customers := List<Customer>{} 
        DO WHILE !oDbfCdx:EOF
            customers:Add( RecordToCustomer(oDbfCdx) )
            oDbfCdx:Skip(1)
        ENDDO
        //
        oDbfCdx:Close()
		return Ok(customers)
	END METHOD

    [HttpPost];
    public async method PostCustomers([FromBody] cust AS Customer ) as Task<IActionResult>
        // open DBF
        VAR oDbfCdx := DbfCdx{}
        oDbfCdx:Open(dbInfo)
        //
        oDbfCdx:Append( true ) // Add a new record
        CustomerToRecord(cust, oDbfCdx) // Convert Customer object to DBF record
        //
        oDbfCdx:Close()
		return Ok("PostCustomer called with value: " + cust:FirstName + " " + cust:LastName)
	END METHOD

end class
