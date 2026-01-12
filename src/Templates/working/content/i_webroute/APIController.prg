// UsersController
using System.Threading.Tasks
using Microsoft.AspNetCore.Mvc
using System.Collections.Generic

[ApiController];
[Route("[controller]")];
public class APIController inherit ControllerBase
	PUBLIC CONSTRUCTOR()

    [HttpGet];
	public async method GetMethod() as Task<IActionResult>
        //
        VAR oneObject := Object{}
		return Ok(oneObject)
	END METHOD

    [HttpPost];
    public async method PostMethod([FromBody] items AS List<String> ) as Task<IActionResult>
        //
        VAR message := "Post Ok"
		return Ok( message )
	END METHOD

    [HttpPost];
    [Route("Item")];
    public async method SingleCustomer([FromBody] item AS String ) as Task<IActionResult>
        //
        VAR message := "Post Ok: " + item
        Console.WriteLine( message )
		return Ok( message )
	END METHOD

end class
