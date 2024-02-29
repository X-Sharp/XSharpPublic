USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Threading
USING Serilog.Core
USING Serilog.Events
USING Serilog.Configuration
USING Serilog.Formatting.Display
USING Serilog
  
BEGIN NAMESPACE VFPXPorterLib
  
	/// <summary>
	/// Serilog Buffered Sink : 
	/// All Emit Events are buffered in a List<LogEvent>
	/// You can retrieve ALL Events via Log, or only Errors.
	/// Don't forget to clear the Buffer
	/// </summary>
	PUBLIC CLASS BufferedSink IMPLEMENTS ILogEventSink, IDisposable
  
		PRIVATE STATIC INITONLY LocalInstance := AsyncLocal<BufferedSink>{} AS AsyncLocal<BufferedSink>
  
		/// <summary>
		/// LogEvent buffer
		/// </summary>
		PRIVATE INITONLY _logEvents AS List<LogEvent>
  
		/// <summary>
		/// Return the current BufferedSink instance object
		/// </summary>
		/// <value></value>
		PUBLIC STATIC PROPERTY Instance AS BufferedSink
			GET
				IF LocalInstance:Value == NULL
					LocalInstance:Value := BufferedSink{}
				ENDIF
				RETURN LocalInstance:Value
  
			END GET
		END PROPERTY
  
		/// <summary>
		/// Retrieve all LogEvents
		/// </summary>
		/// <value></value>
		PUBLIC PROPERTY LogEvents AS IEnumerable<LogEvent> GET _logEvents:AsReadOnly()
  
		PUBLIC CONSTRUCTOR()
			SELF:_logEvents := List<LogEvent>{}
  

		PUBLIC METHOD Emit(logEvent AS LogEvent ) AS VOID
			SELF:_logEvents:Add(logEvent)
  
		PUBLIC METHOD Dispose() AS VOID
			SELF:_logEvents:Clear()
  
		PUBLIC METHOD Clear() AS VOID
			SELF:_logEvents:Clear()
  
		PUBLIC PROPERTY Log AS STRING
			GET
				VAR outputTemplate := "[{Timestamp:HH:mm:ss} {Level:u3}] {Message:lj}{NewLine}{Exception}"
				VAR formatter := MessageTemplateTextFormatter{outputTemplate, NULL }
				VAR writer := StringWriter{}
				//
				FOREACH VAR evt IN SELF:_logEvents
					formatter:Format( evt, writer )
				NEXT
				RETURN writer:ToString()
			END GET
		END PROPERTY
  
		PUBLIC PROPERTY Errors AS STRING
			GET
				VAR outputTemplate := "[{Timestamp:HH:mm:ss} {Level:u3}] {Message:lj}{NewLine}{Exception}"
				VAR formatter := MessageTemplateTextFormatter{outputTemplate, NULL }
				VAR writer := StringWriter{}
				//
				FOREACH VAR evt IN SELF:_logEvents
					IF evt:Level == LogEventLevel.Error .OR. evt:Level == LogEventLevel.Fatal
						formatter:Format( evt, writer )
					ENDIF
				NEXT
				RETURN writer:ToString()
			END GET
		END PROPERTY
  
	END	CLASS
  
	/// <summary>
	/// LoggerConfiguration Extension Class, in order to use FabLog
	/// </summary>
	PUBLIC STATIC CLASS BufferedSinkExtensions
  
		/// <summary>
		/// Easy to use Method to create a LoggerConfiguration, using the BufferedSink
		/// </summary>
		/// <param name="sinkConfiguration"></param>
		/// <param name="restrictedToMinimumLevel"></param>
		/// <param name="levelSwitch"></param>
		/// <returns></returns>
		PUBLIC STATIC METHOD BufferedLog(SELF sinkConfiguration AS LoggerSinkConfiguration , ;
			restrictedToMinimumLevel := LogEventLevel.Verbose AS LogEventLevel , ;
			levelSwitch := NULL AS LoggingLevelSwitch ) AS LoggerConfiguration
			IF sinkConfiguration == NULL
				THROW ArgumentNullException{ "sinkConfiguration"}
			ENDIF
			RETURN sinkConfiguration:Sink(BufferedSink.Instance, restrictedToMinimumLevel, levelSwitch)
  
  
	END CLASS
  
END NAMESPACE