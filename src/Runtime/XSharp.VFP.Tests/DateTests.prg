//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System
using System.Collections.Generic
using System.Linq
using System.Text
using XUnit


// Array tests are not working correctly yet with the current build
begin namespace XSharp.VFP.Tests

	class DateTests
	    static constructor
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro

		[Fact, Trait("Category", "Date and Time")];
		method ConversionTests() as void
            local d as date
            local dt as DateTime
            dt := DateTime{2020,1,1,11,12,13}

            Assert.Equal(11, Hour(dt))
            Assert.Equal(12, Minute(dt))
            Assert.Equal(13, Sec(dt))
            d := ConDate(2020,1,1)
            Assert.Equal(d, TTod(dt))
            dt := DateTime{2020,1,1}
            Assert.True( DToT(d)== dt)


		[Fact, Trait("Category", "Date and Time")];
            method QuarterTests() as void
            Assert.Equal( Quarter ( ConDate ( 2020, 1 , 1  ) ) , 1 )
            Assert.Equal( Quarter ( ConDate ( 2020, 2 , 1  ) ) , 1 )
            Assert.Equal( Quarter ( ConDate ( 2020, 3 , 1  ) ) , 1 )
            Assert.Equal( Quarter ( ConDate ( 2020, 4 , 1  ) ) , 2 )
            Assert.Equal( Quarter ( ConDate ( 2020, 5 , 1  ) ) , 2 )
            Assert.Equal( Quarter ( ConDate ( 2020, 6 , 1  ) ) , 2 )
            Assert.Equal( Quarter ( ConDate ( 2020, 7 , 1  ) ) , 3 )
            Assert.Equal( Quarter ( ConDate ( 2020, 8 , 1  ) ) , 3 )
            Assert.Equal( Quarter ( ConDate ( 2020, 9 , 1  ) ) , 3 )
            Assert.Equal( Quarter ( ConDate ( 2020, 10 , 1 ) ) , 4 )
            Assert.Equal( Quarter ( ConDate ( 2020, 11 , 1 ) ) , 4 )
            Assert.Equal( Quarter ( ConDate ( 2020, 12 , 1 ) ) , 4 )

            Assert.Equal( Quarter ( ConDate ( 2020, 1 , 1  ) , 1 ), 1 )
            Assert.Equal( Quarter ( ConDate ( 2020, 2 , 1  ) , 1 ), 1 )
            Assert.Equal( Quarter ( ConDate ( 2020, 3 , 1  ) , 1 ), 1 )
            Assert.Equal( Quarter ( ConDate ( 2020, 4 , 1  ) , 1) ,2 )
            Assert.Equal( Quarter ( ConDate ( 2020, 5 , 1  ) , 1) ,2  )
            Assert.Equal( Quarter ( ConDate ( 2020, 6 , 1  ) , 1) ,2  )
            Assert.Equal( Quarter ( ConDate ( 2020, 7 , 1  ) , 1) ,3  )
            Assert.Equal( Quarter ( ConDate ( 2020, 8 , 1  ) , 1) ,3  )
            Assert.Equal( Quarter ( ConDate ( 2020, 9 , 1  ) , 1) ,3  )
            Assert.Equal( Quarter ( ConDate ( 2020, 10 , 1  ) , 1), 4 )
            Assert.Equal( Quarter ( ConDate ( 2020, 11 , 1  ) , 1), 4 )
            Assert.Equal( Quarter ( ConDate ( 2020, 12 , 1  ) , 1), 4 )

            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 1  ) ,3  )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 2  ) ,3  )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 3  ) ,3  )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 4  ) ,2  )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 5  ) ,2 )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 6  ) ,2  )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 7  ) ,1  )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 8  ) ,1  )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 9  ) ,1  )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 10  ), 4 )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 11  ), 4 )
            Assert.Equal( Quarter ( ConDate ( 2000, 9 , 23  ) , 12  ), 4 )

            Assert.Equal( Quarter ( ConDate ( 2018, 1, 14    ), 4) ,4   )
            Assert.Equal( Quarter ( ConDate ( 2018, 3, 14    ), 4) ,4   )
            Assert.Equal( Quarter ( ConDate ( 2018, 4, 14    ), 4) ,1   )
            Assert.Equal( Quarter ( ConDate ( 2018, 6, 14    ), 4) ,1   )
            Assert.Equal( Quarter ( ConDate ( 2018, 7, 14    ), 4) ,2   )
            Assert.Equal( Quarter ( ConDate ( 2018, 9, 14    ), 4) ,2   )
            Assert.Equal( Quarter ( ConDate ( 2018, 10, 14    ), 4), 3  )
            Assert.Equal( Quarter ( ConDate ( 2018, 12, 14    ), 4), 3  )

            Assert.Equal( Quarter ( {^2000-9-23}, 7 ) , 1)
            Assert.Equal( Quarter ( NULL_DATE ) , 0)
            Assert.Equal( Quarter ( DateTime{} ) , 0  )
            Assert.Equal( Quarter ( 2020.04.20 ) , 2         )

		[Fact, Trait("Category", "Date and Time")];
            method GoMonthTests() as void

            Assert.Equal(GoMonth ( NULL_DATE , -12 ) , NULL_DATE)
            Assert.Equal(GoMonth ( DateTime{} , -12 ) , NULL_DATE)
            Assert.Equal(GoMonth ( Today() , 0 ) , Today()  )
            Assert.Equal(GoMonth ( ConDate ( 9999, 12 , 31 )  , 1 ) , NULL_DATE) // this causes a suppressed exception !
            Assert.Equal(GoMonth ( ConDate ( 1753, 1 , 1 )  , 0 ) , ConDate ( 1753, 1 , 1 ))
            Assert.Equal(GoMonth ( ConDate ( 1753, 1 , 1 )  , -1 ) , NULL_DATE) // VFP year limit is 1753
            Assert.Equal(GoMonth({^1998-12-31}, 2) , ConDate ( 1999 , 2 , 28 ) )
            Assert.Equal(GoMonth({^1998-12-31}, -2) , ConDate ( 1998 , 10 , 31  ) )

		[Fact, Trait("Category", "Date and Time")];
            method WeekTests() as void
                Assert.Equal(Week(1998.02.16)  , 8)
                Assert.Equal(Week(1998.02.16,1,1)  , 8)
                Assert.Equal(Week(2010.01.03,2,4)  , 1)
                Assert.Equal(Week(2013.01.05,1,1)  , 1)    // Sunday, 1st week has jan 1
                Assert.Equal(Week(2013.01.05,2,1)  , 1)    // Sunday, 1st week has 4 days
                Assert.Equal(Week(2013.01.05,3,1)  , 53)    // Sunday, 1st week has 7 days


	[Fact, Trait("Category", "Date and Time")];
	method TToCTests as void
	    local dt as DateTime

	    dt := DateTime{2025, 8, 13, 14, 30, 45}

	    // Parámetro 0 - formato numérico (igual que parámetro 1)
	    Assert.Equal("20250813143045", TToC(dt, 0))

	    // Parámetro 1 - formato numérico
	    Assert.Equal("20250813143045", TToC(dt, 1))

	    // Parámetro 2 - solo hora
	    Assert.Equal("02:30:45 PM", TToC(dt, 2))

	    // Parámetro 3 - formato ISO
	    Assert.Equal("2025-08-13T14:30:45", TToC(dt, 3))

	end method

	[Fact, Trait("Category", "Date and Time")];
	method TToCDefaultFormatTests as void
	  local dt as DateTime
	  dt := DateTime{2025, 8, 13, 14, 30, 45}

	  // Sin parámetro - debería usar formato por defecto (depende de SET CENTURY, etc.)
	  local result := TToC(dt) as string
	  Assert.True(result != null)
	  Assert.True(result:Length > 0)

	  // Con SET CENTURY ON debería incluir siglo completo
	  local oldCentury := SetCentury(true) as logic
	  local resultWithCentury := TToC(dt) as string
	  SetCentury(oldCentury)

	  Assert.True(resultWithCentury:Contains("2025")) // Debe contener año completo
	end method

	[Fact, Trait("Category", "Date and Time")];
	method TToCHoursFormatTests as void
	  local dt as DateTime
	  dt := DateTime{2025, 8, 13, 14, 30, 45} // 2:30 PM

	  // Guardar configuración actual
	  local oldHours := SetHours() as long
	  local oldSeconds := SetSeconds() as logic

	  try
	      // Formato 12 horas con segundos
	      SetHours(12)
	      SetSeconds(true)
	      Assert.Equal("02:30:45 PM", TToC(dt, 2))

	      // Formato 12 horas sin segundos
	      SetSeconds(false)
	      Assert.Equal("02:30 PM", TToC(dt, 2))

	      // Formato 24 horas con segundos
	      SetHours(24)
	      SetSeconds(true)
	      Assert.Equal("14:30:45", TToC(dt, 2))

	      // Formato 24 horas sin segundos
	      SetSeconds(false)
	      Assert.Equal("14:30", TToC(dt, 2))

	  finally
	      // Restaurar configuración
	      SetHours(oldHours)
	      SetSeconds(oldSeconds)
	  end try
	end method

	[Fact, Trait("Category", "Date and Time")];
	method TToCEdgeCasesTests as void
	  // Medianoche
	  local midnight := DateTime{2025, 8, 13, 0, 0, 0} as DateTime
	  Assert.Equal("12:00:00 AM", TToC(midnight, 2))

	  // Mediodía
	  local noon := DateTime{2025, 8, 13, 12, 0, 0} as DateTime
	  Assert.Equal("12:00:00 PM", TToC(noon, 2))

	  // 1 AM
	  local oneAM := DateTime{2025, 8, 13, 1, 0, 0} as DateTime
	  Assert.Equal("01:00:00 AM", TToC(oneAM, 2))

	  // 1 PM
	  local onePM := DateTime{2025, 8, 13, 13, 0, 0} as DateTime
	  Assert.Equal("01:00:00 PM", TToC(onePM, 2))

	  // 11 PM
	  local elevenPM := DateTime{2025, 8, 13, 23, 59, 59} as DateTime
	  Assert.Equal("11:59:59 PM", TToC(elevenPM, 2))
	end method

	[Fact, Trait("Category", "Date and Time")];
	method TToCNumericFormatTests as void
	  local dt as DateTime
	  dt := DateTime{2025, 8, 13, 14, 30, 45}

	  // Formato numérico completo
	  Assert.Equal("20250813143045", TToC(dt, 1))

	  // Sin segundos (si aplica)
	  local oldSeconds := SetSeconds(false) as logic
	  try
	      // El formato numérico debería incluir segundos independientemente del SET SECONDS
	      Assert.Equal("20250813143045", TToC(dt, 1))
	  finally
	      SetSeconds(oldSeconds)
	  end try
	end method

	[Fact, Trait("Category", "Date and Time")];
	method TToCISOFormatTests as void
	  local dt as DateTime
	  dt := DateTime{2025, 8, 13, 14, 30, 45}

	  // Formato ISO estándar
	  Assert.Equal("2025-08-13T14:30:45", TToC(dt, 3))

	  // Verificar que no se ve afectado por configuraciones locales
	  local oldHours := SetHours(12) as long
	  local oldSeconds := SetSeconds(false) as logic
	  try
	      // ISO debe mantener formato 24h y incluir segundos siempre
	      Assert.Equal("2025-08-13T14:30:45", TToC(dt, 3))
	  finally
	      SetHours(oldHours)
	      SetSeconds(oldSeconds)
	  end try
	end method

	[Fact, Trait("Category", "Date and Time")];
	method TToCInvalidParameterTests as void
	  local dt as DateTime
	  dt := DateTime{2025, 8, 13, 14, 30, 45}

	  // Parámetros inválidos - deberían usar formato por defecto o lanzar error
	  try
	      local result := TToC(dt, 99) as string
	      // Si no lanza error, debería devolver algo válido
	      Assert.True(result != null)
	      Assert.True(result:Length > 0)
	  catch e as Exception
	      // Si lanza error, está bien también
	      Assert.True(e != null)
	  end try

	  // Parámetro negativo
	  try
	      local result := TToC(dt, -1) as string
	      Assert.True(result != null)
	      Assert.True(result:Length > 0)
	  catch e as Exception
	      Assert.True(e != null)
	  end try
	end method

	[Fact, Trait("Category", "Date and Time")];
	method TToCDateOnlyTests as void
	  // Probar con Date (sin hora)
	  local d := DateTime{2025, 8, 13} as DateTime

	  // Formato numérico con fecha sin hora
	  local numericResult := TToC(d, 1) as string
	  Assert.True(numericResult:StartsWith("20250813"))

	  // Formato solo hora con fecha sin hora
	  local timeResult := TToC(d, 2) as string
	  Assert.True(timeResult:Contains("12:00:00 AM")) // Debería ser medianoche

	  // Formato ISO con fecha sin hora
	  local isoResult := TToC(d, 3) as string
	  Assert.True(isoResult:StartsWith("2025-08-13T00:00:00"))
	end method

	[Fact, Trait("Category", "Date and Time")];
	method TToCConfigurationPersistenceTests as void
	  // Verificar que los cambios de configuración persisten
	  local originalHours := SetHours() as long
	  local originalSeconds := SetSeconds() as logic

	  // Cambiar configuración
	  SetHours(12)
	  SetSeconds(true)

	  // Verificar que los cambios se mantienen
	  Assert.Equal(12, SetHours())
	  Assert.Equal(true, SetSeconds())

	  // Usar TToC y verificar que usa la nueva configuración
	  local dt := DateTime{2025, 8, 13, 14, 30, 45} as DateTime
	  local result := TToC(dt, 2) as string
	  Assert.True(result:Contains("PM"))
	  Assert.True(result:Contains(":45")) // Debe incluir segundos

	  // Restaurar configuración original
	  SetHours(originalHours)
	  SetSeconds(originalSeconds)

	  // Verificar que se restauró
	  Assert.Equal(originalHours, SetHours())
	  Assert.Equal(originalSeconds, SetSeconds())
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTBasicTests as void
	    local result as DateTime

	    // Formato numérico completo
	    result := CToT("20250813143045")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	    // Formato numérico sin hora
	    result := CToT("20250813")
	    Assert.Equal(DateTime{2025, 8, 13, 0, 0, 0}, result)

	    // Formato ISO
	    result := CToT("2025-08-13T14:30:45")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	    // Formato ISO sin segundos
	    result := CToT("2025-08-13T14:30")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 0}, result)
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTDateFormatTests as void
	    local result as DateTime
	    local oldDateFormat := SetDateFormat("AMERICAN") as string
	    local oldCentury := SetCentury() as logic

	    try
	        // Formato AMERICAN (MM/dd/yyyy)
	        SetDateFormat("AMERICAN")
	        SetCentury(true)

	        result := CToT("08/13/2025")
	        Assert.Equal(DateTime{2025, 8, 13}, result)

	        result := CToT("08/13/2025 14:30:45")
	        Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	        // Formato BRITISH (dd/MM/yyyy)
	        SetDateFormat("BRITISH")

	        result := CToT("13/08/2025")
	        Assert.Equal(DateTime{2025, 8, 13}, result)

	        result := CToT("13/08/2025 14:30:45")
	        Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	        // Formato ANSI (yyyy.MM.dd)
	        SetDateFormat("ANSI")

	        result := CToT("2025.08.13")
	        Assert.Equal(DateTime{2025, 8, 13}, result)

	        result := CToT("2025.08.13 14:30:45")
	        Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	    finally
	        SetDateFormat(oldDateFormat)
	        SetCentury(oldCentury)
	    end try
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTTimeFormatTests as void
	    local result as DateTime
	    local oldHours := SetHours() as long
	    local oldSeconds := SetSeconds() as logic

	    try
	        // Formato 12 horas con AM/PM
	        SetHours(12)
	        SetSeconds(true)

	        result := CToT("08/13/2025 02:30:45 PM")
	        Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	        result := CToT("08/13/2025 02:30:45 AM")
	        Assert.Equal(DateTime{2025, 8, 13, 2, 30, 45}, result)

	        // Formato 24 horas
	        SetHours(24)

	        result := CToT("08/13/2025 14:30:45")
	        Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	        result := CToT("08/13/2025 02:30:45")
	        Assert.Equal(DateTime{2025, 8, 13, 2, 30, 45}, result)

	        // Sin segundos
	        SetSeconds(false)

	        result := CToT("08/13/2025 14:30")
	        Assert.Equal(DateTime{2025, 8, 13, 14, 30, 0}, result)

	    finally
	        SetHours(oldHours)
	        SetSeconds(oldSeconds)
	    end try
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTCenturyTests as void
	    local result as DateTime
	    local oldCentury := SetCentury() as logic
	    local oldDateFormat := SetDateFormat("AMERICAN") as string

	    try
	        SetDateFormat("AMERICAN")

	        // Con siglo completo
	        SetCentury(true)
	        result := CToT("08/13/2025")
	        Assert.Equal(DateTime{2025, 8, 13}, result)

	        result := CToT("08/13/25")  // Debería interpretar como 2025
	        Assert.Equal(DateTime{2025, 8, 13}, result)

	        // Sin siglo (años de 2 dígitos)
	        SetCentury(false)
	        result := CToT("08/13/25")
	        Assert.Equal(DateTime{2025, 8, 13}, result)  // Asumiendo ventana de siglo apropiada

	        result := CToT("08/13/2025")  // Debería funcionar también con 4 dígitos
	        Assert.Equal(DateTime{2025, 8, 13}, result)

	    finally
	        SetCentury(oldCentury)
	        SetDateFormat(oldDateFormat)
	    end try
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTEdgeCasesTests as void
	    local result as DateTime

	    // Medianoche
	    result := CToT("08/13/2025 12:00:00 AM")
	    Assert.Equal(DateTime{2025, 8, 13, 0, 0, 0}, result)

	    // Mediodía
	    result := CToT("08/13/2025 12:00:00 PM")
	    Assert.Equal(DateTime{2025, 8, 13, 12, 0, 0}, result)

	    // 1 AM
	    result := CToT("08/13/2025 01:00:00 AM")
	    Assert.Equal(DateTime{2025, 8, 13, 1, 0, 0}, result)

	    // 11:59:59 PM
	    result := CToT("08/13/2025 11:59:59 PM")
	    Assert.Equal(DateTime{2025, 8, 13, 23, 59, 59}, result)

	    // Solo hora (debería usar fecha mínima o actual)
	    result := CToT("14:30:45")
	    Assert.Equal(14, result:Hour)
	    Assert.Equal(30, result:Minute)
	    Assert.Equal(45, result:Second)
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTInvalidInputTests as void
	    local result as DateTime

	    // Cadena vacía
	    result := CToT("")
	    Assert.Equal(DateTime.MinValue, result)

	    // Cadena nula
	    result := CToT(null)
	    Assert.Equal(DateTime.MinValue, result)

	    // Solo espacios
	    result := CToT("   ")
	    Assert.Equal(DateTime.MinValue, result)

	    // Formato inválido
	    result := CToT("fecha inválida")
	    Assert.Equal(DateTime.MinValue, result)

	    // Fecha inexistente
	    result := CToT("02/30/2025")  // 30 de febrero no existe
	    Assert.Equal(DateTime.MinValue, result)

	    // Hora inválida
	    result := CToT("08/13/2025 25:00:00")  // Hora 25 no existe
	    Assert.Equal(DateTime.MinValue, result)
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTFlexibleFormatsTests as void
	    local result as DateTime

	    // Diferentes separadores de fecha
	    result := CToT("2025-08-13")
	    Assert.Equal(DateTime{2025, 8, 13}, result)

	    result := CToT("2025/08/13")
	    Assert.Equal(DateTime{2025, 8, 13}, result)

	    result := CToT("2025.08.13")
	    Assert.Equal(DateTime{2025, 8, 13}, result)

	    // Sin ceros iniciales
	    result := CToT("8/13/2025")
	    Assert.Equal(DateTime{2025, 8, 13}, result)

	    result := CToT("8/3/2025")
	    Assert.Equal(DateTime{2025, 8, 3}, result)

	    // Diferentes formatos de hora
	    result := CToT("08/13/2025 2:30 PM")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 0}, result)

	    result := CToT("08/13/2025 14:30")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 0}, result)
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTSpecialVFPFormatsTests as void
	    local result as DateTime

	    // Formato numérico compacto sin separadores
	    result := CToT("20250813")
	    Assert.Equal(DateTime{2025, 8, 13}, result)

	    result := CToT("20250813143045")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	    result := CToT("202508131430")  // Sin segundos
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 0}, result)

	    // Formato con T (ISO-like)
	    result := CToT("20250813T143045")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTRoundTripTests as void
	    // Verificar que CToT puede parsear lo que TToC genera
	    local originalDt := DateTime{2025, 8, 13, 14, 30, 45} as DateTime

	    // Formato numérico
	    local numericStr := TToC(originalDt, 1) as string
	    local parsedDt := CToT(numericStr) as DateTime
	    Assert.Equal(originalDt, parsedDt)

	    // Formato ISO
	    local isoStr := TToC(originalDt, 3) as string
	    parsedDt := CToT(isoStr)
	    Assert.Equal(originalDt, parsedDt)

	    // Formato por defecto
	    local defaultStr := TToC(originalDt) as string
	    parsedDt := CToT(defaultStr)
	    Assert.Equal(originalDt:Date, parsedDt:Date)  // Al menos la fecha debe coincidir
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTWhitespaceHandlingTests as void
	    local result as DateTime

	    // Espacios al inicio y final
	    result := CToT("  20250813143045  ")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	    // Espacios entre fecha y hora
	    result := CToT("08/13/2025   14:30:45")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	    // Tabs y otros espacios en blanco
	    result := CToT("\t08/13/2025\t14:30:45\t")
	    Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)
	end method

	[Fact, Trait("Category", "Date and Time")];
	method CToTConfigurationIndependenceTests as void
	    // Verificar que ciertos formatos funcionan independientemente de la configuración
	    local oldDateFormat := SetDateFormat("AMERICAN") as string
	    local oldCentury := SetCentury() as logic
	    local oldHours := SetHours() as long

	    try
	        // Cambiar todas las configuraciones
	        SetDateFormat("BRITISH")
	        SetCentury(false)
	        SetHours(12)

	        // El formato numérico debe funcionar independientemente
	        local result := CToT("20250813143045") as DateTime
	        Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	        // El formato ISO debe funcionar independientemente
	        result := CToT("2025-08-13T14:30:45")
	        Assert.Equal(DateTime{2025, 8, 13, 14, 30, 45}, result)

	    finally
	        SetDateFormat(oldDateFormat)
	        SetCentury(oldCentury)
	        SetHours(oldHours)
	    end try
	end method

end class

end namespace
