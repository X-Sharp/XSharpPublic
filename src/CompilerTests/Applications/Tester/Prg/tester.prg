FUNCTION Start() AS VOID
  LOCAL oCustomer AS TUPLE(Name AS STRING, Age AS INT)
  oCustomer := TUPLE{"Nikos", 47}

  LOCAL name AS STRING
  LOCAL age AS INT

  (name, age) := oCustomer
  ? name, age // "Nikos", 47
  ? iif (age = 47, (age -= 1,age += 1,age), 46)
  return


FUNCTION Start2() AS VOID
  LOCAL oCustomer AS TUPLE(STRING, INT)
  oCustomer := TUPLE{"Nikos", 47}

  LOCAL (name AS STRING, age AS INT) := oCustomer
  ? name, age // "Nikos", 47
  // You can also deconstruct into existing local variables without the LOCAL keyword:
  (name, age) := oCustomer
