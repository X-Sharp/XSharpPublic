FUNCTION Start() AS VOID
    ? SetAmPm(TRUE)
    ? Time(), SetHours()
    ? SetAmPm(FALSE)
    ? Time(), SetHours()
    ? SetHours(12)
    ? Time(), SetAmPm()
    ? SetHours(24)
    ? Time(), SetAmPm()
    wait
