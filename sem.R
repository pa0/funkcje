#
# wyliczanie błędu pomiarowego
#
sem <- function(sd, rtt)
  {
  return(sd*sqrt(1-rtt))
  }
