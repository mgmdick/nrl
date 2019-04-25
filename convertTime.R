convertTime <- function(x){
  Hour = (x * 24) %% 24 #For x>1, only the value after decimal will be considered
  Minutes = (Hour %% 1) * 60
  Seconds = (Minutes %% 1) * 60
  
  hrs = ifelse (Hour < 10, paste("0",floor(Hour),sep = ""), as.character(floor(Hour)))
  mins = ifelse (Minutes < 10, paste("0",floor(Minutes),sep = ""), as.character(floor(Minutes)))    
  secs = ifelse (Seconds < 10, paste("0",round(Seconds,0),sep = ""), as.character(round(Seconds,0)))
  
  return(paste(hrs, mins, secs, sep = ":"))
}