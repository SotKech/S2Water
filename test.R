for (i in 1:10) {
  message(i,"\r",appendLF=FALSE)
  flush.console()
  Sys.sleep(1)
}


for (i in 1:10) {
  
  # Sleep for 1 second
  Sys.sleep(1)
  
  # Print the current iteration
  cat(paste0("\r", i))      
}
