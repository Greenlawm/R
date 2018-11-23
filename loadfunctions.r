loadfunctions = function(){
  
  #source(file.path(ss.codedirectory, "loadfunctions.r"))
  pathnames <- list.files(pattern="[.]r$", path = ss.codedirectory, full.names=TRUE, recursive=T)
  sapply(pathnames, FUN=source)
  print("Functions Loaded")
  
}