# Importing Algorithm
#
#   a <- readxl::read_excel("FTSE100.xlsx")
#   for(i in 2:dim(a)[2]){
#     nam <- paste("STOCK", i-1, sep = "_")
#     assign(nam,a[2:4697,i])
#   }
#

pickup <- function(data="data/FTSE100.Rdata"){
  load(data)
  return(NULL)
}


cleanup <- function(data){
  return(clean)
}

partition <- function(data){
  return(part)
}

listgen <- function(){
  return(list)
}
