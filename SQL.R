
##  UPDATE DESCTOS
## SANDRO JAKOSKA

library(DBI)
library(tidyverse)

con3 <- dbConnect(odbc::odbc(), "repro_prod", timeout = 10)
con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)


query8 <- paste("UPDATE TBPPRODU SET TBPPCDESCTO=14,TBPPCDESCTO2=14 WHERE TBPCODIGO=73 AND TBPPCDESCTO2=22;", sep = "")
dbSendQuery(con3,query8)



query8 <- paste("DELETE FROM TBPPRODU WHERE TBPCODIGO=861")
dbSendQuery(con3,query8)

query9 <- paste("DELETE FROM TBPCOMBPROPRO WHERE TBPCODIGO=862")
dbSendQuery(con3,query9)


query8 <- paste("DELETE FROM CLITBPCOMB WHERE TBPCODIGO=22 AND CLICODIGO=4543")
dbSendQuery(con3,query8)



query8 <- paste("UPDATE TABPRECO SET TBPSITUACAO='A' WHERE TBPCODIGO=861")
dbSendQuery(con3,query8)



