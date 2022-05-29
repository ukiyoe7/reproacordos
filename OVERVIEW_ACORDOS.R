library(DBI)
library(tidyverse)

con3 <- dbConnect(odbc::odbc(), "repro_prod", timeout = 10)
con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)


dbGetQuery(con2,"SELECT * FROM TABPRECO WHERE TBPCODIGO=73")

dbGetQuery(con2,"SELECT * FROM TBPPRODU WHERE TBPCODIGO=73") %>% View()
