## OVERVIEW ACORDOS
## SANDRO JAKOSKA

library(DBI)
library(tidyverse)

con3 <- dbConnect(odbc::odbc(), "repro_prod", timeout = 10)
con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)

## TABLES
dbGetQuery(con2,"SELECT * FROM TABPRECO WHERE TBPCODIGO=73")

## PRODUCTS
dbGetQuery(con2,"SELECT * FROM TBPPRODU WHERE TBPCODIGO=73") %>% View()

## CLIENTS
dbGetQuery(con2,"SELECT * FROM CLITBP WHERE TBPCODIGO=73") %>% View()