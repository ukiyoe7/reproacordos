## OVERVIEW ACORDOS
## SANDRO JAKOSKA

library(DBI)
library(tidyverse)

con3 <- dbConnect(odbc::odbc(), "repro_prod", timeout = 10)
con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)

## TABLES
dbGetQuery(con2,"SELECT * FROM TABPRECO WHERE TBPCODIGO=73")

## PRODUCTS
dbGetQuery(con2,"
           WITH PROD AS (SELECT PRODESCRICAO,PROCODIGO FROM PRODU WHERE PROSITUACAO='A')
           
           SELECT PRODESCRICAO,T.*,T.PROCODIGO FROM TBPPRODU T
           INNER JOIN PROD P ON T.PROCODIGO=P.PROCODIGO
           WHERE TBPCODIGO=73") %>% View()

## CLIENTS
dbGetQuery(con2,"SELECT * FROM CLITBP WHERE TBPCODIGO=73") %>% View()