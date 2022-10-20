## OVERVIEW ACORDOS
## SANDRO JAKOSKA

library(DBI)
library(tidyverse)

con3 <- dbConnect(odbc::odbc(), "repro_prod", timeout = 10)
con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)



## TABLES
acordos_cliente <- dbGetQuery(con2,"
           WITH CLI AS (SELECT CLICODIGO,
                                CLINOMEFANT,
                                 GCLCODIGO 
                                  FROM CLIEN WHERE CLICODIGO=792),
           
           CLITB AS (SELECT CT.CLICODIGO,
                             GCLCODIGO,
                              CLINOMEFANT,
                              TBPCODIGO 
                               FROM CLITBP CT
                                INNER JOIN CLI C ON C.CLICODIGO=CT.CLICODIGO)
                                
                      SELECT CLICODIGO,
                              GCLCODIGO,
                               CLINOMEFANT,
                                TB.TBPCODIGO,
                                 TBPDESCRICAO,
                                  TBPDTINICIO,
                                   TBPDTVALIDADE,
                                    TBPSITUACAO
                                     FROM TABPRECO TB
                                      INNER JOIN CLITB CT ON CT.TBPCODIGO=TB.TBPCODIGO") %>% 
                                       filter(TBPSITUACAO !='I')

View(acordos_cliente)



## PRODUCTS
dbGetQuery(con2,"
           WITH CLI AS (SELECT CLICODIGO,
                                CLINOMEFANT,
                                 GCLCODIGO 
                                  FROM CLIEN),
           
           CLITB AS (SELECT CT.CLICODIGO,
                             GCLCODIGO,
                              CLINOMEFANT,
                              TBPCODIGO 
                               FROM CLITBP CT
                                INNER JOIN CLI C ON C.CLICODIGO=CT.CLICODIGO WHERE TBPCODIGO IN (201,202)),
                                
           TAB AS (SELECT CLICODIGO,
                             GCLCODIGO,
                              CLINOMEFANT,
                          TB.TBPCODIGO,
                           TBPDESCRICAO,
                            TBPDTINICIO,
                              TBPDTVALIDADE,
                               TBPSITUACAO
                                FROM TABPRECO TB
                                 INNER JOIN CLITB CT ON CT.TBPCODIGO=TB.TBPCODIGO
                                 WHERE
                                  TB.TBPCODIGO NOT IN (5,6,125,126,100,101,102,103,104,105)),                      
 
           PROD AS (SELECT PRODESCRICAO,
                            PROCODIGO FROM PRODU WHERE PROSITUACAO='A')
           
           SELECT CLICODIGO,
                   GCLCODIGO,
                    CLINOMEFANT,
                     TB.TBPCODIGO,
                      TBPDESCRICAO,
                       TBPDTINICIO,
                        TBPDTVALIDADE,
                         TBPSITUACAO,
                          TB.PROCODIGO,
                           PRODESCRICAO,
                            TBPPCOVENDA,
                             TBPPCDESCTO, 
                              TBPPCDESCTO2,
                               TBPPCOVENDA2
                             FROM TBPPRODU TB
                             INNER JOIN TAB TA ON TA.TBPCODIGO=TB.TBPCODIGO
                             LEFT JOIN PROD P ON TB.PROCODIGO=P.PROCODIGO") %>% View()

## TABLES
dbGetQuery(con2,"SELECT * FROM TABPRECO") %>% View()

## RELREPRO


dbGetQuery(con2,"SELECT * FROM CLITBP WHERE CLICODIGO=792") %>% View()


## PRODUCTS
dbGetQuery(con2,"
           WITH PROD AS (SELECT PRODESCRICAO,PROCODIGO FROM PRODU WHERE PROSITUACAO='A')
           
           SELECT PRODESCRICAO,T.*,T.PROCODIGO FROM TBPPRODU T
           INNER JOIN PROD P ON T.PROCODIGO=P.PROCODIGO
           WHERE TBPCODIGO=861") %>% View()

## CLIENTS
dbGetQuery(con2,"SELECT * FROM CLITBP WHERE TBPCODIGO=789") %>% View()