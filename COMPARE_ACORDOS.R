## COMPARE AND COPY ACORDOS
## SANDRO JAKOSKA

library(DBI)
library(tidyverse)

con3 <- dbConnect(odbc::odbc(), "repro_prod", timeout = 10)
con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)

## TABELAS ====================================================================

A <- dbGetQuery(con2,"
          WITH TAB AS (SELECT  TBPCODIGO,
                                TBPDESCRICAO,
                                 TBPDTINICIO,
                                  TBPDTVALIDADE,
                                    TBPSITUACAO
                                    FROM TABPRECO)
           
                    SELECT CLICODIGO,
                              C.TBPCODIGO,
                               TBPDESCRICAO,
                                 TBPDTINICIO,
                                  TBPDTVALIDADE,
                                    TBPSITUACAO,
                                     TBPDESCFECH,TBPDESC,TBPDESC2,TBPDTCADASTRO
                               FROM CLITBP C
                               LEFT JOIN TAB T ON T.TBPCODIGO=C.TBPCODIGO
                               WHERE CLICODIGO=4219") 

B <- dbGetQuery(con2,"
          WITH TAB AS (SELECT  TBPCODIGO,
                                TBPDESCRICAO,
                                 TBPDTINICIO,
                                  TBPDTVALIDADE,
                                    TBPSITUACAO
                                    FROM TABPRECO)
           
                    SELECT CLICODIGO,
                              C.TBPCODIGO,
                               TBPDESCRICAO,
                                 TBPDTINICIO,
                                  TBPDTVALIDADE,
                                    TBPSITUACAO,
                                     TBPDESCFECH,TBPDESC,TBPDESC2,TBPDTCADASTRO
                               FROM CLITBP C
                               LEFT JOIN TAB T ON T.TBPCODIGO=C.TBPCODIGO
                               WHERE CLICODIGO=4180") 


anti_join(A,B,by="TBPCODIGO") %>% View()

anti_join(B,A,by="TBPCODIGO") %>% View()


include_tb <- anti_join(A,B,by="TBPCODIGO") %>% 
               select(CLICODIGO,TBPCODIGO,TBPDESCFECH,TBPDESC,TBPDESC2,TBPDTCADASTRO) %>% 
                mutate(TBPDTCADASTRO=format(Sys.Date(),"%d.%m.%Y")) %>% 
                 mutate(CLICODIGO=4180) %>% 
                  replace(is.na(.),"null")

x <- data.frame(CLICODIGO=NA,TBPCODIGO=NA,TBPDESCFECH=NA,TBPDESC=NA,TBPDESC2=NA,TBPDTCADASTRO=NA)


for (i in 1:nrow(include_tb)) {
  x[i,] <- include_tb[i,]
  querytb <- paste("INSERT INTO CLITBP (CLICODIGO,TBPCODIGO,TBPDESCFECH,TBPDESC,TBPDESC2,TBPDTCADASTRO) VALUES (",x[i,"CLICODIGO"],",",x[i,"TBPCODIGO"],",'",x[i,"TBPDESCFECH"],"',",x[i,"TBPDESC"],",",x[i,"TBPDESC2"],",'",x[i,"TBPDTCADASTRO"],"');", sep = "")
  dbSendQuery(con3,querytb)
  
}


## COMBINADOS =================================================================== 


AC <- dbGetQuery(con2,"
          WITH TAB AS (SELECT  TBPCODIGO,
                                TBPDESCRICAO,
                                 TBPDTINICIO,
                                  TBPDTVALIDADE,
                                    TBPSITUACAO
                                    FROM TABPRECO)
           
                    SELECT CLICODIGO,
                              C.TBPCODIGO,
                               TBPDESCRICAO,
                                 TBPDTINICIO,
                                  TBPDTVALIDADE,
                                    TBPSITUACAO,
                                     TBPDESCFECH,TBPDESC,TBPDESC2,CLITBPCOMBDTCADASTRO
                               FROM CLITBPCOMB C
                               LEFT JOIN TAB T ON T.TBPCODIGO=C.TBPCODIGO
                               WHERE CLICODIGO=4219") 

BC <- dbGetQuery(con2,"
          WITH TAB AS (SELECT  TBPCODIGO,
                                TBPDESCRICAO,
                                 TBPDTINICIO,
                                  TBPDTVALIDADE,
                                    TBPSITUACAO
                                    FROM TABPRECO)
           
                    SELECT CLICODIGO,
                              C.TBPCODIGO,
                               TBPDESCRICAO,
                                 TBPDTINICIO,
                                  TBPDTVALIDADE,
                                    TBPSITUACAO,
                                     TBPDESCFECH,TBPDESC,TBPDESC2,CLITBPCOMBDTCADASTRO
                               FROM CLITBPCOMB C
                               LEFT JOIN TAB T ON T.TBPCODIGO=C.TBPCODIGO
                               WHERE CLICODIGO=4180") 


anti_join(AC,BC,by="TBPCODIGO") %>% View()

anti_join(BC,AC,by="TBPCODIGO") %>% View()


include_tbc <- anti_join(AC,BC,by="TBPCODIGO") %>% 
  select(CLICODIGO,TBPCODIGO,TBPDESCFECH,TBPDESC,TBPDESC2,CLITBPCOMBDTCADASTRO) %>% 
  mutate(CLITBPCOMBDTCADASTRO=format(Sys.Date(),"%d.%m.%Y")) %>% 
  mutate(CLICODIGO=4180) %>% 
  replace(is.na(.),"null")

y <- data.frame(CLICODIGO=NA,TBPCODIGO=NA,TBPDESCFECH=NA,TBPDESC=NA,TBPDESC2=NA,CLITBPCOMBDTCADASTRO=NA)


for (i in 1:nrow(include_tbc)) {
  y[i,] <- include_tbc[i,]
  querytbc <- paste("INSERT INTO CLITBPCOMB (CLICODIGO,TBPCODIGO,TBPDESCFECH,TBPDESC,TBPDESC2,CLITBPCOMBDTCADASTRO) VALUES (",y[i,"CLICODIGO"],",",y[i,"TBPCODIGO"],",'",y[i,"TBPDESCFECH"],"',",y[i,"TBPDESC"],",",y[i,"TBPDESC2"],",'",y[i,"CLITBPCOMBDTCADASTRO"],"');", sep = "")
  dbSendQuery(con3,querytbc)
  
}


