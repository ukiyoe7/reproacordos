## TABELAS ACORDOS

library(DBI)
library(tidyverse)
library(googlesheets4)
library(xlsx)
library(reshape2)


## GERAL ======================================


cli_desctgeral <- dbGetQuery(con2,"SELECT CLICODIGO,GCLCODIGO,CLIPCDESCPRODU DESCTGERAL FROM CLIEN")

View(cli_desctgeral)

## VARILUX ======================================

## LOJAS

clitbp_vlx <- dbGetQuery(con2,"SELECT CLICODIGO,TBPCODIGO,TBPDESC2 FROM CLITBP
                     WHERE TBPCODIGO IN (101,102)")

clitbp_vlx_2 <- left_join(clitbp,aux, by="TBPCODIGO") 

View(clitbp_vlx_2)


c_vlx_order <- c("CLICODIGO","VARILUX X SERIES","VARILUX E SERIES","VARILUX PHYSIO 360","VARILUX COMFORT MAX","VARILUX COMFORT","VARILUX LIBERTY 360","VARILUX LIBERTY","VARILUX DIGITIME","VARILUX ROAD PILOT/SPORT")

clitbp_vlx_3 <- clitbp_vlx_2 %>% 
            select(CLICODIGO,TBPDESCRICAO,TBPDESC2) %>% 
             dcast(CLICODIGO ~ TBPDESCRICAO,na.rm = TRUE) %>% as.data.frame() %>% 
              .[,c_vlx_order] 


                View(clitbp_vlx_3)

## GRUPOS
                
g_vlx_order <- c("GCLCODIGO","VARILUX X SERIES","VARILUX E SERIES","VARILUX PHYSIO 360","VARILUX COMFORT MAX","VARILUX COMFORT","VARILUX LIBERTY 360","VARILUX LIBERTY","VARILUX DIGITIME","VARILUX ROAD PILOT/SPORT")
                

g_clitbp_vlx_2 <- 
left_join(clien %>% 
            select(CLICODIGO,GCLCODIGO) %>% 
             filter(!is.na(GCLCODIGO)),clitbp_vlx_2 ,by="CLICODIGO") %>% 
               dcast(GCLCODIGO ~ TBPDESCRICAO,value.var = "TBPDESC2",fun.aggregate = max) %>% 
                .[,g_vlx_order] 

View(g_clitbp_vlx_2)
  

  
## KODAK ======================================  
                
## LOJAS

clitbp_kdk <- dbGetQuery(con2,"SELECT CLICODIGO,TBPCODIGO,TBPDESC2 FROM CLITBP
                     WHERE TBPCODIGO IN (201,202)")

clitbp_kdk_2 <- left_join(clitbp_kdk,aux, by="TBPCODIGO") 

View(clitbp_kdk_2)

left_join(clitbp,tabpreco,by="TBPCODIGO") %>% View()

corder_kdk <- c("CLICODIGO","KODAK EASY PLUS","KODAK EASY","KODAK UP","KODAK SOFTWEAR","KODAK PRECISE","KODAK NETWORK","KODAK UNIQUE","KODAK SINGLE RX")

clitbp_kdk_3 <- clitbp_kdk_2 %>% 
  select(CLICODIGO,TBPDESCRICAO,TBPDESC2) %>% 
  dcast(CLICODIGO ~ TBPDESCRICAO,na.rm = TRUE) %>% as.data.frame() %>% 
  .[,corder_kdk] 


View(clitbp_kdk_3)

## GRUPOS

g_order_kdk <- c("GCLCODIGO","KODAK EASY PLUS","KODAK EASY","KODAK UP","KODAK SOFTWEAR","KODAK PRECISE","KODAK NETWORK","KODAK UNIQUE","KODAK SINGLE RX")

g_clitbp_kdk <- 

left_join(clien %>% 
            select(CLICODIGO,GCLCODIGO) %>% 
            filter(!is.na(GCLCODIGO)),clitbp_kdk_2 ,by="CLICODIGO") %>% 
  dcast(GCLCODIGO ~ TBPDESCRICAO,value.var = "TBPDESC2",fun.aggregate = max) %>% 
  .[,g_order_kdk] 


View(g_clitbp_kdk)


## KODAK VS ===============================================================================  

clitbp_kdk_vs <- dbGetQuery(con2,"SELECT DISTINCT CLICODIGO,TBPCODIGO FROM CLITBP")


tbpprodu9 <- dbGetQuery(con2,"

WITH PROD AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE (PRODESCRICAO LIKE '%KODAK%' AND
PRODESCRICAO NOT LIKE '%SINGLE%') AND GR2CODIGO=3
 AND PROSITUACAO='A'),
 
TAB_ACORDO AS (SELECT TBPCODIGO,TBPDESCRICAO FROM TABPRECO WHERE TBPSITUACAO='A' AND 
                       (TBPDTVALIDADE >='TODAY' OR TBPDTVALIDADE IS NULL) AND TBPCODIGO<>202 AND
                        TBPTABCOMB='N')

SELECT DISTINCT P.TBPCODIGO,P.PROCODIGO,PRODESCRICAO,TBPPCDESCTO,TBPPCOVENDA,TBPPCDESCTO2,TBPPCOVENDA2
                              FROM TBPPRODU P
                               INNER JOIN PROD PR ON PR.PROCODIGO=P.PROCODIGO
                                INNER JOIN TAB_ACORDO TA ON P.TBPCODIGO=TA.TBPCODIGO")


inner_join(tbpprodu9,clitbp_kdk_vs,by="TBPCODIGO") %>% View()



## EYEZEN ===============================================================================  

## LOJAS

clitbp_eyez <- dbGetQuery(con2,"SELECT CLICODIGO,TBPCODIGO,TBPDESC2 FROM CLITBP
                     WHERE TBPCODIGO IN (309)")

clitbp_eyez_2 <- left_join(clitbp_eyez,aux, by="TBPCODIGO") 


corder_eyez <- c("CLICODIGO","EYEZEN")

clitbp_eyez_3 <- clitbp_eyez_2 %>% 
  select(CLICODIGO,TBPDESCRICAO,TBPDESC2) %>% 
  dcast(CLICODIGO ~ TBPDESCRICAO,na.rm = TRUE) %>% as.data.frame() %>% 
  .[,corder_eyez] 


View(clitbp_eyez_3)

## GRUPOS

g_order_eyez <- c("GCLCODIGO","EYEZEN")


g_clitbp_eyez <- 
  inner_join(clien %>% 
              select(CLICODIGO,GCLCODIGO) %>% 
              filter(!is.na(GCLCODIGO)),clitbp_eyez_2 ,by="CLICODIGO") %>% 
  dcast(GCLCODIGO ~ TBPDESCRICAO,value.var = "TBPDESC2",fun.aggregate = max) %>% 
  .[,g_order_eyez] 


View(g_clitbp_eyez)





