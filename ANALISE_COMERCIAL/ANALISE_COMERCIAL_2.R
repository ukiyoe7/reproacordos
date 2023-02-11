## TABELAS ACORDOS

library(DBI)
library(tidyverse)
library(googlesheets4)
library(xlsx)
library(reshape2)

con2 <- dbConnect(odbc::odbc(), "reproreplica")

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

## LOJAS

clitbp_kdk_vs <- dbGetQuery(con2,"SELECT DISTINCT CLICODIGO,TBPCODIGO FROM CLITBP")


tbpprodu9 <- dbGetQuery(con2,"

WITH PROD AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE (PRODESCRICAO LIKE '%KODAK%' AND
PRODESCRICAO NOT LIKE '%SINGLE%') AND GR2CODIGO=3
 AND PROSITUACAO='A'),
 
TAB_ACORDO AS (SELECT TBPCODIGO,TBPDESCRICAO FROM TABPRECO WHERE TBPSITUACAO='A' AND 
                       (TBPDTVALIDADE >='TODAY' OR TBPDTVALIDADE IS NULL) AND TBPCODIGO<>202 AND
                        TBPTABCOMB='N' AND TBPDESCRICAO NOT LIKE '%PROMO DO MES%')

SELECT DISTINCT P.TBPCODIGO,P.PROCODIGO,PRODESCRICAO,TBPPCDESCTO,TBPPCOVENDA,TBPPCDESCTO2,TBPPCOVENDA2
                              FROM TBPPRODU P
                               INNER JOIN PROD PR ON PR.PROCODIGO=P.PROCODIGO
                                INNER JOIN TAB_ACORDO TA ON P.TBPCODIGO=TA.TBPCODIGO")


clitbp_kdk_vs_2  <- inner_join(tbpprodu9,clitbp_kdk_vs,by="TBPCODIGO") 

View(clitbp_kdk_vs_2) 


clitbp_kdk_vs_3 <- 
inner_join(tbpprodu9,clitbp_kdk_vs,by="TBPCODIGO") %>% 
   group_by(CLICODIGO) %>% 
    .[,c(1,3,2)] %>% 
    summarize(TBPPCDESCTO2=round(mean(TBPPCDESCTO2))) 

View(clitbp_kdk_vs_3)

##GRUPOS

clitbp_kdk_vs_4 <- 
inner_join(clien %>% 
            select(CLICODIGO,GCLCODIGO) %>% 
            filter(!is.na(GCLCODIGO)),clitbp_kdk_vs_3 ,by="CLICODIGO") %>% 
              group_by(GCLCODIGO) %>% 
                summarize(TBPPCDESCTO2=round(mean(TBPPCDESCTO2,na.rm = TRUE)))


View(clitbp_kdk_vs_4)
  


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


#LA CRIZAL =========================

clitbp_LA <- dbGetQuery(con2,"SELECT DISTINCT CLICODIGO,TBPCODIGO FROM CLITBP")

tbpprodu10 <- dbGetQuery(con2,"

WITH PROD AS (SELECT PROCODIGO2 FROM PRODU WHERE PRODESCRICAO LIKE '%CRIZAL%' AND
LEFT(PROCODIGO2,2)='LA' AND PROSITUACAO='A'),
 
TAB_ACORDO AS (SELECT TBPCODIGO,TBPDESCRICAO FROM TABPRECO WHERE TBPSITUACAO='A' AND 
                       (TBPDTVALIDADE >='TODAY' OR TBPDTVALIDADE IS NULL) AND
                        TBPTABCOMB='N' AND TBPDESCRICAO NOT LIKE '%PROMO DO MES%'),
                        
CLITB AS (SELECT CLICODIGO,TBPCODIGO FROM CLITBP)                         

SELECT DISTINCT P.TBPCODIGO,
                  CLICODIGO,
                   P.PROCODIGO,
                    TBPPCDESCTO,
                     TBPPCOVENDA2,
                      TBPPCDESCTO2,
                       TBPPCOVENDA2
                        FROM TBPPRODU P
                               INNER JOIN PROD PR ON PR.PROCODIGO2=P.PROCODIGO
                                INNER JOIN TAB_ACORDO TA ON P.TBPCODIGO=TA.TBPCODIGO
                                  LEFT JOIN CLITB TB ON P.TBPCODIGO=TB.TBPCODIGO")

View(tbpprodu10)

clitbp_crizal<-
tbpprodu10 %>% select(CLICODIGO,TBPPCDESCTO2) %>% 
   group_by(CLICODIGO) %>% 
   summarize(`CRIZAL VS`=round(mean(TBPPCDESCTO2,na.rm = TRUE),2))

View(clitbp_crizal)


clitbp_crizal_grupo <- 
inner_join(clitbp_crizal,clien %>% 
            select(CLICODIGO,GCLCODIGO) %>% 
            filter(!is.na(GCLCODIGO)),by="CLICODIGO") %>%
             group_by(GCLCODIGO) %>% 
              summarize(`CRIZAL VS`=round(mean(`CRIZAL VS`,na.rm = TRUE),2))

View(clitbp_crizal_grupo)
  
  
  
  
  
  
  
  
  
  


