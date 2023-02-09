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

View(clitbp2)

left_join(clitbp,tabpreco,by="TBPCODIGO") %>% View()

corder <- c("CLICODIGO","VARILUX X SERIES","VARILUX E SERIES","VARILUX PHYSIO 360","VARILUX COMFORT MAX","VARILUX COMFORT","VARILUX LIBERTY 360","VARILUX LIBERTY","VARILUX DIGITIME","VARILUX ROAD PILOT/SPORT")

clitbp3 <- clitbp2 %>% 
            select(CLICODIGO,TBPDESCRICAO,TBPDESC2) %>% 
             dcast(CLICODIGO ~ TBPDESCRICAO,na.rm = TRUE) %>% as.data.frame() %>% 
              .[,corder] 


                View(clitbp3)

## GRUPOS
                
gorder <- c("GCLCODIGO","VARILUX X SERIES","VARILUX E SERIES","VARILUX PHYSIO 360","VARILUX COMFORT MAX","VARILUX COMFORT","VARILUX LIBERTY 360","VARILUX LIBERTY","VARILUX DIGITIME","VARILUX ROAD PILOT/SPORT")
                

left_join(clien %>% 
            select(CLICODIGO,GCLCODIGO) %>% 
             filter(!is.na(GCLCODIGO)),clitbp_vlx_2 ,by="CLICODIGO") %>% 
               dcast(GCLCODIGO ~ TBPDESCRICAO,value.var = "TBPDESC2",fun.aggregate = max) %>% 
                .[,gorder] %>% View()
  


left_join(clien %>% 
            select(CLICODIGO,GCLCODIGO) %>% 
            filter(!is.na(GCLCODIGO)),clitbp_vlx_2 ,by="CLICODIGO") %>% 
  dcast(GCLCODIGO ~ TBPDESCRICAO,value.var = "TBPDESC2",fun.aggregate = max) %>% 
  .[,gorder] %>% View()

  
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

gorder <- c("GCLCODIGO","KODAK EASY PLUS","KODAK EASY","KODAK UP","KODAK SOFTWEAR","KODAK PRECISE","KODAK NETWORK","KODAK UNIQUE","KODAK SINGLE RX")


left_join(clien %>% 
            select(CLICODIGO,GCLCODIGO) %>% 
            filter(!is.na(GCLCODIGO)),clitbp_kdk_2 ,by="CLICODIGO") %>% 
  dcast(GCLCODIGO ~ TBPDESCRICAO,value.var = "TBPDESC2",fun.aggregate = max) %>% 
  .[,gorder] %>% View()



left_join(clien %>% 
            select(CLICODIGO,GCLCODIGO) %>% 
            filter(!is.na(GCLCODIGO)),clitbp_vlx_2 ,by="CLICODIGO") %>% 
  dcast(GCLCODIGO ~ TBPDESCRICAO,value.var = "TBPDESC2",fun.aggregate = max) %>% 
  .[,gorder] %>% View()

                

