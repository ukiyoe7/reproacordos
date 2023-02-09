## TABELAS ACORDOS

library(DBI)
library(tidyverse)
library(googlesheets4)
library(xlsx)
library(reshape2)




clitbp <- dbGetQuery(con2,"SELECT CLICODIGO,TBPCODIGO,TBPDESC2 FROM CLITBP
                     WHERE TBPCODIGO IN (101,102)")

clitbp2 <- left_join(clitbp,aux, by="TBPCODIGO") 

View(clitbp2)

left_join(clitbp,tabpreco,by="TBPCODIGO") %>% View()

corder <- c("CLICODIGO","VARILUX X SERIES","VARILUX E SERIES","VARILUX PHYSIO 360","VARILUX COMFORT MAX","VARILUX COMFORT","VARILUX LIBERTY 360","VARILUX LIBERTY","VARILUX DIGITIME","VARILUX ROAD PILOT/SPORT")

clitbp3 <- clitbp2 %>% 
            select(CLICODIGO,TBPDESCRICAO,TBPDESC2) %>% 
             dcast(CLICODIGO ~ TBPDESCRICAO,na.rm = TRUE) %>% as.data.frame() %>% 
              .[,corder] %>% 
                View()


View(clitbp)


