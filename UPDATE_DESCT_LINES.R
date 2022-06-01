##  UPDATE DESCTOS
## SANDRO JAKOSKA

library(DBI)
library(tidyverse)

con3 <- dbConnect(odbc::odbc(), "repro_prod", timeout = 10)
con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)

## VARILUX ==========================================================================================

## CLIENTS
cli_vlx <- dbGetQuery(con2,"SELECT * FROM CLITBP 
WHERE TBPCODIGO IN (100,101,102,103,104,105)") 


view(cli_vlx)


desct_vlx <- AJUSTE_DESCONTOS_SETOR3_VLX %>% filter(`ALTERAR DESCONTO`!='NA') %>%  
  filter(`ALTERAR DESCONTO`!='?') %>% mutate(`ALTERAR DESCONTO`=as.numeric(`ALTERAR DESCONTO`)*100) 

inner_join(cli_vlx,desct_vlx,by="CLICODIGO") %>% View()


update_vlx <- inner_join(cli_vlx,desct_vlx,by="CLICODIGO") %>% .[,c(1,2,14)] %>% 
  mutate(TBPDESC=`ALTERAR DESCONTO`) %>% mutate(TBPDESC2=`ALTERAR DESCONTO`) %>% .[,-3] 

View(update_vlx)


y <- data.frame(CLICODIGO=NA,TBPCODIGO=NA,TBPDESC=NA,TBPDESC2=NA)


for (i in 1:nrow(update_vlx)) {
  y[i,] <- update_vlx[i,]
  query1 <- paste("UPDATE CLITBP SET TBPDESC=",y[i,"TBPDESC"],",TBPDESC2=",y[i,"TBPDESC2"]," WHERE TBPCODIGO=",y[i,"TBPCODIGO"]," AND CLICODIGO=",y[i,"CLICODIGO"],";", sep = "")
  dbSendQuery(con3,query1)
}





## KODAK  ==========================================================================================

## CLIENTS
cli_kdk <- dbGetQuery(con2,"SELECT * FROM CLITBP WHERE
            TBPCODIGO IN (201,202)") 

view(cli_kdk)


desct_kdk <- AJUSTE_DESCONTOS_SETOR3_KDK %>% filter(`ALTERAR DESCONTO`!='NA') %>%  
  filter(`ALTERAR DESCONTO`!='?') %>% mutate(`ALTERAR DESCONTO`=as.numeric(`ALTERAR DESCONTO`)*100) 


update_kdk <- inner_join(cli_kdk,desct_kdk,by="CLICODIGO") %>% .[,c(1,2,14)] %>% 
  mutate(TBPDESC=`ALTERAR DESCONTO`) %>% mutate(TBPDESC2=`ALTERAR DESCONTO`) %>% .[,-3] 

View(update_kdk)



w <- data.frame(CLICODIGO=NA,TBPCODIGO=NA,TBPDESC=NA,TBPDESC2=NA)


for (i in 1:nrow(update_kdk)) {
  w[i,] <- update_kdk[i,]
  query2 <- paste("UPDATE CLITBP SET TBPDESC=",w[i,"TBPDESC"],",TBPDESC2=",w[i,"TBPDESC2"]," WHERE TBPCODIGO=",w[i,"TBPCODIGO"]," AND CLICODIGO=",w[i,"CLICODIGO"],";", sep = "")
  dbSendQuery(con3,query2)
}



## MARCA PROPRIA ==========================================================================================

## CLIENTS
cli_mp <- dbGetQuery(con2,"SELECT * FROM CLITBP 
WHERE TBPCODIGO IN (301,302,303,304,305,306,308)") 


View(cli_mp)


desct_mp <- AJUSTE_DESCONTOS_SETOR3_MP2 %>% filter(`ALTERAR DESCONTO`!='NA') %>%  
  filter(`ALTERAR DESCONTO`!='?') %>% mutate(`ALTERAR DESCONTO`=as.numeric(`ALTERAR DESCONTO`)*100) 

cli_desct_mp <- inner_join(cli_mp,desct_mp)

View(cli_desct_mp)

n_distinct(cli_desct_mp$CLICODIGO)


update_mp2 <- inner_join(cli_mp,desct_mp,by="CLICODIGO") %>% .[,c(1,2,7)] %>% 
  mutate(TBPDESC=`ALTERAR DESCONTO`) %>% mutate(TBPDESC2=`ALTERAR DESCONTO`) %>% .[,-3]


q <- data.frame(CLICODIGO=NA,TBPCODIGO=NA,TBPDESC=NA,TBPDESC2=NA)


for (i in 1:nrow(update_mp2)) {
  q[i,] <- update_mp2[i,]
  query3 <- paste("UPDATE CLITBP SET TBPDESC=",q[i,"TBPDESC"],",TBPDESC2=",q[i,"TBPDESC2"]," WHERE TBPCODIGO=",q[i,"TBPCODIGO"]," AND CLICODIGO=",q[i,"CLICODIGO"],";", sep = "")
  dbSendQuery(con3,query3)
}

dbSendQuery(con3,query3)


## DESCONTO GERAL ==========================================================================================

## CLIENTS
cli_geral <- dbGetQuery(con2,"SELECT DISTINCT 
    CLICODIGO,
    CLIPCDESCPRODU
    FROM CLIEN
    WHERE CLICLIENTE='S'") 

View(cli_geral)


desct_geral <- AJUSTE_DESCONTOS_SETOR3_GERAL %>% filter(`ALTERAR DESCONTO`!='NA') %>%  
  filter(`ALTERAR DESCONTO`!='?') %>% mutate(`ALTERAR DESCONTO`=as.numeric(`ALTERAR DESCONTO`)*100) 

View(cli_desct_geral)

n_distinct(desct_geral$CLICODIGO)


update_geral <- inner_join(cli_geral,desct_geral,by="CLICODIGO") %>% .[,c(1,2,10)] %>% 
  mutate(CLIPCDESCPRODU=`ALTERAR DESCONTO`) %>% .[,-3]

View(update_geral)


l <- data.frame(CLICODIGO=NA,CLIPCDESCPRODU=NA)


for (i in 1:nrow(update_geral)) {
  l[i,] <- update_geral[i,]
  query4 <- paste("UPDATE CLIEN SET CLIPCDESCPRODU=",l[i,"CLIPCDESCPRODU"]," WHERE CLICODIGO=",l[i,"CLICODIGO"]," ;", sep = "")
  dbSendQuery(con3,query4)
  
}



