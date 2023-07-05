
library(readxl)
library(DBI)
library(dplyr)
library(glue)


con2 <- dbConnect(odbc::odbc(), "reproreplica")
con3 <- dbConnect(odbc::odbc(), "repro_prod", timeout = 10)

## GET AND SAVE DATA

current_date <- format(Sys.Date(), "%d_%m_%y")

TABPRECO_050723_ANTES <- dbGetQuery(con2,"SELECT TBPCODIGO,TBPDESCRICAO,TBPDTINICIO,TBPDTVALIDADE FROM TABPRECO ")

write.csv2(TABPRECO_050723_ANTES,file = "C:\\Users\\Repro\\Documents\\ACORDOS\\2023\\JULHO\\TABPRECO_050723_ANTES.csv", row.names = FALSE ,na = "")


## GET DATA

current_month <- toupper(format(Sys.Date(), "%B"))

file_path <- gsub("/JUL/", paste0("/", current_month, "/"), "~/ACORDOS/2023/JUL/PRORROGAR.xlsx")

PRORROGAR <- read_excel(file_path, col_types = c("numeric", "date"))

PRORROGAR_2 <-
PRORROGAR %>% mutate(DATA= format(DATA, "%d.%m.%Y"))


## LOOP SQL

rm(query_acordos)

x <- data.frame(TBPCODIGO=NA,DATA=NA)

for (i in 1:nrow(PRORROGAR_2)) {
  x[i,] <- PRORROGAR_2[i,]
  query_acordos <- paste("UPDATE TABPRECO SET TBPDTVALIDADE='",x[i,"DATA"],"' WHERE TBPCODIGO=",x[i,"TBPCODIGO"],";", sep = "")
  dbSendQuery(con3,query_acordos)
}



## VIEW

query_acordos_2 <- glue_sql("SELECT TBPCODIGO,TBPDTVALIDADE FROM TABPRECO WHERE TBPCODIGO IN ({PRORROGAR_2$TBPCODIGO*})")

query_acordos_3<-  dbGetQuery(con3,query_acordos_2) 

View(query_acordos_3)


## SAVE DATA


TABPRECO_050723_DEPOIS <- dbGetQuery(con2,"SELECT TBPCODIGO,TBPDESCRICAO,TBPDTINICIO,TBPDTVALIDADE FROM TABPRECO ")

  write.csv2(TABPRECO_050723_DEPOIS,file = "C:\\Users\\Repro\\Documents\\ACORDOS\\2023\\JULHO\\TABPRECO_050723_DEPOIS.csv", row.names = FALSE ,na = "")



