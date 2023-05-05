
library(readxl)
library(DBI)
library(dplyr)
library(glue)

con2 <- dbConnect(odbc::odbc(), "reproreplica")
con3 <- dbConnect(odbc::odbc(), "repro_prod", timeout = 10)

## GET DATA

current_month <- toupper(format(Sys.Date(), "%B"))

file_path <- gsub("/MAI/", paste0("/", current_month, "/"), "~/ACORDOS/2023/MAI/PRORROGAR.xlsx")

PRORROGAR <- read_excel(file_path, col_types = c("numeric", "date"))

PRORROGAR_2 <-
PRORROGAR %>% mutate(DATA= format(DATA, "%d.%m.%Y"))



## LOOP SQL


x <- data.frame(TBPCODIGO=NA,DATA=NA)

for (i in 1:nrow(PRORROGAR_2)) {
  x[i,] <- PRORROGAR_2[i,]
  query_acordos <- paste("UPDATE TABPRECO SET TBPDTVALIDADE='",x[i,"DATA"],"' WHERE TBPCODIGO=",x[i,"TBPCODIGO"],";", sep = "")
  dbSendQuery(con3,query_acordos)
}



## VIEW

query_acordos_2 <- glue_sql("SELECT TBPCODIGO,TBPDTVALIDADE FROM TABPRECO WHERE TBPCODIGO IN ({PRORROGAR$TBPCODIGO*})")

query_acordos_3<-  dbGetQuery(con3,query_acordos_2) 

View(query_acordos_3)






