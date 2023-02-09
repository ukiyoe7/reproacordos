
tabpreco <- dbGetQuery(con2,"SELECT * FROM TABPRECO")

View(tabpreco)


tbpprodu2 <- dbGetQuery(con2,"SELECT TBPCODIGO,P.PROCODIGO,PRODESCRICAO,PROSITUACAO 
                              FROM TBPPRODU P
                               LEFT JOIN (SELECT PROCODIGO,PRODESCRICAO,PROSITUACAO FROM PRODU)A ON A.PROCODIGO=P.PROCODIGO
                     WHERE TBPCODIGO IN (102)")

View(tbpprodu)


tbpprodu3 <- dbGetQuery(con2,"SELECT TBPCODIGO,P.PROCODIGO,PRODESCRICAO,PROSITUACAO 
                              FROM TBPPRODU P
                               LEFT JOIN (SELECT PROCODIGO,PRODESCRICAO,PROSITUACAO FROM PRODU)A ON A.PROCODIGO=P.PROCODIGO
                     WHERE TBPCODIGO IN (103)")

View(tbpprodu)


tbpprodu4 <- dbGetQuery(con2,"SELECT TBPCODIGO,P.PROCODIGO,PRODESCRICAO,PROSITUACAO 
                              FROM TBPPRODU P
                               LEFT JOIN (SELECT PROCODIGO,PRODESCRICAO,PROSITUACAO FROM PRODU)A ON A.PROCODIGO=P.PROCODIGO
                     WHERE TBPCODIGO IN (104)")

View(tbpprodu)


tbpprodu5 <- dbGetQuery(con2,"SELECT TBPCODIGO,P.PROCODIGO,PRODESCRICAO,PROSITUACAO 
                              FROM TBPPRODU P
                               LEFT JOIN (SELECT PROCODIGO,PRODESCRICAO,PROSITUACAO FROM PRODU)A ON A.PROCODIGO=P.PROCODIGO
                     WHERE TBPCODIGO IN (105)")

View(tbpprodu)


tbpprodu6 <- dbGetQuery(con2,"SELECT TBPCODIGO,P.PROCODIGO,PRODESCRICAO,PROSITUACAO 
                              FROM TBPPRODU P
                               LEFT JOIN (SELECT PROCODIGO,PRODESCRICAO,PROSITUACAO FROM PRODU)A ON A.PROCODIGO=P.PROCODIGO
                     WHERE TBPCODIGO IN (201)")

View(tbpprodu6)


tbpprodu7 <- dbGetQuery(con2,"SELECT TBPCODIGO,P.PROCODIGO,PRODESCRICAO,PROSITUACAO 
                              FROM TBPPRODU P
                               LEFT JOIN (SELECT PROCODIGO,PRODESCRICAO,PROSITUACAO FROM PRODU)A ON A.PROCODIGO=P.PROCODIGO
                     WHERE TBPCODIGO IN (202)")

View(tbpprodu7)


tbpprodu8 <- dbGetQuery(con2,"SELECT TBPCODIGO,P.PROCODIGO,PRODESCRICAO,PROSITUACAO 
                              FROM TBPPRODU P
                               LEFT JOIN (SELECT PROCODIGO,PRODESCRICAO,PROSITUACAO FROM PRODU)A ON A.PROCODIGO=P.PROCODIGO
                     WHERE TBPCODIGO IN (309)")

View(tbpprodu8)




anti_join(tbpprodu7,tbpprodu6,by="PROCODIGO") %>% View()


metas <- read.table(text = read_clip(), header = TRUE, sep = "\t") 
