
library(DBI)
library(dplyr)
library(googlesheets4)
con2 <- dbConnect(odbc::odbc(), "reproreplica")



sales <- dbGetQuery(con2,"
      
     WITH 
                                 
  FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),
      
      PED AS (SELECT CLICODIGO
                              FROM PEDID P
                               INNER JOIN FIS ON P.FISCODIGO1=FIS.FISCODIGO
                                 WHERE PEDDTBAIXA BETWEEN '01.02.2023' AND 'TODAY' 
                                  AND PEDSITPED<>'C'
                                   AND PEDLCFINANC IN ('S', 'L','N')) 
      
                              SELECT  CLICODIGO,
                                 SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
                                  FROM PDPRD PD
                                   INNER JOIN PED P ON PD.ID_PEDIDO=P.ID_PEDIDO
                                    GROUP BY 1")  
 

View(sales)