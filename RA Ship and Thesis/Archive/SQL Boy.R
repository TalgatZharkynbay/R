library(RPostgreSQL)


drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, 
                 dbname="",
                 host="",
                 #port=1234,
                 user="",
                 password="")
dbListTables(con) 

myTableTest <-dbGetQuery(con, statement = "SELECT filings_filer.cik, filings_edgarindexentry.company_name, 
filings_newposition.cusip_id, stocks_newstock.name, 
filings_cusip.stock_id, SUM(filings_newposition.value) 
AS total_value, SUM(filings_newposition.ssh_prn_amt) AS total_count 
FROM filings_newposition INNER JOIN filings_cusip 
ON (filings_newposition.cusip_id = filings_cusip.cusip) 
INNER JOIN filings_filing13f ON (filings_newposition.filing_id = filings_filing13f.id) 
INNER JOIN filings_filer ON (filings_filing13f.filer_id = filings_filer.id) 
LEFT OUTER JOIN filings_edgarindexentry 
ON (filings_filing13f.edgar_index_entry_id = filings_edgarindexentry.id) 
LEFT OUTER JOIN stocks_newstock ON (filings_cusip.stock_id = stocks_newstock.ticker) 
WHERE UPPER(filings_filing13f.quarter_id::text) = UPPER('2020Q2')
GROUP BY filings_filer.cik, filings_edgarindexentry.company_name, 
filings_newposition.cusip_id, stocks_newstock.name, filings_cusip.stock_id ,filings_filing13f.quarter_id 
ORDER BY total_value DESC") 

write.csv(myTableTest, "C:/Users/talga/Desktop/2020Q2.csv")






# myTableGood <-dbGetQuery(con, statement = "SELECT filings_filer.cik, filings_edgarindexentry.company_name, 
# filings_newposition.cusip_id, stocks_newstock.name, 
# filings_cusip.stock_id, SUM(filings_newposition.value) 
# AS total_value, SUM(filings_newposition.ssh_prn_amt) AS total_count 
# FROM filings_newposition INNER JOIN filings_cusip 
# ON (filings_newposition.cusip_id = filings_cusip.cusip) 
# INNER JOIN filings_filing13f ON (filings_newposition.filing_id = filings_filing13f.id) 
# INNER JOIN filings_filer ON (filings_filing13f.filer_id = filings_filer.id) 
# LEFT OUTER JOIN filings_edgarindexentry 
# ON (filings_filing13f.edgar_index_entry_id = filings_edgarindexentry.id) 
# LEFT OUTER JOIN stocks_newstock ON (filings_cusip.stock_id = stocks_newstock.ticker) 
# WHERE (filings_newposition.cusip_id = '464287101' 
# AND UPPER(filings_filing13f.quarter_id::text) = UPPER('2019Q3')) 
# GROUP BY filings_filer.cik, filings_edgarindexentry.company_name, 
# filings_newposition.cusip_id, stocks_newstock.name, filings_cusip.stock_id 
# ORDER BY total_value DESC  LIMIT 50") 
