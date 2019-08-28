# formating
#membaca file
folder <- "C:/Users/ide2/Documents/ANGGUN_MBA/homework_formating/"
file_list <- list.files(path=folder, pattern="*.csv") 


for (i in 1:length(file_list)){
  
  assign(file_list[i],read.csv(paste(folder, file_list[i], sep='')))
  
}


#melihat isi file
online_retail = Online_Retail.csv

online_retail

# preview first 6 rows of data
head(online_retail)

#melihat summary data
summary(online_retail)

#melihat missing value

plot_missing(online_retail)

# load library
library(tidyverse)
library(lubridate)
library(DataExplorer)

# Recency   : jumlah hari ini s.d. terakhir bertransaksi (dalam hari)
# Frequency : jumlah transaksi yang terjadi dalam 6 bulan terakhir 
# Monetary  : jumlah uang yang dibelanjakan oleh Customer ID unik


frequency <- online_retail %>% group_by(CustomerID = as.integer(CustomerID))%>% summarise(frequency = n_distinct(CustomerID))
frequency



monetary <- online_retail %>% group_by(CustomerID = as.numeric(CustomerID)) %>% summarise(monetary=sum(as.numeric(UnitPrice)*as.numeric(Quantity)))                                               

monetary




recency <- online_retail %>% group_by(CustomerID) %>% arrange(desc(InvoiceDate)) %>%   filter(row_number()==1) %>% mutate(recency = as.numeric(as.duration(interval(InvoiceDate,ymd("2011-12-31"))))/86400) %>% select(CustomerID, recency)  

recency


#melakukan proses join untuk proses penggabungan
df_rfm <- recency %>% left_join(frequency,by="CustomerID") %>% left_join(monetary,by="CustomerID")
df_rfm

#melihat summary data
summary (df_rfm)




