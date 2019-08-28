
# Install Package 'readxl' untuk membaca data input bertipe .xls dan .xlsx

install.packages('readxl')



# Load package 'readxl'

library('readxl')


#membaca file
online_retail = read_excel('C:/Users/ide2/Documents/ANGGUN_MBA/homework_formating/Online_Retail.xlsx')

#melihat isi file


head(online_retail)

# preview first 6 rows of data
head(online_retail)

#melihat struktur data
str(online_retail)


#melihat summary data
summary(online_retail)

#melihat missing value
library(DataExplorer)
plot_missing(online_retail)

#terdapat missing pada field 24.93 %

#drop data dengan customerid = Na
online_retail_clean = online_retail[!is.na(online_retail$CustomerID),]

#cek ulang missing value customerid dengan data clean
plot_missing(online_retail_clean)

#melihat summary data clean
summary(online_retail_clean)

#membuat summary data yang berisi customerid, recency, frequency, dan monetary

# Recency   : jumlah hari ini s.d. terakhir bertransaksi (dalam hari)
# Frequency : jumlah transaksi yang terjadi dalam 6 bulan terakhir 
# Monetary  : jumlah uang yang dibelanjakan oleh Customer ID unik

library('tidyverse')

frequency <- online_retail_clean %>% group_by(CustomerID)%>% summarise(frequency = n_distinct(InvoiceNo))
head(frequency)

monetary <- online_retail %>% group_by(CustomerID) %>% summarise(monetary=sum(UnitPrice*Quantity))                                               

head(monetary)



library('lubridate')
recency <- online_retail %>% group_by(CustomerID) %>% arrange(desc(InvoiceDate)) %>%   filter(row_number()==1) %>% mutate(recency = as.numeric(as.duration(interval(InvoiceDate,ymd("2011-12-31"))))/86400) %>% select(CustomerID, recency)  

head(recency)


#melakukan proses join untuk proses penggabungan
df_rfm <- recency %>% left_join(frequency,by="CustomerID") %>% left_join(monetary,by="CustomerID")
head(df_rfm)

#melihat summary data
summary (df_rfm)

#write result 
write.csv(df_rfm, "C:/Users/ide2/Documents/ANGGUN_MBA/homework_formating/online_result_formating_dian_anggun.csv", quote = F, row.names= F)


