# designing a meta-regression model

# require packages
require(readxl);require(data.table)

# read in database from excel file 
d1 <- as.data.table(read_xlsx('../02 data/220621 dataset_s1.xlsx',sheet = 'Dataset_s1'))

# columns that are character or numeric
cols.c <- colnames(d1)[grepl('paper|treatm|prec|temp|exp|altit|name|type|unit|code|source',colnames(d1))]
cols.n <- colnames(d1)[!colnames(d1) %in% cols.c]

# ensure that columns are the correct type
d1[,c(cols.c) := lapply(.SD,as.character),.SDcols = cols.c]
d1[,c(cols.n) := lapply(.SD,as.numeric),.SDcols = cols.n]

