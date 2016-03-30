# install.packages("readxl")
library("readxl")

# ###############
setwd("/Users/weichun/Desktop/creditcard")
data_list <- readRDS("data_list.rds")
datalist_by_year <- readRDS("datalist_by_year.rds")
aggr_by_month <- readRDS("aggr_by_month.rds")
sum_by_year <- readRDS("sum_by_year.rds")
# ###############

# sys.setlocale(category = "LC_ALL", locale = "zh_TW.UTF-8")
# read data
data <- read_excel("/Users/weichun/Desktop/creditcard/data/105/10501+信用卡重要資訊揭露.xlsx")

m <- as.matrix(data)
m[ is.na(m) ] <- ""
colname_index <- which( m[, 1] == "金融機構名稱", arr.ind=T )
colname_index2 <- tail(which( m[1:10, 1] == "", arr.ind=T ), 1)
data_col_names <- apply(m[colname_index:colname_index2, ], 2, function(x) paste(x, collapse=""))
data_col_names <- gsub("[[:space:]]", "", data_col_names)
colnames(m) <- data_col_names

total_idx <- which(m == "總計", arr.ind=T)[1, 1]
m <- m[((colname_index2+1):total_idx),]

data_row_names <- m[, 1]
rownames(m) <- data_row_names

col_count <- ncol(m)
m <- m[,(-1)]
m[ is.na(m) ] <- 0
m <- t(apply(m, 1, function(x) as.numeric(x)))

##### 欄位數: 100-8之前13欄，之後14欄 ######

for (i in 94:104) {
	for (j in 1:12) {
		cat(i, "年\n", j, "月", "\n")
		cat("銀行數:", nrow(data_list[[i]][[j]]), ", 欄位數:", ncol(data_list[[i]][[j]]), "\n")
	}
}

#####

# aggregate data

data_by_year <- vector("list", 11)
# names(data_by_year) <- c(94:104)
for (i in 94:104) {

	if (i > 99) {
		for (j in 1:12) {
			data_list[[i]][[j]] <- data_list[[i]][[j]][,colnames(data_list[[i]][[j]])!= "未到期分期付款餘額"]
		}
	}

	temp <- t(sapply(data_list[[i]], function(x) tail(x, 1), simplify=TRUE))
	# cat(temp, "\n")
	colnames(temp) <- colnames(data_list[[94]][[1]])

	# cat(ncol(temp),"\n")

	year <- rep(i, 12)
	month <- c(1:12)
	temp <- cbind(year, month, temp)
	data_by_year[[(i-93)]] <- temp
}
do.call(rbind, data_by_year)






