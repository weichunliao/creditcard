# install.packages("readxl")
library("readxl")

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

# data2 <- read_excel("/Users/weichun/Desktop/creditcard/data/103/1.xlsx")
# sum(as.numeric(data[8:43,2]))
# # paste(data[4, 10], data[5, 10], data[6, 10], data[7, 10], sep="")
# paste(data[4:7, 10], collapse="")


# m <- as.matrix(data[8:43,2:12])
# m2 <- t(apply(m, 1,as.numeric))
# bank_list <- data[8:43,1]

