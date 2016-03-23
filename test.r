# install.packages("readxl")
library("readxl")

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

for (i in 94:95) {
	for (j in 1:12) {
		temp <- append(i, j, tail(data_list[[i]][[j]], 1))
		cat(temp , "\n")
	}
}
