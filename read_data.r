library("readxl")
library("base")

setwd("/Users/weichun/Desktop/creditcard")

# data <- read_excel("data/94/1.xls")
data_list <- vector("list", 11)
names(data_list) <- c(94:104)
for (i in 94:102) {
	# cat(i,"年", "\n")
	monthly_data <- vector("list", 12)
	for (j in 1:12) {
		# cat(j, "月", "\n")
		filename <- paste("data/", i, "/", j, ".xls", sep="")
		temp <- read_excel(filename)
		m <- as.matrix(temp)
		m[ is.na(m) ] <- ""
		m <- gsub("[[:space:]]", "", m)

		# set colnames
		colname_index <- which( m[, 1] == "金融機構名稱", arr.ind=T )
		colname_index2 <- tail(which( m[1:10, 1] == "", arr.ind=T ), 1)
		data_col_names <- apply(m[colname_index:colname_index2, ], 2, function(x) paste(x, collapse=""))
		data_col_names <- gsub("[[:space:]]", "", data_col_names)
		# cat(data_col_names, "\n")
		colnames(m) <- data_col_names

		total_idx <- which(m == "總計", arr.ind=T)[1, 1]
		m <- m[((colname_index2+1):total_idx),]

		# set rownames
		data_row_names <- m[, 1]
		rownames(m) <- data_row_names

		col_count <- ncol(m)
		m <- m[,(-1)]
		# cat(colnames(m), "bbbbbb\n")
		data_col_names <- colnames(m)
		# cat(data_col_names, "aaaaaaa\n")
		m[ is.na(m) ] <- 0
		m <- t(apply(m, 1, function(x) as.numeric(x)))

		monthly_data[[j]] <- m
		colnames(monthly_data[[j]]) <- data_col_names

		# cat(colnames(monthly_data[[j]]), "aaa\n")
	}
	data_list[[i]] <- monthly_data
}


for (i in 103:104) {
	# cat(i, "年\n")
	monthly_data <- vector("list", 12)
	for (j in 1:12) {
		# cat(j, "月", "\n")
		filename <- paste("data/", i, "/", j, ".xlsx", sep="")
		temp <- read_excel(filename)
		m <- as.matrix(temp)
		m[ is.na(m) ] <- ""
		m <- gsub("[[:space:]]", "", m)

		# set colnames
		colname_index <- which( m[, 1] == "金融機構名稱", arr.ind=T )
		colname_index2 <- tail(which( m[1:10, 1] == "", arr.ind=T ), 1)
		# cat(colname_index2, "\n")
		data_col_names <- apply(m[colname_index:colname_index2, ], 2, function(x) paste(x, collapse=""))
		data_col_names <- gsub("[[:space:]]", "", data_col_names)
		# cat(data_col_names, "\n")
		colnames(m) <- data_col_names

		total_idx <- which(m == "總計", arr.ind=T)[1, 1]
		m <- m[((colname_index2+1):total_idx),]

		# set rownames
		data_row_names <- m[, 1]
		rownames(m) <- data_row_names

		col_count <- ncol(m)
		m <- m[,(-1)]
		data_col_names <- colnames(m)
		m[ is.na(m) ] <- 0
		m <- t(apply(m, 1, function(x) as.numeric(x)))

		monthly_data[[j]] <- m
		colnames(monthly_data[[j]]) <- data_col_names
	}
	data_list[[i]] <- monthly_data
}

##### 欄位數: 100-8之前13欄，之後14欄 ######


# saveRDS(data_list, "data_list.rds")



