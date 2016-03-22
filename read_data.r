library("readxl")
library("base")

setwd("/Users/weichun/Desktop/creditcard")

preprocess_data <- function(data) {
	m <- as.matrix(temp)
	m[ is.na(m) ] <- ""
	m <- gsub("[[:space:]]", "", m)

	# apply(m, 1, function(x) cat(x, "\n"))
	# cat("tttttt\n")
	# delete "r" and "," in the data
	m <- gsub("r", "", m)
	m <- gsub(",", "", m)
	m <- gsub("占", "佔", m)
	# apply(m, 1, function(x) cat(x, "\n"))
	# cat("rrrr\n")

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

	# apply(m, 1, function(x) cat(x, "\n"))
	# cat("tttttt\n")
	# delete "r" in the data
	m <- gsub("r", "", m)
	# apply(m, 1, function(x) cat(x, "\n"))
	# cat("rrrr\n")

	col_count <- ncol(m)
	m <- m[,(-1)]
	data_col_names <- colnames(m)
	m[ is.na(m) ] <- 0
	m <- t(apply(m, 1, function(x) as.numeric(x)))
	colnames(m) <- data_col_names

	return (m)
}


# data <- read_excel("data/94/1.xls")
data_list <- vector("list", 11)
names(data_list) <- c(94:104)
for (i in 94:102) {
	# cat(i,"年", "\n")
	monthly_data <- vector("list", 12)
	for (j in 1:12) {
		# cat(j, "月", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n")
		filename <- paste("data/", i, "/", j, ".xls", sep="")
		temp <- read_excel(filename)
		# m <- as.matrix(temp)
		# m[ is.na(m) ] <- ""
		# m <- gsub("[[:space:]]", "", m)

		# # apply(m, 1, function(x) cat(x, "\n"))
		# # cat("tttttt\n")
		# # delete "r" and "," in the data
		# m <- gsub("r", "", m)
		# m <- gsub(",", "", m)
		# # apply(m, 1, function(x) cat(x, "\n"))
		# # cat("rrrr\n")

		# # set colnames
		# colname_index <- which( m[, 1] == "金融機構名稱", arr.ind=T )
		# colname_index2 <- tail(which( m[1:10, 1] == "", arr.ind=T ), 1)
		# data_col_names <- apply(m[colname_index:colname_index2, ], 2, function(x) paste(x, collapse=""))
		# data_col_names <- gsub("[[:space:]]", "", data_col_names)
		# # cat(data_col_names, "\n")
		# colnames(m) <- data_col_names

		# total_idx <- which(m == "總計", arr.ind=T)[1, 1]
		# m <- m[((colname_index2+1):total_idx),]

		# # set rownames
		# data_row_names <- m[, 1]
		# rownames(m) <- data_row_names

		# apply(m, 1, function(x) cat(x, "\n"))
		# # cat("tttttt\n")
		# # delete "r" in the data
		# m <- gsub("r", "", m)
		# apply(m, 1, function(x) cat(x, "\n"))
		# # cat("rrrr\n")

		# col_count <- ncol(m)
		# m <- m[,(-1)]
		# data_col_names <- colnames(m)
		# m[ is.na(m) ] <- 0
		# m <- t(apply(m, 1, function(x) as.numeric(x)))
		# colnames(m) <- data_col_names

		monthly_data[[j]] <- preprocess_data(temp)
		# monthly_data[[j]] <- m
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
		
		monthly_data[[j]] <- preprocess_data(temp)
	}
	data_list[[i]] <- monthly_data
}

##### 欄位數: 100-8之前13欄，之後14欄 ######

# data_list[[96]][[12]][43, 11] <- sum(data_list[[96]][[12]][1:42, 11])
# data_list[[96]][[12]][43, 12] <- sum(data_list[[96]][[12]][1:42, 12])

# saveRDS(data_list, "data_list.rds")



