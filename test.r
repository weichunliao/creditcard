# install.packages("readxl")
library("readxl")

# ###############
setwd("/Users/weichun/Desktop/creditcard")
data_list <- readRDS("data_list.rds")
data_list_v2 <- readRDS("data_list_v2.rds")
# datalist_by_year <- readRDS("datalist_by_year.rds")
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
# ####################
# 100-9 rownames有*號
rownames(data_list[[(100-93)]][[9]]) <- rownames(data_list[[(100-93)]][[10]])
rownames(data_list_v2[[(100-93)]][[9]]) <- rownames(data_list_v2[[(100-93)]][[10]])

# ######################################

# 13 col data list
data_list_v2 <- vector("list", 11)
for (i in 1:11) {
	for (j in 1:12) {
		data_list_v2[[i]][[j]] <- data_list[[i]][[j]][,colnames(data_list[[i]][[j]])!= "未到期分期付款餘額"]	
	}
}


# ######################################
for (i in 1:11) { for (j in 1:12) { cat(i+93,j,which(rownames(data_list[[i]][[j]]) == "台北富邦銀行"),"\n") }}
identical(ttt, chinatrust)
# ### 

# 
# 前五大發卡銀行
# 中國信託商業銀行 chinatrust
# 國泰世華商業銀行 cathay
# 玉山商業銀行 sun
# 台新國際商業銀行 taishin
# 花旗(台灣)商業銀行 citibank
# 台北富邦商業銀行 fubon

top_bank <- c("中國信託商業銀行", "國泰世華商業銀行", "玉山商業銀行", "台新國際商業銀行", "花旗(台灣)商業銀行", "台北富邦商業銀行")

data_list_v2_101 <- apply(as.matrix(c(9:11)), 1, function(x) {return (data_list_v2[[x]])})

chinatrust <- lapply(data_list_v2, function(x) {return ( cbind(month=c(1:12), t(sapply(x, function (y) { return(y["中國信託商業銀行",]) })))) } )
cathay <- lapply(data_list_v2, function(x) {return ( cbind(month=c(1:12), t(sapply(x, function (y) { return(y["國泰世華商業銀行",]) })))) } )
sun <- lapply(data_list_v2, function(x) {return ( cbind(month=c(1:12), t(sapply(x, function (y) { return(y["玉山商業銀行",]) })))) } )
taishin <- lapply(data_list_v2, function(x) {return ( cbind(month=c(1:12), t(sapply(x, function (y) { return(y["台新國際商業銀行",]) })))) } )
# 花旗(台灣)銀行(96-12 ~ 98-6)  vs  美商花旗銀行(94-1 ~ 98-7) vs 花旗(台灣)商業銀行(98-7~)
citibank <- lapply(data_list_v2_101, function(x) {return ( cbind(month=c(1:12), t(sapply(x, function (y) { return(y["花旗(台灣)商業銀行",]) })))) } )
# 台北富邦銀行(94-1 ~ 101-9, ~94-2) vs 台北國際商業銀行(94-1 ~ 95-7) vs 台北富邦商業銀行(101-10~, +94-2)
fubon <- lapply(data_list_v2_101, function(x) {return ( cbind(month=c(1:12), t(sapply(x, function (y) { return(y["台北富邦商業銀行",]) })))) } )

# ###

chinatrust <- lapply(seq_along(chinatrust), function(i) {return (cbind(year=rep((i+93), 12), chinatrust[[i]]))})
chinatrust <- do.call(rbind, chinatrust)

# ###
# get the data for 
get_bank <- function(bank_name, data_list = data_list_v2, start_year = 94) {
	start_year <- start_year-1
	temp <- lapply(data_list, function(x) {return ( cbind(month=c(1:12), t(sapply(x, function (y) { return(y[bank_name,]) })))) } )
	temp <- lapply(seq_along(temp), function(i) {return (cbind(year=rep((i+start_year), 12), temp[[i]]))})
	temp <- do.call(rbind, temp)

	return (temp)
}
chinatrust <- get_bank("中國信託商業銀行")
cathay <- get_bank("國泰世華商業銀行")
sun <- get_bank("玉山商業銀行")
taishin <- get_bank("台新國際商業銀行")
citibank <- get_bank("花旗(台灣)商業銀行")



# ######################################


