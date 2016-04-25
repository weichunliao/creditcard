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

# Sys.getlocale()
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
for (i in 1:11) { for (j in 1:12) { cat(i+93,j, which(rownames(data_list[[i]][[j]]) == "台北富邦銀行"),"\n") }}
for (i in 1:11) { for (j in 1:12) { cat(i+93,j, ("花旗(台灣)商業銀行" %in% rownames(data_list[[i]][[j]])),"\n") }}
identical(ttt, chinatrust)
as.Date(paste(1, (test[,2]+1911), test[,3]), format="%d %Y %m")
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
# get the data for banks
get_bank <- function(bank_name, year_count = 11, data_list = data_list_v2) {
	data_list_v2 <- tail(data_list_v2, year_count)
	start_year <- 104 - year_count
	temp <- lapply(data_list, function(x) {return ( cbind(month=c(1:12), t(sapply(x, function (y) { return(y[bank_name,]) })))) } )
	temp <- lapply(seq_along(temp), function(i) {return (cbind(year=rep((i+start_year), 12), temp[[i]]))})
	temp <- do.call(rbind, temp)


	temp <- as.data.frame(temp)
	temp <- cbind( date = as.Date(paste(1, (temp[,1]+1911), temp[,2]), format="%d %Y %m"), temp)
	# temp <- temp[,-c(2,3)]

	return (temp)
}

get_bank <- function(bank_name, year_count = 11, data_list = data_list_v2) {
	data_list_v2 <- tail(data_list_v2, year_count)
	start_year <- 104 - year_count
	temp <- lapply(data_list, function(x) {return ( cbind(month=c(1:12), t(sapply(x, function (y) { return(y[bank_name,]) })))) } )
	temp <- lapply(seq_along(temp), function(i) {return (cbind(year=rep((i+start_year), 12), temp[[i]]))})
	temp <- do.call(rbind, temp)


	temp <- as.data.frame(temp)
	temp <- cbind( date = as.Date(paste(1, (temp[,1]+1911), temp[,2]), format="%d %Y %m"), temp)
	# temp <- temp[,-c(2,3)]

	return (temp)
}

get_bank_v2 <- function(bank_name, year_count = 11, data_list = data_list_v2) {
	data_list_v2 <- tail(data_list_v2, year_count)
	start_year <- 104 - year_count
	temp <- lapply(data_list, function(x) {return ( cbind(month=c(1:12), t(sapply(x, function (y) { return((y[bank_name,]/tail(y,1))) })))) } )
	temp <- lapply(seq_along(temp), function(i) {return (cbind(year=rep((i+start_year), 12), temp[[i]]))})
	temp <- do.call(rbind, temp)


	temp <- as.data.frame(temp)
	temp <- cbind( date = as.Date(paste(1, (temp[,1]+1911), temp[,2]), format="%d %Y %m"), temp)
	# temp <- temp[,-c(2,3)]

	return (temp)
}


chinatrust <- get_bank("中國信託商業銀行", year_count = 6)
cathay <- get_bank("國泰世華商業銀行", year_count = 6)
sun <- get_bank("玉山商業銀行", year_count = 6)
taishin <- get_bank("台新國際商業銀行", year_count = 6)
citibank <- get_bank("花旗(台灣)商業銀行", year_count = 6)

plot(chinatrust[,1], chinatrust[,11], col="black", type="l", ylim=c(0,1.5))
lines(cathay[,1],cathay[,11], col="blue", type="l")
lines(sun[,1],sun[,11], col="green", type="l")
lines(taishin[,1],taishin[,11], col="purple", type="l")
lines(citibank[,1],citibank[,11], col="orange", type="l")
color <- c("black", "blue", "green", "purple", "orange")

# 

chinatrust2 <- get_bank_v2("中國信託商業銀行", year_count = 6)
cathay2 <- get_bank_v2("國泰世華商業銀行", year_count = 6)
sun2 <- get_bank_v2("玉山商業銀行", year_count = 6)
taishin2 <- get_bank_v2("台新國際商業銀行", year_count = 6)
citibank2 <- get_bank_v2("花旗(台灣)商業銀行", year_count = 6)

plot(chinatrust2[,1], (chinatrust[,9]/chinatrust[,5])*1000, col="black", ylim=c(4800,11000), type="b", pch=16)
points(cathay2[,1], (cathay[,9]/cathay[,5])*1000, col="blue", type="b", pch=16)
points(sun2[,1], (sun[,9]/sun[,5])*1000, col="green", type="b", pch=16)
points(taishin2[,1], (taishin[,9]/taishin[,5])*1000, col="purple", type="b", pch=16)
points(citibank2[,1], (citibank[,9]/citibank[,5])*1000, col="orange", type="b", pch=16)

plot(chinatrust2[,1], (chinatrust[,"當月停卡數"]/(chinatrust[,"流通卡數"]-chinatrust[,"當月發卡數"]+chinatrust[,"當月停卡數"])), type="l", col="black", ylim=c(0, 0.17
	))
points(cathay2[,1], (cathay[,"當月停卡數"]/(cathay[,"流通卡數"]-cathay[,"當月發卡數"]+cathay[,"當月停卡數"])), col="blue", type="l")
points(sun2[,1], (sun[,"當月停卡數"]/(sun[,"流通卡數"]-sun[,"當月發卡數"]+sun[,"當月停卡數"])), col="green", type="l")
points(taishin2[,1], (taishin[,"當月停卡數"]/(taishin[,"流通卡數"]-taishin[,"當月發卡數"]+taishin[,"當月停卡數"])), col="purple", type="l")
points(citibank2[,1], (citibank[,"當月停卡數"]/(citibank[,"流通卡數"]-citibank[,"當月發卡數"]+citibank[,"當月停卡數"])), col="orange", type="l")

plot(chinatrust2[,1], (sun[,"當月發卡數"]/(sun[,"流通卡數"]-sun[,"當月發卡數"]+sun[,"當月停卡數"])), type="l", col="black", ylim=c(0, 0.1
	), main = "sun")
points(sun[,1], (sun[,"當月停卡數"]/(sun[,"流通卡數"]-sun[,"當月發卡數"]+sun[,"當月停卡數"])), col="green", type="l")

plot(chinatrust2[,1], (chinatrust[,"當月發卡數"]/(chinatrust[,"流通卡數"]-chinatrust[,"當月發卡數"]+chinatrust[,"當月停卡數"])), type="l", col="black", ylim=c(0, 0.17
	), main="chinatrust")
points(sun[,1], (chinatrust[,"當月停卡數"]/(chinatrust[,"流通卡數"]-chinatrust[,"當月發卡數"]+chinatrust[,"當月停卡數"])), col="green", type="l")

plot(chinatrust2[,1], (taishin[,"當月發卡數"]/(taishin[,"流通卡數"]-taishin[,"當月發卡數"]+taishin[,"當月停卡數"])), type="l", col="black", ylim=c(0, 0.1
	), main="taishin")
points(sun2[,1], (taishin[,"當月停卡數"]/(taishin[,"流通卡數"]-taishin[,"當月發卡數"]+taishin[,"當月停卡數"])), col="green", type="l")

plot(chinatrust2[,1], (citibank[,"當月發卡數"]/(citibank[,"流通卡數"]-citibank[,"當月發卡數"]+citibank[,"當月停卡數"])), type="l", col="black", ylim=c(0, 0.1
	), main="citibank")
points(sun2[,1], (citibank[,"當月停卡數"]/(citibank[,"流通卡數"]-citibank[,"當月發卡數"]+citibank[,"當月停卡數"])), col="green", type="l")

plot(chinatrust2[,1], (cathay[,"當月發卡數"]/(cathay[,"流通卡數"]-cathay[,"當月發卡數"]+cathay[,"當月停卡數"])), type="l", col="black", ylim=c(0, 0.1
	), main="cathay")
points(sun2[,1], (cathay[,"當月停卡數"]/(cathay[,"流通卡數"]-cathay[,"當月發卡數"]+cathay[,"當月停卡數"])), col="green", type="l")



# ######################################


