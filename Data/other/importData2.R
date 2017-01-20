## Set work directory and load libraries;
setwd("/srv/ff/OECD")
# library(XLConnect)
# 
# ## Import data
# wb <- loadWorkbook("Data/data.xlsx"); getSheets(wb)
# setMissingValue(wb, value = "miss")
# Dat <- readWorksheet(wb, sheet = "Data View")
Dat <- read.csv("Data/data.csv", sep = ";", stringsAsFactors = FALSE)
Dat$Topics.Code <- gsub("_COMM", "_C", Dat$Topics.Code)

dim(Dat)
head(Dat, 1)
names(Dat) <- gsub("\\.", "", names(Dat))

## Filter Topics code 
Codes <- sort(unique(Dat$TopicsCode))
Codes <- Codes[grepl("^[1-3]", Codes) & sapply(gregexpr("_", Codes), length) >=2]

## Dataset by removing some questions
Dat <- Dat[which(Dat$TopicsCode %in% Codes), ]
dim(Dat)

## Remove the records that not included on the map 
cntCode <- read.csv("Data/cntCode.csv")
cnt <- c("SK", "GB", "IE", "EE", "FI", "SE", "DE", "LU", "NL", "DK", "BE", "FR", "ES", "PO",
         "CZ", "CH", "AT", "IS", "HU", "SI", "IT", "GR", "TR", "AU", "CA", "CL", "IL", "JP",
         "KR", "MX", "NZ", "US", "LV", "PT", "NO")
cntCode <- cntCode[which(cntCode$CountryCode %in% cnt), ]
Dat <- merge(Dat[, -which(names(Dat) == "CountryCode")], cntCode)


## Code data.frame
CodeFrame <- data.frame(Code = Codes, Code2 = NA, Code3 = NA, Code4 = NA, stringsAsFactors = FALSE)
for (i in 1:nrow(CodeFrame)) {
  CodeFrame[i, paste("Code", sum(gregexpr("_", CodeFrame$Code[i])[[1]]>0) , sep = "")] <- CodeFrame$Code[i]
}

for (i in seq(nrow(CodeFrame), 1)) {
  if (!is.na(CodeFrame$Code4[i])) {
    index <- rev(gregexpr("_", CodeFrame$Code4[i])[[1]])[1]-1
    CodeFrame$Code3[i] <- substr(CodeFrame$Code4[i], 1, index)
  }
  if (!is.na(CodeFrame$Code3[i])) {
    index <- rev(gregexpr("_", CodeFrame$Code3[i])[[1]])[1]-1
    CodeFrame$Code2[i] <- substr(CodeFrame$Code3[i], 1, index)
  }
}



q_ls <- unique(CodeFrame$Code2)
topics <- unique(Dat[, c("TopicsCode", "Topics")])
Q_ls <- list()



## question
q <- 1
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- Dat[which(Dat$TopicsCode == "1_1_1"), c("CountryCode", "Data")]
names(dat)[2] <- "1_1_1"
Q_ls[["1_1_1"]] <- dat


## question
q <- 2
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}

## question
q <- 3
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
## Comment: if we can convert differene currency unit into one, then this will be an interesting map also.

## question
q <- 4
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
# Q_ls[["1_2_2_1"]] <- Dat[which(Dat$TopicsCode == "1_2_2_1"), ]
# Q_ls[["1_2_2_2"]] <- Dat[which(Dat$TopicsCode == "1_2_2_2"), ]

## question
q <- 5
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
tmp <- list()
dat <- Dat[which(Dat$TopicsCode == "1_2_3_1"), c("CountryCode", "Data")]
names(dat)[2] <- "1_2_3_1"
tmp[["1_2_3_1"]] <- dat
dat <- Dat[which(Dat$TopicsCode == "1_2_3_2"), c("CountryCode", "Data")]
names(dat)[2] <- "1_2_3_2"
tmp[["1_2_3_2"]] <- dat
Q_ls[["1_2_3"]] <- tmp

## question
q <- 6
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}

dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
for (i in 1:15) {
  dat <- merge(dat, Dat[which(Dat$TopicsCode == sprintf("2_1_0_%s", i)),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <-  sprintf("2_1_0_%s", i)
}
Q_ls[["2_1_0"]] <- dat


## question
q <- 7
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}

TMP <- list()
for (i in 1:15) {
  dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
  dat <- merge(dat, Dat[which(Dat$TopicsCode == sprintf("2_1_1_%s_1", i)),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- sprintf("2_1_1_%s_1", i)
  dat <- merge(dat, Dat[which(Dat$TopicsCode == sprintf("2_1_1_%s_2", i)),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- sprintf("2_1_1_%s_2", i)
  dat <- merge(dat, Dat[which(Dat$TopicsCode == sprintf("2_1_1_%s_3", i)),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- sprintf("2_1_1_%s_3", i)
  TMP[[i]] <- dat; names(TMP)[i] <- sprintf("2_1_1_%s", i)
}
Q_ls[["2_1_1"]] <- TMP


## question
q <- 8
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
## Comment: the data seems very informative, however in a very bad quality. E.g. some values for percentage are greater than 100%;


## question
q <- 9
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}

TMP <- list()
for (i in 1:2) {
  dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
  dat <- merge(dat, Dat[which(Dat$TopicsCode == sprintf("2_2_2_%s_1", i)),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- sprintf("2_2_2_%s_1", i)
  dat <- merge(dat, Dat[which(Dat$TopicsCode == sprintf("2_2_2_%s_2", i)),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- sprintf("2_2_2_%s_2", i)
  TMP[[i]] <- dat; names(TMP)[i] <- sprintf("2_2_2_%s", i)
}
Q_ls[["2_2_2"]] <- TMP


## question
q <- 10
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- Dat[which(Dat$TopicsCode == "2_3_1"), c("CountryCode", "Data")]
names(dat)[2] <- "2_3_1"
Q_ls[["2_3_1"]] <- dat


## question
q <- 11
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- Dat[which(Dat$TopicsCode == "2_3_2"), c("CountryCode", "Data")]
names(dat)[2] <- "2_3_2"
Q_ls[["2_3_2"]] <- dat



## question
q <- 12
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- Dat[which(Dat$TopicsCode == "2_3_3"), c("CountryCode", "Data")]
names(dat)[2] <- "2_3_3"
Q_ls[["2_3_3"]] <- dat


## question
q <- 13
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- Dat[which(Dat$TopicsCode == "2_3_4"), c("CountryCode", "Data")]
names(dat)[2] <- "2_3_4"
Q_ls[["2_3_4"]] <- dat

## question
q <- 14
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- Dat[which(Dat$TopicsCode == "2_3_5"), c("CountryCode", "Data")]
names(dat)[2] <- "2_3_5"
Q_ls[["2_3_5"]] <- dat


## question
q <- 15
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- Dat[which(Dat$TopicsCode == "2_3_6"), c("CountryCode", "Data")]
names(dat)[2] <- "2_3_6"
Q_ls[["2_3_6"]] <- dat


## question
q <- 16
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
## Comment: seems too many words to display

## question
q <- 17
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
## Comment: maybe interesting, however difficult to show on a figure.

## question
q <- 18
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
# Q_ls[["2_4_2"]] <- Dat[which(Dat$TopicsCode == "2_4_2"), ]
## Comment: numbers need to be extracted from sentence.


## question
q <- 19
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}

## question
q <- 20
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_1_1_1"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_1_1_1"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_1_1_2"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_1_1_2"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_1_1_3"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_1_1_3"
# dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_1_1_4"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_1_1_4"
Q_ls[["3_1_1"]] <- dat


## question
q <- 21
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}

for (cnt in sort(unique(Dat$CountryCode))) {
  if (Dat$Data[which(Dat$CountryCode == cnt & Dat$TopicsCode == "3_1_2_N")] == "1") {
    Dat$Data[which(Dat$CountryCode == cnt & Dat$TopicsCode == "3_1_2")] <- "No"
  }
  if (Dat$Data[which(Dat$CountryCode == cnt & Dat$TopicsCode == "3_1_2_Y")] == "1") {
    Dat$Data[which(Dat$CountryCode == cnt & Dat$TopicsCode == "3_1_2")] <- "Yes"
  }
}


tmp <- list()
dat <- Dat[which(Dat$TopicsCode == "3_1_2"), c("CountryCode", "Data")]
names(dat)[2] <- "3_1_2"
tmp[["3_1_2"]] <- dat
dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_1_2_H_1"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_1_2_H_1"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_1_2_H_2"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_1_2_H_2"
# dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_1_2_H_3"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_1_2_H_3"
tmp[["3_1_2_H"]] <- dat
Q_ls[["3_1_2"]] <- tmp



## question
q <- 22
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- Dat[which(Dat$TopicsCode == "3_1_3"), c("CountryCode", "Data")]
names(dat)[2] <- "3_1_3"
Q_ls[["3_1_3"]] <- dat


## question
q <- 23
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- Dat[which(Dat$TopicsCode == "3_1_4"), c("CountryCode", "Data")]
names(dat)[2] <- "3_1_4"
Q_ls[["3_1_4"]] <- dat

## question
q <- 24
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
TMP <- list()
for (i in 1:6) {
  dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
  dat <- merge(dat, Dat[which(Dat$TopicsCode == sprintf("3_1_5_%s_N", i)),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- sprintf("3_1_5_%s_N", i)
  dat <- merge(dat, Dat[which(Dat$TopicsCode == sprintf("3_1_5_%s_S", i)),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- sprintf("3_1_5_%s_S", i)
  dat <- merge(dat, Dat[which(Dat$TopicsCode == sprintf("3_1_5_%s_Y", i)),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- sprintf("3_1_5_%s_Y", i)
  # dat <- merge(dat, Dat[which(Dat$TopicsCode == sprintf("3_1_5_%s_C", i)),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- sprintf("3_1_5_%s_C", i)
  TMP[[i]] <- dat; names(TMP)[i] <- sprintf("3_1_5_%s", i)
}
Q_ls[["3_1_5"]] <- TMP


## question
q <- 25
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}

## question
q <- 26
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- Dat[which(Dat$TopicsCode == "3_2_1"), c("CountryCode", "Data")]
names(dat)[2] <- "3_2_1"
Q_ls[["3_2_1"]] <- dat


## question
q <- 27
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
tmp <- list()
dat <- Dat[which(Dat$TopicsCode == "3_2_2_1"), c("CountryCode", "Data")]
names(dat)[2] <- "3_2_2_1"
tmp[["3_2_2_1"]] <- dat

dat <- Dat[which(Dat$TopicsCode == "3_2_2_2"), c("CountryCode", "Data")]
names(dat)[2] <- "3_2_2_2"
tmp[["3_2_2_2"]] <- dat
Q_ls[["3_2_2"]] <- dat


## question
q <- 28
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}

## question
q <- 29
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}

tmp <- list()
dat <- Dat[which(Dat$TopicsCode == "3_3_1"), c("CountryCode", "Data")]
names(dat)[2] <- "3_3_1"
tmp[["3_3_1"]] <- dat

dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_3_1_A_1"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_3_1_A_1"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_3_1_A_2"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_3_1_A_2"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_3_1_A_3"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_3_1_A_3"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_3_1_A_4"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_3_1_A_4"
# dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_3_1_A_5"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_3_1_A_5"
tmp[["3_3_1_A"]] <- dat

dat <- Dat[which(Dat$TopicsCode == "3_3_1_T"), c("CountryCode", "Data")]
names(dat)[2] <- "3_3_1_T"
tmp[["3_3_1_T"]] <- dat

dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_3_1_W_1"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_3_1_W_1"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_3_1_W_2"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_3_1_W_2"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_3_1_W_3"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_3_1_W_3"
# dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_3_1_W_4"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_3_1_W_4"
tmp[["3_3_1_W"]] <- dat

Q_ls[["3_3_1"]] <- tmp


## question
q <- 30
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}

## question
q <- 31
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
tmp <- list()
dat <- Dat[which(Dat$TopicsCode == "3_4_1"), c("CountryCode", "Data")]
names(dat)[2] <- "3_4_1"
tmp[["3_4_1"]] <- dat

dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_4_1_1_1"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_4_1_1_1"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_4_1_1_2"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_4_1_1_2"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_4_1_1_3"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_4_1_1_3"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_4_1_1_4"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_4_1_1_4"
# dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_4_1_1_5"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_4_1_1_5"
tmp[["3_4_1_1"]] <- dat

Q_ls[["3_3_1"]] <- tmp


## question
q <- 32
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}

## question
q <- 33
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
tmp <- list()
dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_5_1_N"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_5_1_N"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_5_1_YL"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_5_1_YL"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_5_1_YM"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_5_1_YM"
tmp[["3_5_1"]] <- dat


dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_5_1_1_E"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_5_1_1_E"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_5_1_1_O"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_5_1_1_O"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_5_1_1_S"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_5_1_1_S"
tmp[["3_5_1_1"]] <- dat

Q_ls[["3_5_1"]] <- tmp


## question
q <- 34
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_5_2_N"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_5_2_N"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_5_2_YA"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_5_2_YA"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_5_2_YS"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_5_2_YS"
Q_ls[["3_5_2"]] <- dat


## question
q <- 35
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}


## question
q <- 36
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
tmp <- list()
dat <- Dat[which(Dat$TopicsCode == "3_6_1"), c("CountryCode", "Data")]
names(dat)[2] <- "3_6_1"
tmp[["3_6_1"]] <- dat

dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_1_1_D"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_1_1_D"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_1_1_M"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_1_1_M"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_1_1_S"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_1_1_S"
# dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_1_1_O"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_1_1_O"
tmp[["3_6_1_1"]] <- dat

Q_ls[["3_6_1"]] <- tmp


## question
q <- 37
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}

## question
q <- 38
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_3_D"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_3_D"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_3_I"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_3_I"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_3_R"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_3_R"
Q_ls[["3_6_3"]] <- dat

## question
q <- 39
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_4_L"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_4_L"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_4_F"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_4_F"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_4_N"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_4_N"
Q_ls[["3_6_4"]] <- dat


## question
q <- 40
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_5_F"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_5_F"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_5_I"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_5_I"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_5_N"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_5_N"
# dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_5_O"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_5_O"
Q_ls[["3_6_5"]] <- dat


## question
q <- 41
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_6_H"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_6_H"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_6_L"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_6_L"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_6_M"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_6_M"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_6_6_N"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_6_6_N"
Q_ls[["3_6_6"]] <- dat


## question
q <- 42
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_7_1_M"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_7_1_M"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_7_1_P"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_7_1_P"
dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_7_1_W"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_7_1_W"
# dat <- merge(dat, Dat[which(Dat$TopicsCode == "3_7_1_O"),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- "3_7_1_O"
Q_ls[["3_7_1"]] <- dat


## question
q <- 43
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- Dat[which(Dat$TopicsCode == "3_8_1"), c("CountryCode", "Data")]
names(dat)[2] <- "3_8_1"
Q_ls[["3_8_1"]] <- dat


## question
q <- 44
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
TMP <- list()
for (i in c("APEC", "EU", "G20", "NATO", "OECD", "UN")) {
  dat <- data.frame(CountryCode = sort(unique(Dat$CountryCode)))
  dat <- merge(dat, Dat[which(Dat$TopicsCode == sprintf("3_9_1_%s_COG", i)),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- sprintf("3_9_1_%s_COG", i)
  dat <- merge(dat, Dat[which(Dat$TopicsCode == sprintf("3_9_1_%s_MOF", i)),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- sprintf("3_9_1_%s_MOF", i)
  dat <- merge(dat, Dat[which(Dat$TopicsCode == sprintf("3_9_1_%s_NA", i)),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- sprintf("3_9_1_%s_NA", i)
  # dat <- merge(dat, Dat[which(Dat$TopicsCode == sprintf("3_9_1_%s_O", i)),  c("CountryCode", "Data")]); names(dat)[ncol(dat)] <- sprintf("3_9_1_%s_O", i)
  TMP[[sprintf("3_9_1_%s", i)]] <- dat;
}
Q_ls[["3_9_1"]] <- TMP

## Comment: 3_9_1_O1, 3_9_1_O2, 3_9_1_O3 are ommitted. 

## question
q <- 45
topic <- CodeFrame[which(CodeFrame$Code2 == q_ls[q]),]
print(topic)
cat("\n")
for (code in topic$Code) {
  print(paste(code, topics$Topics[which(topics$TopicsCode == code)], sep = ": "))
  dat <- Dat$Data[which(Dat$TopicsCode == code)]
  tab <- table(dat)
  print(table(dat))
  cat("\n")
}
dat <- Dat[which(Dat$TopicsCode == "3_9_2"), c("CountryCode", "Data")]
names(dat)[2] <- "3_9_2"
Q_ls[["3_9_2"]] <- dat


save.image(file = "Data/Q_ls2.RData")
