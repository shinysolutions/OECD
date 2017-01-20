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
Q_ls[["1_1_1"]] <- Dat[which(Dat$TopicsCode == "1_1_1"), ]


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
Q_ls[["1_2_3_1"]] <- Dat[which(Dat$TopicsCode == "1_2_3_1"), ]
Q_ls[["1_2_3_2"]] <- Dat[which(Dat$TopicsCode == "1_2_3_2"), ]

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
tmp <- list()
tmp[["2_1_0_1"]] <- Dat[which(Dat$TopicsCode == "2_1_0_1"), ]
tmp[["2_1_0_2"]] <- Dat[which(Dat$TopicsCode == "2_1_0_2"), ]
tmp[["2_1_0_3"]] <- Dat[which(Dat$TopicsCode == "2_1_0_3"), ]
tmp[["2_1_0_4"]] <- Dat[which(Dat$TopicsCode == "2_1_0_4"), ]
tmp[["2_1_0_5"]] <- Dat[which(Dat$TopicsCode == "2_1_0_5"), ]
tmp[["2_1_0_6"]] <- Dat[which(Dat$TopicsCode == "2_1_0_6"), ]
tmp[["2_1_0_7"]] <- Dat[which(Dat$TopicsCode == "2_1_0_7"), ]
tmp[["2_1_0_8"]] <- Dat[which(Dat$TopicsCode == "2_1_0_8"), ]
tmp[["2_1_0_9"]] <- Dat[which(Dat$TopicsCode == "2_1_0_9"), ]
tmp[["2_1_0_10"]] <- Dat[which(Dat$TopicsCode == "2_1_0_10"), ]
tmp[["2_1_0_11"]] <- Dat[which(Dat$TopicsCode == "2_1_0_11"), ]
tmp[["2_1_0_12"]] <- Dat[which(Dat$TopicsCode == "2_1_0_12"), ]
tmp[["2_1_0_13"]] <- Dat[which(Dat$TopicsCode == "2_1_0_13"), ]
tmp[["2_1_0_14"]] <- Dat[which(Dat$TopicsCode == "2_1_0_14"), ]
tmp[["2_1_0_15"]] <- Dat[which(Dat$TopicsCode == "2_1_0_15"), ]
Q_ls[["2_1_0"]] <- tmp

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
tmp <- list()
tmp[["2_1_1_1_1"]] <- Dat[which(Dat$TopicsCode == "2_1_1_1_1"), ]
tmp[["2_1_1_1_2"]] <- Dat[which(Dat$TopicsCode == "2_1_1_1_2"), ]
tmp[["2_1_1_1_3"]] <- Dat[which(Dat$TopicsCode == "2_1_1_1_3"), ]
TMP[["2_1_1_1"]] <- tmp

tmp <- list()
tmp[["2_1_1_2_1"]] <- Dat[which(Dat$TopicsCode == "2_1_1_2_1"), ]
tmp[["2_1_1_2_2"]] <- Dat[which(Dat$TopicsCode == "2_1_1_2_2"), ]
tmp[["2_1_1_2_3"]] <- Dat[which(Dat$TopicsCode == "2_1_1_2_3"), ]
TMP[["2_1_1_2"]] <- tmp

tmp <- list()
tmp[["2_1_1_3_1"]] <- Dat[which(Dat$TopicsCode == "2_1_1_3_1"), ]
tmp[["2_1_1_3_2"]] <- Dat[which(Dat$TopicsCode == "2_1_1_3_2"), ]
tmp[["2_1_1_3_3"]] <- Dat[which(Dat$TopicsCode == "2_1_1_3_3"), ]
TMP[["2_1_1_3"]] <- tmp

tmp <- list()
tmp[["2_1_1_4_1"]] <- Dat[which(Dat$TopicsCode == "2_1_1_4_1"), ]
tmp[["2_1_1_4_2"]] <- Dat[which(Dat$TopicsCode == "2_1_1_4_2"), ]
tmp[["2_1_1_4_3"]] <- Dat[which(Dat$TopicsCode == "2_1_1_4_3"), ]
TMP[["2_1_1_4"]] <- tmp

tmp <- list()
tmp[["2_1_1_5_1"]] <- Dat[which(Dat$TopicsCode == "2_1_1_5_1"), ]
tmp[["2_1_1_5_2"]] <- Dat[which(Dat$TopicsCode == "2_1_1_5_2"), ]
tmp[["2_1_1_5_3"]] <- Dat[which(Dat$TopicsCode == "2_1_1_5_3"), ]
TMP[["2_1_1_5"]] <- tmp

tmp <- list()
tmp[["2_1_1_6_1"]] <- Dat[which(Dat$TopicsCode == "2_1_1_6_1"), ]
tmp[["2_1_1_6_2"]] <- Dat[which(Dat$TopicsCode == "2_1_1_6_2"), ]
tmp[["2_1_1_6_3"]] <- Dat[which(Dat$TopicsCode == "2_1_1_6_3"), ]
TMP[["2_1_1_6"]] <- tmp

tmp <- list()
tmp[["2_1_1_7_1"]] <- Dat[which(Dat$TopicsCode == "2_1_1_7_1"), ]
tmp[["2_1_1_7_2"]] <- Dat[which(Dat$TopicsCode == "2_1_1_7_2"), ]
tmp[["2_1_1_7_3"]] <- Dat[which(Dat$TopicsCode == "2_1_1_7_3"), ]
TMP[["2_1_1_7"]] <- tmp

tmp <- list()
tmp[["2_1_1_8_1"]] <- Dat[which(Dat$TopicsCode == "2_1_1_8_1"), ]
tmp[["2_1_1_8_2"]] <- Dat[which(Dat$TopicsCode == "2_1_1_8_2"), ]
tmp[["2_1_1_8_3"]] <- Dat[which(Dat$TopicsCode == "2_1_1_8_3"), ]
TMP[["2_1_1_8"]] <- tmp

tmp <- list()
tmp[["2_1_1_9_1"]] <- Dat[which(Dat$TopicsCode == "2_1_1_9_1"), ]
tmp[["2_1_1_9_2"]] <- Dat[which(Dat$TopicsCode == "2_1_1_9_2"), ]
tmp[["2_1_1_9_3"]] <- Dat[which(Dat$TopicsCode == "2_1_1_9_3"), ]
TMP[["2_1_1_9"]] <- tmp

tmp <- list()
tmp[["2_1_1_10_1"]] <- Dat[which(Dat$TopicsCode == "2_1_1_10_1"), ]
tmp[["2_1_1_10_2"]] <- Dat[which(Dat$TopicsCode == "2_1_1_10_2"), ]
tmp[["2_1_1_10_3"]] <- Dat[which(Dat$TopicsCode == "2_1_1_10_3"), ]
TMP[["2_1_1_10"]] <- tmp

tmp <- list()
tmp[["2_1_1_11_1"]] <- Dat[which(Dat$TopicsCode == "2_1_1_11_1"), ]
tmp[["2_1_1_11_2"]] <- Dat[which(Dat$TopicsCode == "2_1_1_11_2"), ]
tmp[["2_1_1_11_3"]] <- Dat[which(Dat$TopicsCode == "2_1_1_11_3"), ]
TMP[["2_1_1_1"]] <- tmp

tmp <- list()
tmp[["2_1_1_12_1"]] <- Dat[which(Dat$TopicsCode == "2_1_1_12_1"), ]
tmp[["2_1_1_12_2"]] <- Dat[which(Dat$TopicsCode == "2_1_1_12_2"), ]
tmp[["2_1_1_12_3"]] <- Dat[which(Dat$TopicsCode == "2_1_1_12_3"), ]
TMP[["2_1_1_12"]] <- tmp

tmp <- list()
tmp[["2_1_1_13_1"]] <- Dat[which(Dat$TopicsCode == "2_1_1_13_1"), ]
tmp[["2_1_1_13_2"]] <- Dat[which(Dat$TopicsCode == "2_1_1_13_2"), ]
tmp[["2_1_1_13_3"]] <- Dat[which(Dat$TopicsCode == "2_1_1_13_3"), ]
TMP[["2_1_1_13"]] <- tmp

tmp <- list()
tmp[["2_1_1_14_1"]] <- Dat[which(Dat$TopicsCode == "2_1_1_14_1"), ]
tmp[["2_1_1_14_2"]] <- Dat[which(Dat$TopicsCode == "2_1_1_14_2"), ]
tmp[["2_1_1_14_3"]] <- Dat[which(Dat$TopicsCode == "2_1_1_14_3"), ]
TMP[["2_1_1_14"]] <- tmp

tmp <- list()
tmp[["2_1_1_15_1"]] <- Dat[which(Dat$TopicsCode == "2_1_1_15_1"), ]
tmp[["2_1_1_15_2"]] <- Dat[which(Dat$TopicsCode == "2_1_1_15_2"), ]
tmp[["2_1_1_15_3"]] <- Dat[which(Dat$TopicsCode == "2_1_1_15_3"), ]
TMP[["2_1_1_15"]] <- tmp

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
tmp <- list()
tmp[["2_2_2_1_1"]] <- Dat[which(Dat$TopicsCode == "2_2_2_1_1"), ]
tmp[["2_2_2_1_2"]] <- Dat[which(Dat$TopicsCode == "2_2_2_1_2"), ]
TMP[["2_2_2_1"]] <- tmp

tmp <- list()
tmp[["2_2_2_2_1"]] <- Dat[which(Dat$TopicsCode == "2_2_2_2_1"), ]
tmp[["2_2_2_2_2"]] <- Dat[which(Dat$TopicsCode == "2_2_2_2_2"), ]
TMP[["2_2_2_2"]] <- tmp

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
Q_ls[["2_3_1"]] <- Dat[which(Dat$TopicsCode == "2_3_1"), ]


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
Q_ls[["2_3_2"]] <- Dat[which(Dat$TopicsCode == "2_3_2"), ]



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
Q_ls[["2_3_3"]] <- Dat[which(Dat$TopicsCode == "2_3_3"), ]

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
Q_ls[["2_3_4"]] <- Dat[which(Dat$TopicsCode == "2_3_4"), ]

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
Q_ls[["2_3_5"]] <- Dat[which(Dat$TopicsCode == "2_3_5"), ]


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
Q_ls[["2_3_6"]] <- Dat[which(Dat$TopicsCode == "2_3_6"), ]


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
tmp <- list()
tmp[["3_1_1_1"]] <- Dat[which(Dat$TopicsCode == "3_1_1_1"), ]
tmp[["3_1_1_2"]] <- Dat[which(Dat$TopicsCode == "3_1_1_2"), ]
tmp[["3_1_1_3"]] <- Dat[which(Dat$TopicsCode == "3_1_1_3"), ]
tmp[["3_1_1_4"]] <- Dat[which(Dat$TopicsCode == "3_1_1_4"), ]
Q_ls[["3_1_1"]] <- tmp


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
tmp[["3_1_2"]] <- Dat[which(Dat$TopicsCode == "3_1_2"), ]

tmp2 <- list()
tmp2[["3_1_2_H_1"]] <- Dat[which(Dat$TopicsCode == "3_1_2_H_1"), ]
tmp2[["3_1_2_H_2"]] <- Dat[which(Dat$TopicsCode == "3_1_2_H_2"), ]
tmp2[["3_1_2_H_3"]] <- Dat[which(Dat$TopicsCode == "3_1_2_H_3"), ]

tmp[["3_1_2_H"]] <- tmp2
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
Q_ls[["3_1_3"]] <- Dat[which(Dat$TopicsCode == "3_1_3"), ]


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
Q_ls[["3_1_4"]] <- Dat[which(Dat$TopicsCode == "3_1_4"), ]


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
tmp <- list()
tmp[["3_1_5_1_N"]] <- Dat[which(Dat$TopicsCode == "3_1_5_1_N"), ]
tmp[["3_1_5_1_S"]] <- Dat[which(Dat$TopicsCode == "3_1_5_1_S"), ]
tmp[["3_1_5_1_Y"]] <- Dat[which(Dat$TopicsCode == "3_1_5_1_Y"), ]
tmp[["3_1_5_1_C"]] <- Dat[which(Dat$TopicsCode == "3_1_5_1_C"), ]
TMP[["3_1_5_1"]] <- tmp

tmp <- list()
tmp[["3_1_5_2_N"]] <- Dat[which(Dat$TopicsCode == "3_1_5_2_N"), ]
tmp[["3_1_5_2_S"]] <- Dat[which(Dat$TopicsCode == "3_1_5_2_S"), ]
tmp[["3_1_5_2_Y"]] <- Dat[which(Dat$TopicsCode == "3_1_5_2_Y"), ]
tmp[["3_1_5_2_C"]] <- Dat[which(Dat$TopicsCode == "3_1_5_2_C"), ]
TMP[["3_1_5_2"]] <- tmp

tmp <- list()
tmp[["3_1_5_3_N"]] <- Dat[which(Dat$TopicsCode == "3_1_5_3_N"), ]
tmp[["3_1_5_3_S"]] <- Dat[which(Dat$TopicsCode == "3_1_5_3_S"), ]
tmp[["3_1_5_3_Y"]] <- Dat[which(Dat$TopicsCode == "3_1_5_3_Y"), ]
tmp[["3_1_5_3_C"]] <- Dat[which(Dat$TopicsCode == "3_1_5_3_C"), ]
TMP[["3_1_5_3"]] <- tmp

tmp <- list()
tmp[["3_1_5_4_N"]] <- Dat[which(Dat$TopicsCode == "3_1_5_4_N"), ]
tmp[["3_1_5_4_S"]] <- Dat[which(Dat$TopicsCode == "3_1_5_4_S"), ]
tmp[["3_1_5_4_Y"]] <- Dat[which(Dat$TopicsCode == "3_1_5_4_Y"), ]
tmp[["3_1_5_4_C"]] <- Dat[which(Dat$TopicsCode == "3_1_5_4_C"), ]
TMP[["3_1_5_4"]] <- tmp

tmp <- list()
tmp[["3_1_5_5_N"]] <- Dat[which(Dat$TopicsCode == "3_1_5_5_N"), ]
tmp[["3_1_5_5_S"]] <- Dat[which(Dat$TopicsCode == "3_1_5_5_S"), ]
tmp[["3_1_5_5_Y"]] <- Dat[which(Dat$TopicsCode == "3_1_5_5_Y"), ]
tmp[["3_1_5_5_C"]] <- Dat[which(Dat$TopicsCode == "3_1_5_5_C"), ]
TMP[["3_1_5_5"]] <- tmp

tmp <- list()
tmp[["3_1_5_6_N"]] <- Dat[which(Dat$TopicsCode == "3_1_5_6_N"), ]
tmp[["3_1_5_6_S"]] <- Dat[which(Dat$TopicsCode == "3_1_5_6_S"), ]
tmp[["3_1_5_6_Y"]] <- Dat[which(Dat$TopicsCode == "3_1_5_6_Y"), ]
tmp[["3_1_5_6_C"]] <- Dat[which(Dat$TopicsCode == "3_1_5_6_C"), ]
TMP[["3_1_5_6"]] <- tmp

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
Q_ls[["3_2_1"]] <- Dat[which(Dat$TopicsCode == "3_2_1"), ]


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
Q_ls[["3_2_2_1"]] <- Dat[which(Dat$TopicsCode == "3_2_2_1"), ]
Q_ls[["3_2_2_2"]] <- Dat[which(Dat$TopicsCode == "3_2_2_2"), ]

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
tmp[["3_3_1"]] <- Dat[which(Dat$TopicsCode == "3_3_1"), ]

tmp2 <- list()
tmp2[["3_3_1_A_1"]] <- Dat[which(Dat$TopicsCode == "3_3_1_A_1"), ]
tmp2[["3_3_1_A_2"]] <- Dat[which(Dat$TopicsCode == "3_3_1_A_2"), ]
tmp2[["3_3_1_A_3"]] <- Dat[which(Dat$TopicsCode == "3_3_1_A_3"), ]
tmp2[["3_3_1_A_4"]] <- Dat[which(Dat$TopicsCode == "3_3_1_A_4"), ]
tmp2[["3_3_1_A_5"]] <- Dat[which(Dat$TopicsCode == "3_3_1_A_5"), ]
tmp[["3_3_1_A"]] <- tmp2

tmp[["3_3_1_T"]] <- Dat[which(Dat$TopicsCode == "3_3_1_T"), ]

tmp2 <- list()
tmp2[["3_3_1_W_1"]] <- Dat[which(Dat$TopicsCode == "3_3_1_W_1"), ]
tmp2[["3_3_1_W_2"]] <- Dat[which(Dat$TopicsCode == "3_3_1_W_2"), ]
tmp2[["3_3_1_W_3"]] <- Dat[which(Dat$TopicsCode == "3_3_1_W_3"), ]
tmp2[["3_3_1_W_4"]] <- Dat[which(Dat$TopicsCode == "3_3_1_W_4"), ]
tmp[["3_3_1_W"]] <- tmp2

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
tmp[["3_4_1"]] <- Dat[which(Dat$TopicsCode == "3_4_1"), ]

tmp2 <- list()
tmp2[["3_4_1_1_1"]] <- Dat[which(Dat$TopicsCode == "3_4_1_1_1"), ]
tmp2[["3_4_1_1_2"]] <- Dat[which(Dat$TopicsCode == "3_4_1_1_2"), ]
tmp2[["3_4_1_1_3"]] <- Dat[which(Dat$TopicsCode == "3_4_1_1_3"), ]
tmp2[["3_4_1_1_4"]] <- Dat[which(Dat$TopicsCode == "3_4_1_1_4"), ]
tmp2[["3_4_1_1_5"]] <- Dat[which(Dat$TopicsCode == "3_4_1_1_5"), ]
tmp[["3_4_1_1"]] <- tmp2

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
tmp[["3_5_1_N"]] <- Dat[which(Dat$TopicsCode == "3_5_1_N"), ]
tmp[["3_5_1_YL"]] <- Dat[which(Dat$TopicsCode == "3_5_1_YL"), ]
tmp[["3_5_1_YM"]] <- Dat[which(Dat$TopicsCode == "3_5_1_YM"), ]

tmp2 <- list()
tmp2[["3_5_1_1_E"]] <- Dat[which(Dat$TopicsCode == "3_5_1_1_E"), ]
tmp2[["3_5_1_1_O"]] <- Dat[which(Dat$TopicsCode == "3_5_1_1_O"), ]
tmp2[["3_5_1_1_S"]] <- Dat[which(Dat$TopicsCode == "3_5_1_1_S"), ]
tmp[["3_5_1_1"]] <- tmp2

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
tmp <- list()
tmp[["3_5_2_N"]] <- Dat[which(Dat$TopicsCode == "3_5_2_N"), ]
tmp[["3_5_2_YA"]] <- Dat[which(Dat$TopicsCode == "3_5_2_YA"), ]
tmp[["3_5_2_YS"]] <- Dat[which(Dat$TopicsCode == "3_5_2_YS"), ]
Q_ls[["3_5_2"]] <- tmp


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
tmp[["3_6_1"]] <- Dat[which(Dat$TopicsCode == "3_6_1"), ]

tmp2 <- list()
tmp2[["3_6_1_1_D"]] <- Dat[which(Dat$TopicsCode == "3_6_1_1_D"), ]
tmp2[["3_6_1_1_M"]] <- Dat[which(Dat$TopicsCode == "3_6_1_1_M"), ]
tmp2[["3_6_1_1_O"]] <- Dat[which(Dat$TopicsCode == "3_6_1_1_O"), ]
tmp2[["3_6_1_1_S"]] <- Dat[which(Dat$TopicsCode == "3_6_1_1_S"), ]
tmp[["3_6_1_1"]] <- tmp2

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
tmp <- list()
tmp[["3_6_3_D"]] <- Dat[which(Dat$TopicsCode == "3_6_3_D"), ]
tmp[["3_6_3_I"]] <- Dat[which(Dat$TopicsCode == "3_6_3_I"), ]
tmp[["3_6_3_R"]] <- Dat[which(Dat$TopicsCode == "3_6_3_R"), ]
Q_ls[["3_6_3"]] <- tmp

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
tmp <- list()
tmp[["3_6_4_L"]] <- Dat[which(Dat$TopicsCode == "3_6_4_L"), ]
tmp[["3_6_4_F"]] <- Dat[which(Dat$TopicsCode == "3_6_4_F"), ]
tmp[["3_6_4_N"]] <- Dat[which(Dat$TopicsCode == "3_6_4_N"), ]
Q_ls[["3_6_4"]] <- tmp


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
tmp <- list()
tmp[["3_6_5_F"]] <- Dat[which(Dat$TopicsCode == "3_6_5_F"), ]
tmp[["3_6_5_I"]] <- Dat[which(Dat$TopicsCode == "3_6_5_I"), ]
tmp[["3_6_5_N"]] <- Dat[which(Dat$TopicsCode == "3_6_5_N"), ]
tmp[["3_6_5_O"]] <- Dat[which(Dat$TopicsCode == "3_6_5_O"), ]
Q_ls[["3_6_5"]] <- tmp


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
tmp <- list()
tmp[["3_6_6_H"]] <- Dat[which(Dat$TopicsCode == "3_6_6_H"), ]
tmp[["3_6_6_L"]] <- Dat[which(Dat$TopicsCode == "3_6_6_L"), ]
tmp[["3_6_6_M"]] <- Dat[which(Dat$TopicsCode == "3_6_6_M"), ]
tmp[["3_6_6_N"]] <- Dat[which(Dat$TopicsCode == "3_6_6_N"), ]
Q_ls[["3_6_6"]] <- tmp


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
tmp <- list()
tmp[["3_7_1_M"]] <- Dat[which(Dat$TopicsCode == "3_7_1_M"), ]
tmp[["3_7_1_P"]] <- Dat[which(Dat$TopicsCode == "3_7_1_P"), ]
tmp[["3_7_1_W"]] <- Dat[which(Dat$TopicsCode == "3_7_1_W"), ]
tmp[["3_7_1_O"]] <- Dat[which(Dat$TopicsCode == "3_7_1_O"), ]
Q_ls[["3_7_1"]] <- tmp


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
Q_ls[["3_8_1"]] <- Dat[which(Dat$TopicsCode == "3_8_1"), ]

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
tmp <- list()
tmp[["3_9_1_APEC_COG"]] <- Dat[which(Dat$TopicsCode == "3_9_1_APEC_COG"), ]
tmp[["3_9_1_APEC_MOF"]] <- Dat[which(Dat$TopicsCode == "3_9_1_APEC_MOF"), ]
tmp[["3_9_1_APEC_NA"]] <- Dat[which(Dat$TopicsCode == "3_9_1_APEC_NA"), ]
tmp[["3_9_1_APEC_O"]] <- Dat[which(Dat$TopicsCode == "3_9_1_APEC_O"), ]
TMP[["3_9_1_APEC"]] <- tmp

tmp <- list()
tmp[["3_9_1_EU_COG"]] <- Dat[which(Dat$TopicsCode == "3_9_1_EU_COG"), ]
tmp[["3_9_1_EU_MOF"]] <- Dat[which(Dat$TopicsCode == "3_9_1_EU_MOF"), ]
tmp[["3_9_1_EU_NA"]] <- Dat[which(Dat$TopicsCode == "3_9_1_EU_NA"), ]
tmp[["3_9_1_EU_O"]] <- Dat[which(Dat$TopicsCode == "3_9_1_EU_O"), ]
TMP[["3_9_1_EU"]] <- tmp

tmp <- list()
tmp[["3_9_1_G20_COG"]] <- Dat[which(Dat$TopicsCode == "3_9_1_G20_COG"), ]
tmp[["3_9_1_G20_MOF"]] <- Dat[which(Dat$TopicsCode == "3_9_1_G20_MOF"), ]
tmp[["3_9_1_G20_NA"]] <- Dat[which(Dat$TopicsCode == "3_9_1_G20_NA"), ]
tmp[["3_9_1_G20_O"]] <- Dat[which(Dat$TopicsCode == "3_9_1_G20_O"), ]
TMP[["3_9_1_G20"]] <- tmp

tmp <- list()
tmp[["3_9_1_NATO_COG"]] <- Dat[which(Dat$TopicsCode == "3_9_1_NATO_COG"), ]
tmp[["3_9_1_NATO_MOF"]] <- Dat[which(Dat$TopicsCode == "3_9_1_NATO_MOF"), ]
tmp[["3_9_1_NATO_NA"]] <- Dat[which(Dat$TopicsCode == "3_9_1_NATO_NA"), ]
tmp[["3_9_1_NATO_O"]] <- Dat[which(Dat$TopicsCode == "3_9_1_NATO_O"), ]
TMP[["3_9_1_NATO"]] <- tmp

tmp <- list()
tmp[["3_9_1_OECD_COG"]] <- Dat[which(Dat$TopicsCode == "3_9_1_OECD_COG"), ]
tmp[["3_9_1_OECD_MOF"]] <- Dat[which(Dat$TopicsCode == "3_9_1_OECD_MOF"), ]
tmp[["3_9_1_OECD_NA"]] <- Dat[which(Dat$TopicsCode == "3_9_1_OECD_NA"), ]
tmp[["3_9_1_OECD_O"]] <- Dat[which(Dat$TopicsCode == "3_9_1_OECD_O"), ]
TMP[["3_9_1_OECD"]] <- tmp

tmp <- list()
tmp[["3_9_1_UN_COG"]] <- Dat[which(Dat$TopicsCode == "3_9_1_UN_COG"), ]
tmp[["3_9_1_UN_MOF"]] <- Dat[which(Dat$TopicsCode == "3_9_1_UN_MOF"), ]
tmp[["3_9_1_UN_NA"]] <- Dat[which(Dat$TopicsCode == "3_9_1_UN_NA"), ]
tmp[["3_9_1_UN_O"]] <- Dat[which(Dat$TopicsCode == "3_9_1_UN_O"), ]
TMP[["3_9_1_UN"]] <- tmp
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
Q_ls[["3_9_2"]] <- Dat[which(Dat$TopicsCode == "3_9_2"), ]


save.image(file = "Data/Q_ls.RData")
