########
# 날짜 #
########
d <- Sys.Date()
d
# "2020-06-19"
class(d)
# "Date"

# Date형 -> character형
format(d, format = "%Y:%m:%d")
class(format(d, format = "%Y:%m:%d"))
# "character"

# character형 -> Date형
as.Date("2016:06:19", format = "%Y:%m:%d")
# "2016-06-19"
class(as.Date("2016:06:19", format = "%Y:%m:%d"))
# "Date"

#
d1 <- as.Date("2016-01-02")
d2 <- as.Date("2016-03-12")
d1 > d2
# FALSE
d2 - d1
# Time difference of 70 days
d1 + 1
# "2016-01-03"

########
# 시간 #
########
t <- Sys.time()
t
# "2020-06-19 15:21:13 KST"
class(t)
# "POSIXct" "POSIXt"

# POXIXct형 -> character형
format(t, format = "%H시 %M분")
# "15시 21분"
class(format(t, format = "%H시 %M분"))
# "character"

# character형 -> POXIXct형
strptime("2016년 9월 26일 13시 49분", format = "%Y년 %m월 %d일 %H시 %M분")
# "2016-09-26 13:49:00 KST"
class(strptime("2016년 9월 26일 13시 49분", format = "%Y년 %m월 %d일 %H시 %M분"))
# "POSIXlt" "POSIXt"

# POXIXct형 -> int형
as.numeric(format(t, "%Y"))
# 2020
as.numeric(format(t, "%m"))
# 6
as.numeric(format(t, "%d"))
# 19
as.numeric(format(t, "%u"))
# 5
# (1)월(2)화(3)수(4)목(5)금(6)토(7)일
as.numeric(format(t, "%H"))
# 15
as.numeric(format(t, "%M"))
# 21


################################
# 분기 + 주중/주말 + 오전/오후 #
################################
now <- Sys.time()
now
# "2020-06-19 15:39:15 KST"
class(now)
"POSIXct" "POSIXt" 

install.packages("dplyr")
library(dplyr)
ifelse(as.numeric(now, "%m") == 1, "1Q", "ELSE")

#### 1. 분기 ####
quater <- ifelse(as.numeric(format(now, "%m")) %in% c(1, 2, 3), "1Q",
                 ifelse(as.numeric(format(now, "%m")) %in% c(4, 5, 6), "2Q",
                        ifelse(as.numeric(format(now, "%m")) %in% c(7, 8, 9), "3Q", "4Q"))) 
quater
# "2Q"

#### 2. 주중/주말 ####
wd_wk <- ifelse(as.numeric(format(now, "%u")) %in% c(1, 2, 3, 4, 5), "주중", "주말")
wd_wk
# "주중"

#### 3. 오전/오후 ####
am_pm <- ifelse(as.numeric(format(now, "%H")) <= 12, "오전", "오후")
am_pm
# "오후"
           