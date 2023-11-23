#########################################
# Data quality table
#########################################

data.q <- read.csv("data_quality2.csv", header = T, sep = ";")
data.q <- data.q[, 1:10]
names(data.q)
summary(data.q)

# Recode for the table:
names(table(data.q$quality))
data.q$recoded <- rep(NA, nrow(data.q))
data.q$recoded[data.q$quality == "C,C"|
               data.q$quality == "C,C*"|
               data.q$quality == "C*,C"|
               data.q$quality == "C*,C*"|
                 data.q$quality == "C*C*"] <- "both complete"
data.q$recoded[data.q$quality == "C,U"|
              data.q$quality == "C,U*"|
              data.q$quality == "C*,U"|
              data.q$quality == "C*,U*"|
              data.q$quality == "U*,C"|
              data.q$quality == "U*,C*"|
              data.q$quality == "U,C"] <- "complete/incomplete"
data.q$recoded[data.q$quality == "C,N"|
                 data.q$quality == "C*,N"|
                 data.q$quality == "C*,N*"|
                 data.q$quality == "N,C*"|
                 data.q$quality == "N,C"|
                 data.q$quality == "N*,C*"] <- "complete/unknown"
data.q$recoded[data.q$quality == "N,U"|
                 data.q$quality == "N,U*"|
                 data.q$quality == "N*,U*"|
                 data.q$quality == "U,N"|
                 data.q$quality == "U*,N"|
                 data.q$quality == "U*,N*"] <- "incomplete/unknown"
data.q$recoded[data.q$quality == "U,U"|
                 data.q$quality == "U,U*"|
                 data.q$quality == "U*,U*"|
                 data.q$quality == "U*,U"|
                 data.q$quality == "U,N*"] <- "both incomplete"
data.q$recoded[data.q$quality == "I,C*"|
                 data.q$quality == "I,I*"|
                 data.q$quality == "N*,U*"|
                 data.q$quality == "I,U"|
                 data.q$quality == "I,I"|
                 data.q$quality == "N,I"|
                 data.q$quality == "C,I"] <- "civil data"
data.q$recoded[data.q$quality == "N,N"|
                 data.q$quality == "N,N*"|
                 data.q$quality == "N*,N*"|
                 data.q$quality == "N*,N"] <- "both unknown"

summary(as.factor(data.q$recoded))

data.q$quality[is.na(data.q$recoded)]

tt <- table(data.q$year, data.q$recode)
py <- prop.table(tt, margin = 1)
rr <- table(data.q$SDG_reg_name, data.q$recode)
pr <- prop.table(rr, margin = 1)

years <- c(seq(1950, 2010,20), 2021)

final.table <- round(rbind(py[row.names(py) %in% years, ] * 100, pr * 100), 2)
write.csv(final.table, "table_quality.csv")
