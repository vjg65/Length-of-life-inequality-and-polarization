####################################################################
# Estimation of lifespan inequality and polarisation   (female pop)#
####################################################################
rm(list=ls())
setwd("C:/Users/jordav/OneDrive - UNICAN/Art?culos/Paper on lifespan inequality/Results_codes_2022")

data.bs <- read.csv("life_tables_all.csv", header=T)
nrow(data.bs)



#############################
# apaño para corregir la población
# habíamos usado la total en lugar de la de las mujeres
# no incluir en el replication code
#############################
# Add data on female population:
data.f <- read.csv("ineq_pol_female.csv", header=T)
names(data.f)
data.f <- data.f[, -c(1, 36:59)]
head(data.f)

names(table(data.f$WB_inc_name))


pop<- read.csv("population_female.csv", header=T,  sep = ";",na.strings = "...")
nrow(pop)
names(pop)
pop$tot.pop <- apply(pop[, 4:24],1, sum)
pop$adult.pop <- apply(pop[, 7:24],1, sum)
data.m <- pop[, c(2, 3, 25, 26)]
summary(data.m)

data.final <- merge(data.f, data.m, by.x = c("id", "year"), by.y = c("id", "Year"))
names(data.final)
write.csv(data.final, "ineq_pol_female.csv")
