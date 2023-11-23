####################################################################
# Estimation of lifespan inequality and polarisation   (male pop.) #
####################################################################
rm(list=ls())
setwd("C:/Users/jordav/OneDrive - UNICAN/Art?culos/Paper on lifespan inequality/Results_codes_2022")

data.bs <- read.csv("life_tables_all.csv", header=T)
nrow(data.bs)
# We divide the data into sexes
data.men <- data.bs[data.bs$Sex == "Male", ]
data.wo <- data.bs[data.bs$Sex == "Female", ]
data.tot <- data.bs[data.bs$Sex == "Total", ]
rm(data.bs)

# Country-level lifespan inequality
lci <- c(0, 1, seq(5,95,5))
uci <- c(1, seq(4,95,5), 100)
# Class mark
age <- c((lci+uci)/2, 100)
alpha <- c(0, 0.25, 0.5, 0.75, 1)

#-------------------------------------------------------------------#
# Lifespan inequality for the whole population (infants and adults) #
#-------------------------------------------------------------------#
years <- as.numeric(names(table(data.men$Time)))
countries <- names(table(data.men$LocID))
ineq.men <- expand.grid(id = countries, year = years)
tt <- matrix(NA, nrow(ineq.men), 18)
ineq.men <- as.data.frame(cbind(ineq.men, tt))
names(ineq.men) <- c("id", "year", "country", "ISO3", "mean_c", "theil_c", "mld_c", "ge2_c",
                     "var_c", "gini_c", "ader_0", "ader_25", "ader_50", "ader_75", 
                     "ader_1", "rder_0", "rder_25", "rder_50", "rder_75", "rder_1")
start_time <- Sys.time()
for (i in 1:nrow(ineq.men)){
  ineq.men[i, 3] <- unique(data.men$Location[data.men$Time == ineq.men$year[i] 
                                             & data.men$LocID == ineq.men$id[i]])
  ineq.men[i, 4] <- unique(data.men$ISO3_code[data.men$Time == ineq.men$year[i] 
                                              & data.men$LocID == ineq.men$id[i]])
  dens.data <- data.men$dx[data.men$Time == ineq.men$year[i] &
                             data.men$LocID == ineq.men$id[i]]/100000
  ineq.men[i, 5] <- sum(dens.data*age)
  le <- ineq.men[i, 5]
  ineq.men[i, 6]  <- sum(dens.data * age/le * log(age/le))
  ineq.men[i, 7] <- sum(dens.data * log(le/age))
  theta <- 2
  ineq.men[i, 8]  <- 1/(theta*(theta-1))*(sum(dens.data*(age/le)^theta)-1)
  ineq.men[i, 9] <- sum(dens.data*age^2)-le^2
  gini.d <- data.frame()
  for(k in 1:length(dens.data)) {
    for(j in 1:length(dens.data)) {
      if(k!=j) {
        gini.d <-rbind(gini.d,abs(age[k]-age[j])*dens.data[k]* dens.data[j])
      }
    }
  }
  ineq.men[i, 10] <- sum(gini.d)/(2*le)
  for(a in 1:length(alpha)){
    pol.d <- data.frame()
    for(k in 1:length(dens.data)) {
      for(j in 1:length(dens.data)) {
        if(k!=j) {
          pol.d <-rbind(pol.d,abs(age[k]-age[j])*dens.data[k]^(1 + alpha[a])* dens.data[j])
        }
      }
    }
    ineq.men[i, 10 + a] <- sum(pol.d)
    ineq.men[i, 15 + a] <- ineq.men[i, 10 + a]/ (2 * le^(1 - alpha[a]))
  }
}
end_time <- Sys.time()
end_time - start_time



#-------------------------------------------------------------------#
# Lifespan inequality for the adult population								      #
# We truncate the distributions at the age of 15  							    #
#-------------------------------------------------------------------#
lci <- seq(15,95,5)
uci <- c(seq(19,95,5), 100)
# Class mark
age <- c((lci+uci)/2, 100)

years <- as.numeric(names(table(data.men$Time)))
countries <- names(table(data.men$LocID))
ineq.men.t <- expand.grid(id = countries, year = years)
tt <- matrix(NA, nrow(ineq.men), 7)
ineq.men.t <- as.data.frame(cbind(ineq.men.t, tt))
names(ineq.men.t) <- c("id", "year", "country", "mean_t", "theil_t", "mld_t", "ge2_t", "var_t", "gini_t")

for (i in 1:nrow(ineq.men)){
  ineq.men.t[i, 3] <- unique(data.men$Location[data.men$Time == ineq.men$year[i] 
                                               & data.men$LocID == ineq.men$id[i]])
  data.temp <- data.men$dx[data.men$Time == ineq.men$year[i] &
                             data.men$LocID == ineq.men$id[i]]/100000
  dens.datat <- data.temp[-c(1:4)]
  dens.data <- dens.datat/sum(dens.datat)
  le <- sum(dens.data*age)
  ineq.men.t[i, 4] <- sum(dens.data*age)
  le <- ineq.men.t[i, 4]
  ineq.men.t[i, 5]  <- sum(dens.data * age/le * log(age/le))
  ineq.men.t[i, 6] <- sum(dens.data * log(le/age))
  theta <- 2
  ineq.men.t[i, 7]  <- 1/(theta*(theta-1))*(sum(dens.data*(age/le)^theta)-1)
  ineq.men.t[i, 8] <- sum(dens.data*age^2)-le^2
  gini.d <- data.frame()
  for(k in 1:length(dens.data)) {
    for(j in 1:length(dens.data)) {
      if(k!=j) {
        gini.d <-rbind(gini.d,abs(age[k]-age[j])*dens.data[k]* dens.data[j])
      }
    }
  }
  ineq.men.t[i, 9] <- sum(gini.d)/(2*le)
}

datos <- merge(ineq.men,ineq.men.t, by= c("year", "id"))

####################################################################
# Add metadata to the data base#
####################################################################

meta <- read.csv("database_metadata.csv", header=T, sep = ";")
pop <- read.csv("population_male.csv", header=T,  sep = ";",na.strings = "...")
summary(pop)

data.f <- merge(datos, meta, by.x = "id", by.y = "country_code")
nrow(data.f)# data on macroregions is removed 
# Check that only data on macroregions has been ruled out
tt <- unique(datos$id)
bb <- unique(meta$country_code)
faltan <- tt[!(tt%in%bb)]
for(i in 1:length(faltan)){
   print(unique(datos$country.x[datos$id == faltan[i]]))
}

# Add data on total population and population over-15
names(pop)
pop$tot.pop <- apply(pop[, 4:24],1, sum)
pop$adult.pop <- apply(pop[, 7:24],1, sum)
data.m <- pop[, c(2, 3, 25, 26)]
summary(data.m)

data.final <- merge(data.f, data.m, by.x = c("id", "year"), by.y = c("id", "Year"))
nrow(data.final)
names(data.final)

write.csv(data.final, "ineq_pol_male.csv")


#############################
# apaño para corregir la población
# habíamos usado la total en lugar de la de los hombres
# no incluir en el replication code
#############################

# Add data on female population:
data.f <- read.csv("ineq_pol_male.csv", header=T)
names(data.f)
data.f <- data.f[, -c(1, 36:37)]
head(data.f)

pop<- read.csv("population_male.csv", header=T,  sep = ";",na.strings = "...")
nrow(pop)
names(pop)
pop$tot.pop <- apply(pop[, 4:24],1, sum)
pop$adult.pop <- apply(pop[, 7:24],1, sum)
data.m <- pop[, c(2, 3, 25, 26)]
summary(data.m)

data.final <- merge(data.f, data.m, by.x = c("id", "year"), by.y = c("id", "Year"))
names(data.final)
write.csv(data.final, "ineq_pol_male.csv")
