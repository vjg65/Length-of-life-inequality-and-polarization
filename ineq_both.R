####################################################################
# Estimation of lifespan inequality and polarisation (both sexes)  #
####################################################################
rm(list=ls())
data.bs <- read.csv("life_tables_all.csv", header=T)

#Change in the names of the countries to make them match with geo data (for the shiny app)
data.bs$Location[data.bs$Location =="Bolivia (Plurinational State of)"]<-"Bolivia"
data.bs$Location[data.bs$Location =="Brunei Darussalam"]<-"Brunei"
data.bs$Location[data.bs$Location =="China, Taiwan Province of China"]<-"Taiwan"
data.bs$Location[data.bs$Location =="CÍ‰te d'Ivoire"]<-"Ivory Coast"
data.bs$Location[data.bs$Location =="CuraÍ_ao"]<-"Curacao"
data.bs$Location[data.bs$Location =="Czechia"]<-"Czech Republic"
data.bs$Location[data.bs$Location =="Dem. People's Republic of Korea"]<-"North Korea"
data.bs$Location[data.bs$Location =="Republic of Korea"]<-"South Korea"
data.bs$Location[data.bs$Location =="Iran (Islamic Republic of)"]<-"Iran"
data.bs$Location[data.bs$Location =="Lao People's Democratic Republic"]<-"Laos"
data.bs$Location[data.bs$Location =="Micronesia (Fed. States of)"]<-"Micronesia"
data.bs$Location[data.bs$Location =="Republic of Moldova"]<-"Moldova"
data.bs$Location[data.bs$Location =="RÍ©union"]<-"Reunion" 
data.bs$Location[data.bs$Location =="Russian Federation"]<-"Russia"
data.bs$Location[data.bs$Location =="Syrian Arab Republic"]<-"Syria" 
data.bs$Location[data.bs$Location =="State of Palestine"]<-"Palestine"
data.bs$Location[data.bs$Location =="TFYR Macedonia"]<-"Macedonia" 
data.bs$Location[data.bs$Location =="United Kingdom"]<-"UK"
data.bs$Location[data.bs$Location =="United Republic of Tanzania"]<-"Tanzania"
data.bs$Location[data.bs$Location =="United States of America"]<-"USA"
data.bs$Location[data.bs$Location =="United States Virgin Islands"]<-"Virgin Islands"
data.bs$Location[data.bs$Location =="Venezuela (Bolivarian Republic of)"]<-"Venezuela"
data.bs$Location[data.bs$Location =="Viet Nam"]<-"Vietnam"

# # Keep only data on total population
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
years <- as.numeric(names(table(data.tot$Time)))
countries <- names(table(data.tot$LocID))
ineq.men <- expand.grid(id = countries, year = years)
tt <- matrix(NA, nrow(ineq.men), 18)
ineq.men <- as.data.frame(cbind(ineq.men, tt))
names(ineq.men) <- c("id", "year", "country", "ISO3", "mean_c", "theil_c", "mld_c", "ge2_c",
                     "var_c", "gini_c", "ader_0", "ader_25", "ader_50", "ader_75", 
                     "ader_1", "rder_0", "rder_25", "rder_50", "rder_75", "rder_1")
start_time <- Sys.time()
for (i in 1:nrow(ineq.men)){
  ineq.men[i, 3] <- unique(data.tot$Location[data.tot$Time == ineq.men$year[i] 
                                       & data.tot$LocID == ineq.men$id[i]])
  ineq.men[i, 4] <- unique(data.tot$ISO3_code[data.tot$Time == ineq.men$year[i] 
                                             & data.tot$LocID == ineq.men$id[i]])
  dens.data <- data.tot$dx[data.tot$Time == ineq.men$year[i] &
                            data.tot$LocID == ineq.men$id[i]]/100000
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

years <- as.numeric(names(table(data.tot$Time)))
countries <- names(table(data.tot$LocID))
ineq.men.t <- expand.grid(id = countries, year = years)
tt <- matrix(NA, nrow(ineq.men), 7)
ineq.men.t <- as.data.frame(cbind(ineq.men.t, tt))
names(ineq.men.t) <- c("id", "year", "country", "mean_t", "theil_t", "mld_t", "ge2_t", "var_t", "gini_t")

for (i in 1:nrow(ineq.men)){
  ineq.men.t[i, 3] <- unique(data.tot$Location[data.tot$Time == ineq.men$year[i] 
                                             & data.tot$LocID == ineq.men$id[i]])
  data.temp <- data.tot$dx[data.tot$Time == ineq.men$year[i] &
                             data.tot$LocID == ineq.men$id[i]]/100000
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
#write.csv(datos, "ineq_pol_both.csv")

#-------------------------------------------------------------------#
# Add metadata
#-------------------------------------------------------------------#
meta <- read.csv("database_metadata.csv", header=T, sep = ";")
pop <- read.csv("population_both.csv", header=T,  sep = ";",na.strings = "...")
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
names(data.final)
write.csv(data.final, "ineq_pol_both.csv")

