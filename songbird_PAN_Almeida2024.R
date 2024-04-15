#'CODE ASSOCIATED WITH: ALMEIDA ET AL. 2024, PEOPLE & NATURE
#'
#'
#'AUTHOR: Ryan J. Almeida
#'
#'DATE: 2024/03/08
#'
#'REQUIRED PACKAGES: tidyverse
#'
#'DESCRIPTION: This script contains the code for the analyses conducted
#'in Almeida et al. (2024): Does fortune follow function? Exploring how 
#'consumer preferences drive the functional trait composition of the 
#'global songbird trade. 

#### Load necessary packages and data----
#load tidyverse
install.packages("tidyverse")
library(tidyverse)

#set working directory to path containing "songbird_df_full.csv"
setwd("SET_WORKING_DIRECTORY_HERE")

#read songbird data
sb.df <- read.csv("songbird_df_full.csv")

#### Distribution of functional traits in trade-threatened species----

#create vector of all functional traits
func.traits <- c("Diet.Vend", "Diet.Vect", "Diet.Vfish", 
                 "Diet.Scav", "Diet.Inv", "Diet.Fruit", 
                 "Diet.Seed", "Diet.Nect", "Diet.PlantO",
                 "ForStrat.aerial", "ForStrat.ground", "ForStrat.understory", 
                 "ForStrat.midhigh", "ForStrat.canopy", "logBodyMass")

#create dataframe of functional traits for only trade-threatened birds
traded.traits.df <- filter(sb.df, trade.binary == "Trade-threatened")[,func.traits]

#determine whether trait distribution is random
nsim <- 10000
traits.df <- data.frame()

#Simulate diet trait distribution
for (i in 1:length(func.traits)){ #for each functional trait
  
  #store current functional trait
  trait <- func.traits[i]
  
  #print trait to console for tracking
  print(trait)
  
  #create df of just this trait and trade status, removing spp w/o data
  trait.df <- na.omit(sb.df[,c(trait, 'trade.binary')])
  
  #determine number of traded birds with this trait data
  n.traded <- nrow(filter(trait.df, trade.binary == "Trade-threatened"))
  
  #initalize vector to store simulated trait values
  trait.vals <- rep(0,nsim)
  
  #generate null-simualted trait values
  for (j in 1:nsim){
    #randomly sample bird species with n = number of traded birds
    rpull <- sample(1:nrow(trait.df), n.traded, replace = F)
    
    #create dataframe with trait data for randomly sampled birds
    sim <- trait.df[rpull,]
    
    #calculate mean null-simulated trait value
    trait.vals[j] <- mean(sim[,trait])
  }
  #save null-simulated data
  current <- data.frame(val = mean(trait.vals), 
                        upper = quantile(trait.vals, 0.975)[[1]], 
                        lower = quantile(trait.vals, 0.025)[[1]], 
                        trait = trait, group = "Null simulated")
  
  #save trait data for trade-threatened species
  traded.df <- data.frame(val = mean(traded.traits.df[,trait]), 
                          upper = NA, 
                          lower = NA, 
                          trait = trait, 
                          group = "Trade observed")
  
  #add null and observed data to data frame
  traits.df <- rbind(traits.df, traded.df, current)
}

#### Distribution of market traits in functional groups-----
#set number of simulations
nsim <- 10000

#create vector of functional group categories
diet.cats <- c("FruiNect", "Invertebrate", "Omnivore", "PlantSeed")

strat.cats <- c("ForStrat.canopy", "ForStrat.ground", "ForStrat.midhigh", 
                "ForStrat.understory", "Mixed strategies")

func.cats <- c(diet.cats,strat.cats)

#create vector of market traits
market.traits <- c("CU_c_spMax", "resid.songQuality", "rangeExtent",
                   "ED", "logBodyMass", "popHigh")

#create dataframe to store results
func.market.df <- data.frame()

#determine null-simulated market trait values for each functional group
#for each functional group
for (i in 1:length(func.cats)){ 
  
  #store current group
  func <- func.cats[i] 
  
  if(func %in% diet.cats){
    #get birds in current func group 
    func.group.df <- sb.df %>% 
      filter(Diet.5Cat == func)
  }
  else{
    #get birds in current func group 
    func.group.df <- sb.df %>% 
      filter(ForStrat.Category == func)
  }
  
 
  
  #for each market trait
  for (j in 1:length(market.traits)){
    
    #store current market trait
    market <- market.traits[j]
    
    #create vector to store simulated trait vals
    market.vals.null <- rep(NA,nsim) 
    
    print(paste(func,market))
    
    #for 10000 iterations
    for (k in 1:nsim){ 
      #randomly draw group of birds with size equivalent to current func group
      rpull <- sample(1:nrow(sb.df), #from all bird species
                      nrow(func.group.df), #sample out spp = nspp func group
                      replace = F)
      
      #look at randomly selected birds
      sim <- sb.df[rpull,] 
      
      #store mean market trait value for this iteration
      market.vals.null[k] <- mean(sim[,market],na.rm = T)
    }
    
    #store results of simulation
    null.market.df <- data.frame(val = mean(market.vals.null,na.rm = T), #mean simulated trait value
                                 upper = quantile(market.vals.null, #upper bound of 95% CI
                                                  0.975,
                                                  na.rm = T)[[1]],
                                 lower = quantile(market.vals.null, #lower bound of 95% CI
                                                  0.025,
                                                  na.rm = T)[[1]],
                                 observed = "Null simulated", #indicate this is null simulated
                                 func = func, #current functional group
                                 market.trait = market, #current market trait
                                 percentile = pnorm(q = mean(func.group.df[,market],na.rm = T), #percentile of observed data relative to null simulated
                                                    mean = mean(market.vals.null),
                                                    sd = sd(market.vals.null)))
    
    #store observed market trait data
    observed.market.df <- data.frame(val = mean(func.group.df[,market],na.rm = T), #observed value of trait
                                     upper = NA,
                                     lower = NA,
                                     observed = "Observed", #indicate this is the actual trait value
                                     func = func, #current functional group
                                     market.trait = market, #current market trait
                                     percentile = pnorm(q = mean(func.group.df[,market],na.rm = T), #percentile of observed data relative to null simulated
                                                        mean = mean(market.vals.null),
                                                        sd = sd(market.vals.null)))
    
    #if market trait is abundance, percentile is 1-percent to look at rarity
    if (market == "popHigh"){
      null.market.df$percentile <- 1 - null.market.df$percentile
      observed.market.df$percentile <- 1 - observed.market.df$percentile
    }
    
    #add results to overall data frame
    func.market.df <- rbind(func.market.df,
                            null.market.df,
                            observed.market.df)
    
  }
}
#### Correlation analysis----
#market traits
market.traits <- c("CU_c_spMax", "resid.songQuality",
                   "logRangeExtent","ED", "logBodyMass", "popHigh")

#All continuous foraging variables
strat.traits.cont <- c("ForStrat.ground", "ForStrat.understory", 
                       "ForStrat.midhigh", "ForStrat.canopy",  "ForStrat.aerial")

#Identify continuous diet variables
diet.traits.cont <- c("Diet.Inv", "Diet.Vend", "Diet.Vect", 
                      "Diet.Scav", "Diet.Fruit", 
                      "Diet.Seed", "Diet.Nect", "Diet.PlantO")

#create dataframe of just trade-threatened species
trade.df <- sb.df %>% 
  filter(trade.binary == "Trade-threatened")

#intialize dataframe to store correlation values
cor.matrix <- data.frame()

#calculate spearman correlation coefficient for each pairwaise combination
# of traits for both all species and just trade-threatened

#for first trait
for (trait1 in c(diet.traits.cont,strat.traits.cont,market.traits)){
  
  #for second trait
  for (trait2 in c(diet.traits.cont,strat.traits.cont,market.traits)){
    
    #create dataframe of two traits for all songbirds
    cor.all.df <- filter(na.omit(sb.df[,c(trait1,trait2)]), 
                         trait1 > 0, trait2 > 0)
    #create dataframe of two traits for trade-threatened songbirds
    cor.trade.df <- filter(na.omit(trade.df[,c(trait1,trait2)]),
                           trait1 > 0, trait2 > 0)
    
    #calculate correlation coefficient for all birds
    cor.all <- cor(cor.all.df[,trait1],cor.all.df[,trait2])
    
    #calculate correlation coefficient for trade-threatened
    cor.trade <- cor(cor.trade.df[,trait1],cor.trade.df[,trait2])
    
    #add results to output dataframe
    cor.matrix <- rbind(cor.matrix,
                        data.frame(trait1 = trait1, 
                                   trait2 = trait2,
                                   val = cor.all, 
                                   group = "All songbirds",
                                   ncount = nrow(cor.all.df)),
                        data.frame(trait1 = trait1, 
                                   trait2 = trait2,
                                   val = cor.trade, 
                                   group = "Trade-threatened songbirds",
                                   ncount = nrow(cor.trade.df)))
  }
}

#if correlation is between the same variable twice, make NA
cor.matrix$val[which(cor.matrix$trait1 == cor.matrix$trait2)] <- NA

#if trait combination has insufficient sample size, make NA
cor.matrix$val[which(cor.matrix$ncount < 30)] <- NA

#filter correlation matrix to only look at links 
#betweeen functional and market traits
market.func.df <- filter(cor.matrix, 
                         trait1 %in% market.traits, #only market traits
                         trait2 %in% c(diet.traits.cont, #only functional traits
                                       strat.traits.cont,
                                       "logBodyMass"))

#recode market traits with more descriptive names
market.func.df$trait1 <- recode(market.func.df$trait1,  
                                logBodyMass = "Body size",
                                resid.songQuality = "Song quality", popHigh = "Abundance",
                                logRangeExtent = "Range extent", ED = "Distinctiveness",
                                CU_c_spMax = "Color")

#recode functional traits with more descriptive names
market.func.df$trait2 <- recode(market.func.df$trait2,  
                                Diet.Fruit = "Fruit", Diet.Nect = "Nectar",
                                Diet.PlantO = "Plant", Diet.Scav = "Carrion",
                                Diet.Seed = "Seeds", Diet.Vect = "Ectotherms",
                                Diet.Vend = "Endotherms", Diet.Vunk = "Unknown",
                                ForStrat.aerial = "Aerial",  Diet.Inv = "Invertebrates",
                                ForStrat.ground = "Ground", ForStrat.understory = "Understory",
                                ForStrat.midhigh = "Mid-high", ForStrat.canopy = "Canopy",
                                logBodyMass = "Body size")

#test whether correlations are higher among traded birds
#data is normal, but failed variance test
t.test(abs(market.func.df$val[market.func.df$group == "All songbirds"]),
       abs(market.func.df$val[market.func.df$group == "Trade-threatened songbirds"]),
       var.equal = F,alternative = "less")