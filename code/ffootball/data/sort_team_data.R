# choose a year and select a dataset

Yr <- c(2015,2016)
data_name <- paste0('games_',Yr,'.csv')

#  Read in the file a data.table named dt'
#  cause data.tables are awesome 

library(rprojroot)
root <- find_root(is_git_root)
File <- file.path(root,'code','ffootball','objs',data_name) 

dt <- data.table::data.table(NULL)

for(i in 1:length(Yr)) {
  
  dt <- rbind(dt, data.table::fread(file = File[i])) 
}
 
# Filter out the data we want an an object called tm

tm <- dt[,.(pass.TD  = sum(pass.tds), 
            rush.TD  = sum(rushtds),
            pass.int = sum(pass.ints),
            kick.TD  = sum(kickret.tds),
            punt.TD  = sum(puntret.tds),
            totl.TD  = sum(pass.tds,rushtds,kickret.tds,puntret.tds),
            pass.YD  = sum(passyds), 
            rush.YD  = sum(rushyds),
            totl.YD  = sum(passyds,rushyds)), 
         by = Team]

# Clean up some rows

if('JAX'%in%tm$Team & 'JAC'%in%tm$Team) {
  
  jac <- which(tm$Team=='JAC')
  jax <- which(tm$Team=='JAX')
  
  tm[jac,] <- c(tm[jac,1],tm[jax,-1] + tm[jac,-1])
  tm <- tm[-jax,]
}

if('STL'%in%tm$Team & 'LA'%in%tm$Team) {
  
  stl <- which(tm$Team=='STL')
  la  <- which(tm$Team=='LA')
  
  tm[la,] <- c(tm[la,1],tm[stl,-1] + tm[la,-1])
  tm <- tm[-stl,]
}

if(NA%in%tm$Team) tm <- tm[-which(is.na(tm$Team)),]

# Add some extra columns for the percentages

tm$pass.TD_percent <- round(tm$pass.TD / tm$totl.TD, digits = 2)
tm$rush.TD_percent <- round(tm$rush.TD / tm$totl.TD, digits = 2)
tm$pass.YD_percent <- round(tm$pass.YD / tm$totl.YD, digits = 2)
tm$rush.YD_percent <- round(tm$rush.YD / tm$totl.YD, digits = 2)
tm$pass.TD_heavy   <- as.factor((tm$pass.TD_percent > 0.6) * 1)
tm$pass.YD_heavy   <- as.factor((tm$pass.YD_percent > 0.6) * 1)
tm$pass.points     <- (tm$pass.TD * 6 + tm$pass.YD / 30 - tm$pass.int * 2) / 32 

## Assign it to an appropriately named object
## Run the above code for 2015, 2016 as training data and 
## Run the above code for 2017 to serve as test data

name = `if`(!2017%in%Yr, 'train_data','test_data')

assign(name, tm)

data.table::setkey(get(name), Team)

  