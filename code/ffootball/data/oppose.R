
# Find the opposing teams

oppose <- function(df, team, home_col, away_col)
{
  new_df <- as.data.frame(df)[, c(home_col, away_col)] 
  ind <- which(new_df != team, arr.ind = T)
  ord <- sort.list(ind[,1])
  opp_df <- sapply(X = 1:nrow(new_df), 
                   FUN = function(x) new_df[c(ind[ord,1][x]), c(ind[ord,2][x])])
  
  return(data.table(unlist(opp_df)))
}