monty.hall = function(sims, decision = 'stay') {
  decision <- match.arg(decision, c("stay", "change"))
doors <- 1:3
first.picks <- sample(doors, size = 1000, replace = TRUE)
final.stage <- matrix(NA, nrow = length(first.picks), ncol = 2)
for (i in 1:length(first.picks)) {
  (if (first.picks[i] == 1)
    c(first.picks[i], sample(doors[-first.picks[i]], 1))
  else
    c(first.picks[i], 1))->result
  final.stage[i,] <- result
  }
  final.stage
  switch(decision, 
         "stay" = sum(final.stage[,1][final.stage[,1]==1],na.rm=T)/length(final.stage[,1]),
         "change" =  sum(final.stage[,2][final.stage[,2]==1],na.rm=T)/length(final.stage[,2]))
}  
