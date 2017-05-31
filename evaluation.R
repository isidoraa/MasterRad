###############################################################################################
# EVALUACIJA MODELA
###############################################################################################

# funkcija koja uzima predikcije ocena i stvarne ocene na test podacima
# i na osnovu njih racuna tacnost, preciznost, odziv i f1 metriku modela
evaluate_on_test = function(ratingsDF_test, ratingOnTest)
{
  results = cbind(ratingsDF_test[,3], ratingOnTest[,3])
  colnames(results) = c("real", "predicted")
  
  tp = nrow(results[results[,1] == 1 & results[,2] == 1 ,])
  fp = nrow(results[results[,1] == -1 & results[,2] == 1 ,])
  tn = nrow(results[results[,1] == -1 & results[,2] == -1 ,])
  fn = nrow(results[results[,1] == 1 & results[,2] == -1 ,])  
  
  #accuracy
  acc = (tp + tn)/(tp + fp + tn + fn)
  #precission
  ppv = tp/(tp + fp)
  #recall
  tpr = tp/(tp + fn)
  #f1
  f1 = 2*ppv*tpr/(ppv + tpr)
  
  return(data.frame(acc, ppv, tpr, f1))
}

# funkcija koja na osnovu datih N preporuka za svakog korisnika
# racuna udeo filmova i udeo korisnika za koje postoji bar jedna preporuka
coverage = function(topNRecommendations, userCount, movieCount)
{
  usersWithoutRecommendations = length(topNRecommendations[topNRecommendations[,2] == 0,1])
  userCoverage = 1 - usersWithoutRecommendations/userCount
  recommendedMovies = unique(as.vector(topNRecommendations[,2:ncol(topNRecommendations)]))
  recommendedMoviesCount = length(recommendedMovies[!(recommendedMovies == 0)])
  movieCoverage = recommendedMoviesCount/movieCount
  return(data.frame(userCoverage, movieCoverage))
}