###############################################################################################
# SISTEM ZA PREPORUCIVANJE NA OSNOVU OKOLINE KORISNIKA
# UZ POMOC KORISCENJA LOGISTICKE REGRESIJE
###############################################################################################

#granica odluke
probability_threshold = 0.5

# ocenjivanje koeficijenata modela logisticke regresije
generate_logistic_model = function(usersDF, ratingsDF_train, moviesDF)
{
  trainData = merge(merge(usersDF, ratingsDF_train, by = "userId"), moviesDF, by = "movieId")
  trainData$rating[trainData$rating == -1] = 0
  trainData = trainData[, c(3:6, 9:26)]
  trainData = trainData[, c(1, 2, 4, 5, 7:16, 18:22)]
  model = glm(rating ~., family=binomial, data = trainData)
  return(model)
}

# funkcija za predvidjanje ocena na test podacima
predict_test_ratings = function(usersDF, ratingsDF_test, model, probability_threshold = 0.5)
{
  testData = merge(merge(usersDF, ratingsDF_test, by = "userId"), moviesDF, by = "movieId")
  testData$rating[testData$rating == -1] = 0
  testData = testData[, c(3:6, 9:26)]
  testData = testData[, c(1, 2, 4, 5, 7:16, 18:22)]
  ratingPred = predict(model, testData, type ="response")
  ratingPred[ratingPred > probability_threshold] = 1
  ratingPred[ratingPred <= probability_threshold] = 0
  return(ratingPred)
}

# funkcija koja uzima predikcije ocena i stvarne ocene na test podacima
# i na osnovu njih racuna tacnost, preciznost, odziv i f1 metriku modela
evaluate_on_test_logit = function(ratingPred, ratingReal)
{
  confMat = table(ratingPred, ratingReal)
  acc = mean(ratingPred == ratingReal)
  ppv = confMat[2,2]/sum(confMat[2,])
  tpr = confMat[2,2]/sum(confMat[,2])
  f1 = 2*ppv*tpr/(ppv+tpr)
  return(data.frame(acc, ppv, tpr, f1))
}

# funkcija za davanje N najboljih preporuka svakom korisniku
generate_top_n_recommendations = function(usersDF, moviesDF, ratingsDF_train, model, N = 3, 
                                          probability_threshold= 0.5)
{
  
  userCount = length(usersDF$userId)
  topNRecommendations = matrix(0, nrow = userCount, ncol = N+1)
  
  dummy = array(0, dim = nrow(moviesDF))
  moviesDF = cbind(moviesDF, dummy)
  
  for (i in 1:userCount)
  {
    topNRecommendations[i,1] = usersDF$userId[i]
    user = cbind(usersDF[i,], 0)
    data = merge(user, moviesDF, by = "V2", allow.cartesian = TRUE)
    ratingPred = predict(model, data, type ="response")
    
    random_array = c(1:length(ratingPred))
    random_array = sample(random_array, length(random_array))
    
    data = cbind(data$movieId, ratingPred, random_array)
    data = data[data[,2] > probability_threshold, ]
    data = data[order(-data[,2], data[,3]),1]
    if(length(data) > N)
      data = data[1:N]
    topNRecommendations[i,2:(N+1)] = data
  }
  
  return (topNRecommendations)
}

# kreiranje modela -----------------------------------------------------------------
model = generate_logistic_model(usersDF, ratingsDF_train, moviesDF)
# osnovne karakteristike modela
summary(model)
# predikcije ocena na test podacima
ratingPred = predict_test_ratings(usersDF, ratingsDF_test, model)
# -----------------------------------------------------------------

# opsta evaluacija ---------------------------------------------------------
# tacnost, preciznost, odziv i f1 metrika modela
resultsOnTest_hybridLogistic = evaluate_on_test_logit(ratingPred, ratingsDF_test$rating)
# brisanje nepotrebnih podataka
rm(ratingPred)
# davanje N najboljih preporuka svakom korisniku
topNRecommendations = generate_top_n_recommendations(usersDF, moviesDF, ratingsDF_train, model, N = 3)
# udeo filmova i udeo korisnika za koje postoji bar jedna preporuka
coverageResults_hybridLogistic = coverage(topNRecommendations, userCount, movieCount)
# brisanje nepotrebnih podataka
rm(topNRecommendations)
# rezultati se cuvaju u fajlu
write.table(resultsOnTest_hybridLogistic, file = "resultsOnTest_hybridLogistic.txt")
write.table(coverageResults_hybridLogistic, file = "coverageResults_hybridLogistic.txt")
# -----------------------------------------------------------------
