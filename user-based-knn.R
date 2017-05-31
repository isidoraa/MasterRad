###############################################################################################
# SISTEM ZA PREPORUCIVANJE NA OSNOVU OKOLINE KORISNIKA
# UZ POMOC KORISCENJA METODE K NAZBLIZIH SUSEDA
###############################################################################################
library(reshape2)

# broj suseda
k = 30
#granica odluke
probability_threshold = 0.5

# funkcija za prionalazenje k najblizih suseda za svakog korisnika
k_nearest_for_user = function(userId, distances, k = 30)
{
  userDistances = distances[userId,-userId]
  userDistances = rbind(userDistances, as.integer(names(userDistances)))
  userDistances = userDistances[,!is.na(userDistances[1,])]
  random_array = c(1:ncol(userDistances))
  random_array = sample(random_array, length(random_array))
  userDistances = rbind(userDistances, random_array)
  kNN = userDistances[2,order(userDistances[1,], userDistances[3,])]
  if(length(kNN) > k)
    kNN = kNN[1:k]
  return(kNN)
}

# funkcija koja za zadati par (korisnik, film) daje verovatnocu da ce se korisniku taj film dopasti
like_probability = function(userId, movieId, distances, ratingmat, k = 30)
{
  probability = 0
  neighbors = k_nearest_for_user(userId, distances, k = k)
  ratings = ratingmat[movieId, neighbors]
  ratings = ratings[!is.na(ratings)]
  ratings[ratings == -1] = 0
  if (length(ratings) > 0)
    probability = mean(ratings)
  return(probability)
}

# funkcija koja za zadati par (korisnik, film) daje predikciju ocene koju bi korisnik dao filmu
estimate_rating = function(userId, movieId, distances, ratingmat, k = 30, probability_threshold = 0.5)
{
  rating = -1
  probability = like_probability(userId, movieId, distances, ratingmat, k = 30)
  if (probability > probability_threshold)
  {
    rating = 1
  }
  return(rating)
}

# funkcija za predvidjanje ocena na test podacima
estimate_ratings_on_test = function(ratingsDF_train, ratingsDF_test, distances, ratingmat, 
                                    k = 30, probability_threshold = 0.5)
{
  n = nrow(ratingsDF_test)
  rating_predictions = matrix(0, ncol=3, nrow=n)
  colnames(rating_predictions) = names(ratingsDF_test)
  
  users = unique(ratingsDF_train$userId)
  users = rbind(sort(users), c(1:length(users)))
  rownames(users) = c("userId", "num")
  
  for (i in 1:nrow(ratingsDF_test))
  {
    userId = ratingsDF_test[i,1]
    movieId = ratingsDF_test[i,2]
    userIdForPrediction = users[2, users[1,] == userId]
    if (!is.numeric(userIdForPrediction))
      rating_prediction = -1
    if (is.numeric(userIdForPrediction))
    {
    rating_predictions[i,] = c(userId, movieId, 
                   estimate_rating(userIdForPrediction, movieId, distances, ratingmat, k, probability_threshold))
    }
  }
  return(rating_predictions)
}

# funkcija za davanje N najboljih preporuka svakom korisniku
generate_top_n_recommendations = function(distances, ratingmat, N = 3, k = 30, probability_threshold = 0.5)
{
  userCount = nrow(distances)
  topNRecommendations = matrix(0, nrow = userCount, ncol = N+1)
  
  for (i in 1:userCount)
  {
    topNRecommendations[i,1] = i
    kNN = k_nearest_for_user(i, distances, k = 30)
    ratings = as.matrix(ratingmat[, kNN])
    maxRatings = apply(ratings, MARGIN = 1, function(x) max(x, na.rm = TRUE))
    movieIds = c(1:length(maxRatings))
    positiveRatingExists = maxRatings == 1
    ratings = ratings[positiveRatingExists,]
    if(nrow(ratings) > 0)
    {
      movieIds = movieIds[positiveRatingExists]
      means = apply(ratings, MARGIN = 1, function(x) mean(x, na.rm = TRUE))
      means = rbind(movieIds, means)
      means = means[, means[2,] > 0]
      random_array = c(1:ncol(means))
      random_array = sample(random_array, length(random_array))
      topNMovies = means[1, order(-means[2,], random_array)]
      if(length(topNMovies) > N)
        topNMovies = topNMovies[1:N]
      topNRecommendations[i,2:(N+1)] = topNMovies
    }
  }
  return(topNRecommendations)
}

# kreiranje modela -----------------------------------------------------------------
ratingmat = dcast(ratingsDF_train, movieId ~ userId, value.var = "rating", na.rm = FALSE)
ratingmat = ratingmat[,-1]
# korelacije izmedju korisnika kao mera slicnosti
similarities = cor(ratingmat, use = "pairwise.complete.obs")
# razdraljina po preporuci iz Machine Learning for Hackers
distances = -log((similarities / 2) + 0.5)
# brisanje nepotrebnih podataka
rm(similarities)
# predikcije ocena na test podacima
ratingOnTest = estimate_ratings_on_test(ratingsDF_train, ratingsDF_test, distances, ratingmat)
# -----------------------------------------------------------------

# opsta evaluacija ---------------------------------------------------------
# tacnost, preciznost, odziv i f1 metrika modela
resultsOnTest_userBasedkNN = evaluate_on_test(ratingsDF_test, ratingOnTest)
# brisanje nepotrebnih podataka
rm(ratingOnTest)
# davanje N najboljih preporuka svakom korisniku
#ovo ce trajati 2 sata!
topNRecommendations = generate_top_n_recommendations(distances, ratingmat, N = 3)
# udeo filmova i udeo korisnika za koje postoji bar jedna preporuka
coverageResults_userBasedkNN = coverage(topNRecommendations, userCount, movieCount)
# brisanje nepotrebnih podataka
rm(topNRecommendations)
# rezultati se cuvaju u fajlu
write.table(resultsOnTest_userBasedkNN, file = "resultsOnTest_userBasedkNN.txt")
write.table(coverageResults_userBasedkNN, file = "coverageResults_userBasedkNN.txt")
# -----------------------------------------------------------------------


