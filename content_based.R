###############################################################################################
# SISTEM ZA PREPORUCIVANJE NA OSNOVU SADRZAJA
###############################################################################################

# funkcija za uklanjanje filmova za koje ne postoje podaci o zanrovima
remove_movies_without_genres = function(ratingsDF, moviesDF)
{
  ratedMovies = unique(ratingsDF$movieId)
  withoutGenre = setdiff(ratedMovies, moviesDF$movieId)
  ratingsDF$movieId %in% withoutGenre == FALSE
  ratingsDF = ratingsDF[ratingsDF$movieId %in% withoutGenre == FALSE, ]
  return(ratingsDF)
}

# funkcija za pravljenje opsteg profila korisnika na osnovu ocena koje su davali
form_user_profiles = function(ratingsDF, moviesDF, genreList)
{
  userProfiles = matrix(0, nrow = length(unique(ratingsDF$userId)), ncol = ncol(moviesDF) - 2)
  colnames(userProfiles) = c("userId", genreList)
  
  moviesDFm = as.matrix(moviesDF)
  
  for(i in unique(ratingsDF$userId))
  {
    userProfiles[i,1] = i
    userRatings = ratingsDF[ratingsDF$userId == i,]
    userRatings = userRatings[order(userRatings$movieId),]
    userMovies = moviesDFm[moviesDF$movieId %in% userRatings$movieId,c(4:ncol(moviesDFm))]
    userMovies = apply(userMovies, 1:2, as.integer)
    userProfile = t(as.matrix(userRatings[,3])) %*% userMovies
    userProfile[userProfile < 0] = -1
    userProfile[userProfile > 0] = 1
    userProfiles[i,2:ncol(userProfiles)] = userProfile
  }
  
  return(userProfiles)
}

# funkcija zapredvidjanje ocena
# kao mera slicnosti se koristi kosinusna slicnost
generate_rating_predictions = function(userProfiles, moviesDF)
{
  userMovieMatrix = matrix(0, nrow = nrow(userProfiles), ncol = nrow(moviesDF))
  moviesDFm = t(as.matrix(moviesDF))[-c(1:3),]
  moviesDFm = apply(moviesDFm, MARGIN=c(1,2), as.integer)
  userProfilesM = userProfiles[,-1]
  userMovieMatrix = userProfilesM %*% moviesDFm
  
  rownames(userMovieMatrix) = userProfiles[,1]
  colnames(userMovieMatrix) = moviesDF$movieId
  
  return(userMovieMatrix)
}

# funkcija za predvidjanje ocena na test podacima
generate_ratings_on_test = function(ratingsDF_test, userMovieMatrix)
{
  n = nrow(ratingsDF_test)
  rating_predictions = matrix(0, ncol=3, nrow=n)
  colnames(rating_predictions) = names(ratingsDF_test)
  
  for (i in 1:n)
  {
    user = ratingsDF_test[i,1]
    movie = ratingsDF_test[i,2]
    rating_prediction = sign(userMovieMatrix[rownames(userMovieMatrix) == user, colnames(userMovieMatrix) == movie])
    #ukoliko je predikcija 0 (korisnik je neutralan), to cemo racunati kao negativnu ocenu
    if(rating_prediction > 0)
      rating_prediction = 1
    else
      rating_prediction = -1
    rating_predictions[i,] = c(user, movie, rating_prediction)
  }
  
  return(rating_predictions)
}

# funkcija za davanje N najboljih preporuka svakom korisniku
generate_top_n_recommendations = function(userMovieMatrix, ratingsDF_train, N = 3)
{
  userCount = nrow(userMovieMatrix)
  topNRecommendations = matrix(0, nrow = userCount, ncol = N+1)
  
  cols = c("userId")
  for (i in 1:N)
    cols = c(cols, paste0("rec", i))
  colnames(topNRecommendations) = cols
  
  for (i in 1:userCount)
  {
    user = as.integer(rownames(userMovieMatrix)[i])
    seen = ratingsDF_train[ratingsDF_train[,1] == rownames(userMovieMatrix)[i],2]
    ratings = userMovieMatrix[i,]
    ratings = ratings[ratings > 0]
    if(length(ratings) > 0)
    {
      # ovo sluzi za slucajan izbor filmova ako su predikcije ocena iste
      random_array = c(1:length(ratings))
      random_array = sample(random_array, length(random_array))
      ratings = rbind(as.integer(names(ratings)), ratings, random_array)
      ratings = ratings[1,order(-ratings[2,], random_array)]
      if (length(ratings) > N)
        ratings = ratings[1:N]
      topNRecommendations[i,1:(length(ratings) + 1)] = c(user, ratings)
    }
    else
    {
      topNRecommendations[i,1] = user
    }
  }
  return(topNRecommendations)
}

# kreiranje modela -----------------------------------------------------------------
# uklanjanje filmova za koje ne postoje podaci o zanrovima
ratingsDF_train2 = remove_movies_without_genres(ratingsDF_train, moviesDF)
# formiranje korisnickih profila
userProfiles = form_user_profiles(ratingsDF_train2, moviesDF, genreList)
# brisanje nepotrebnih podataka
rm(ratingsDF_train2)
# formiranje matrice sa predikcijama ocena za svaku kombinaciju (korisnik, film)
userMovieMatrix = generate_rating_predictions(userProfiles, moviesDF)
# brisanje nepotrebnih podataka
rm(userProfiles)
# predikcije ocena na test podacima
ratingOnTest = generate_ratings_on_test(ratingsDF_test, userMovieMatrix)
# -----------------------------------------------------------------------

# opsta evaluacija ---------------------------------------------------------
# tacnost, preciznost, odziv i f1 metrika modela
resultsOnTest_content = evaluate_on_test(ratingsDF_test, ratingOnTest)
# brisanje nepotrebnih podataka
rm(ratingOnTest)
# davanje N najboljih preporuka svakom korisniku
#ovo ce trajati 2 sata!
topNRecommendations = generate_top_n_recommendations(userMovieMatrix, ratingsDF_train, N = 3)
# brisanje nepotrebnih podataka
rm(userMovieMatrix)
# udeo filmova i udeo korisnika za koje postoji bar jedna preporuka
coverageResults_content = coverage(topNRecommendations, userCount, movieCount)
# brisanje nepotrebnih podataka
rm(topNRecommendations)
# rezultati se cuvaju u fajlu
write.table(resultsOnTest_content, file = "resultsOnTest_content.txt")
write.table(coverageResults_content, file = "coverageResults_content.txt")
# -----------------------------------------------------------------------
