###############################################################################################
# PRIPREMA PODATAKA
###############################################################################################

# biblioteke, podesavanja, parametri ---------------------------------------------------------
library(data.table)
library(geometry)

options(stringsAsFactors=FALSE)

# podesavanje direktorijuma za rad
#setwd("C:/Users/isido/Dropbox/Master rad/MasterR")
setwd("C:/Users/isidora.jovandic/Dropbox/Master rad/MasterR")

#lista svih zanrova
genreList = c("Action", "Adventure", "Animation", "Children's", "Comedy", "Crime",
              "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical",
              "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")

# udeo ocena koji se koristi za pravljenje modela
train_proportion = 0.8
# ------------------------------------------------------------------------------------

#binarna ocena: uzimamo da se korisniku film dopao ako ga je ocenio sa 4 ili 5 (to kodiramo sa 1)
# ili da mu se nije dopao ako ga je ocenio sa 1,2 ili 3 (to kodiramo sa -1)
binaryRating = function(a)
{
  if (a > 3) {a = 1}
  else {a = -1}
}

#funkcija za unos podataka o ocenama
prep_ratings = function()
{
  ratingsDF = as.data.table(do.call(rbind,
                                    strsplit(readLines("./ml-1m/ratings.dat"),"::")))
  ratingsDF = ratingsDF[, 1:3]
  colnames(ratingsDF) = c("userId","movieId", "rating")
  
  ratingInt = array(0, dim = c(1,length(ratingsDF$rating)))
  userIdInt = ratingInt
  movieIdInt = ratingInt
  
  for (i in 1:length(ratingsDF$rating))
  {
    ratingInt[i] = as.integer(ratingsDF$rating[i])
    ratingInt[i] = binaryRating(ratingInt[i])
    userIdInt[i] = as.integer(ratingsDF$userId[i])
    movieIdInt[i] = as.integer(ratingsDF$movieId[i])
  }
  
  ratingsDF = cbind.data.frame(t(userIdInt), t(movieIdInt), t(ratingInt))
  colnames(ratingsDF) = c("userId", "movieId", "rating")
  
  return(ratingsDF)
}

#funkcija za unos podataka o filmovima
prep_genres = function(genreList)
{
  moviesDF = as.data.table(do.call(rbind,
                                   strsplit(readLines("./ml-1m/movies.dat"),"::")))
  colnames(moviesDF) = c("movieId", "name", "genres")
  
  movieIdInt = array(0, dim = c(1,length(moviesDF$movieId)))
  for (i in 1:length(moviesDF$movieId))
    movieIdInt[i] = as.integer(moviesDF$movieId[i])
  
  moviesDF = moviesDF[,-1]
  moviesDF = cbind(t(movieIdInt), moviesDF)
  colnames(moviesDF)[1] = "movieId"
  
  # razdvajanje stringa sa zanrovima na odvojene binarne promenljive
  genreMatrix = matrix(0, nrow = nrow(moviesDF), ncol = length(genreList))
  colnames(genreMatrix) = make.names(genreList)
  
  for (i in 1:length(genreList))
    for (j in 1:nrow(moviesDF))
      if (grepl(genreList[i], moviesDF$genres[j]))
        genreMatrix[j,i] = 1
  
  moviesDF = cbind(moviesDF, genreMatrix)
  
  return(moviesDF)
}

# funkcija za uklanjanje irelevantnih zanrova (onih za koje ni jedan film nema ni jednu ocenu)
remove_unrated_genres = function(ratingsDF, moviesDF, genreList)
{
  moviesDF = as.matrix(moviesDF)
  
  ratedMovies = unique(ratingsDF$movieId)
  unratedGenres = c()
  
  for (i in 1:length(genreList))
  {
    movies = as.integer(moviesDF[moviesDF[,i+3] ==1,1])
    rated = length(intersect(movies, ratedMovies))
    if(rated==0)
      unratedGenres = c(unratedGenres, i+3)
  }
  for (i in unratedGenres)
    moviesDF = (moviesDF[,-i])
  
  moviesDF = as.data.table(moviesDF)
  moviesDF = transform(moviesDF, movieId = as.integer(movieId))
  
  moviesDF = transform(moviesDF, Action = as.integer(Action))
  moviesDF = transform(moviesDF, Adventure = as.integer(Adventure))
  moviesDF = transform(moviesDF, Animation = as.integer(Animation))
  moviesDF = transform(moviesDF, Children.s = as.integer(Children.s))
  moviesDF = transform(moviesDF, Comedy = as.integer(Comedy))
  moviesDF = transform(moviesDF, Crime = as.integer(Crime))
  moviesDF = transform(moviesDF, Documentary = as.integer(Documentary))
  moviesDF = transform(moviesDF, Drama = as.integer(Drama))
  moviesDF = transform(moviesDF, Fantasy = as.integer(Fantasy))
  moviesDF = transform(moviesDF, Film.Noir = as.integer(Film.Noir))
  moviesDF = transform(moviesDF, Horror = as.integer(Horror))
  moviesDF = transform(moviesDF, Musical = as.integer(Musical))
  moviesDF = transform(moviesDF, Mystery = as.integer(Mystery))
  moviesDF = transform(moviesDF, Romance = as.integer(Romance))
  moviesDF = transform(moviesDF, Sci.Fi = as.integer(Sci.Fi))
  moviesDF = transform(moviesDF, Thriller = as.integer(Thriller))
  moviesDF = transform(moviesDF, War = as.integer(War))
  moviesDF = transform(moviesDF, Western = as.integer(Western))
  
  return(moviesDF)
}

prep_users = function()
{
  usersDF = as.data.table(do.call(rbind,
                                    strsplit(readLines("./ml-1m/users.dat"),"::"))) 
  usersDF = usersDF[, 1:4]
  names(usersDF) = c("userId", "sex", "age", "occupation")
  
  usersDF$userId = sapply(usersDF$userId, FUN = as.integer)
  usersDF$sex = factor(usersDF$sex)
  usersDF$age = factor(usersDF$age, ordered = TRUE)
  usersDF$occupation = factor(usersDF$occupation)
  
  return(usersDF)
}

# unos podataka ----------------------------------------------------------------------------
#unos ocena
ratingsDF = prep_ratings()
#podela ocena na podatke koji sluze za pravljenje modela i one koji sluze za njegovu evaluaciju
#NOTE: pravimo ili ucitamo stari
train_sample = sort(sample(nrow(ratingsDF), nrow(ratingsDF)*train_proportion))
train_sample = scan(file = "train_sample.txt")
# pamtimo ovo da bi rezultati mogli da se reprodukuju
write(train_sample, file = "train_sample.txt")
#podela podataka na trening i test
ratingsDF_train = ratingsDF[train_sample,]
ratingsDF_test = ratingsDF[-train_sample,]
#broj korisnika
userCount = length(unique(ratingsDF$userId))
# ova tabela vise nije potrebna
rm(ratingsDF)
#unos filmova
moviesDF = prep_genres(genreList)
#broj filmova
movieCount = nrow(moviesDF)
#uklanjanje nepotrebnih zanrova
moviesDF = remove_unrated_genres(ratingsDF_train, moviesDF, genreList)
#unos korisnika
usersDF = prep_users()
# -----------------------------------------------------------------------------------------







