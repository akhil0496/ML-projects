#IMPORT LIBRARIES
library(recommenderlab)
library(data.table)
library(ggplot2)
library(reshape2)

#SET PATH DIRECTORY AND READ THE DATA
setwd("E:/R practices/IMDB-Dataset")                      
movie_data <- read.csv("movies.csv",stringsAsFactors=FALSE)
r?ting_data <- read.csv("ratings.csv")

#DESCRIPTION OF THE DATA
str(movie_data)
summary(movie_data)
head(movie_data)

str(rating_data)
summary(rating_data)   
head(rating_data)

#FIRST WE NEED TO CONVERT GENRES COLUMN FROM MOVIE_DATA INTO INTEGERS FOR FURTH?R STUDY
#USE ONE-HOT ENCODING
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors=FALSE)

#AS WE GET ALL MOVIE GENRES IN ONE COLUMN ITS DIFFICULT TO PREDICT, SO SPLLIT THE USERS MOVIE GENRES
library(data.table)
movie_genre2 <- as.data.frame(ts?rsplit(movie_genre[,1], '[|]', 
                                        type.convert=TRUE), 
                              stringsAsFactors=FALSE) 

colnames(movie_genre2) <- c(1:10)
list_genre <- c("Action", "Adventure", "Animation", "Children", 
        ?       "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

genre_mat1 <- matrix(0,10330,18)
genre_mat1[1,] <- list_genre
colnam?s(genre_mat1) <- list_genre
for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col])
    genre_mat1[index+1,gen_col] <- 1
  }
}

genre_mat2 <- as.data.frame(genre_mat1[-1,], ?tringsAsFactors=FALSE) #remove first row, which was the genre list
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col]) #convert from characters to integers
} 
str(genre_mat2)

SearchMatrix <- cbind(movie_data[,1:2], genre_m?t2[])
head(SearchMatrix)

#TO MAKE MORE SENSE AND UNDERSTANDABLE CONVERT INTO SPARSE MATRIX
ratingMatrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds

#Convert rating ?atrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix

#NOW LETS OVERVIEW SOME IMPORTANT PARAMETERS, WHICH HELPS TO DEVELOP RECOMMENDATIONS SYSTEMS 
recommendation_model <- recommenderRegistry$get_entri?s(dataType = "realRatingMatrix")
names(recommendation_model)

lapply(recommendation_model, "[[", "description")

#IMPLEMENT ITEM BASED COLLABORATIVE FILTERING
recommendation_model$IBCF_realRatingMatrix$parameters

#EXPLORING SIMILAR DATA
similarity_mat <- ?imilarity(ratingMatrix[1:4, ],
                             method = "cosine",
                             which = "users")
as.matrix(similarity_mat)
image(as.matrix(similarity_mat), main = "User's Similarities")

#USERS SIMILARITIES WITH MOST ACCURATELY ?ONE
#NOW DO SIMILAARITY THAT IS SHARED BETWEEN THE MOVIES
movie_similarity <- similarity(ratingMatrix[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(movie_similarity)
image(as.matrix(movie_similarity), main = "Movies?similarity")

#EXTRACT THE MOST UNIQUE RATINGS
rating_values <- as.vector(ratingMatrix@data)
unique(rating_values) # extracting unique ratings

#CREATE A TABLE OF RATINGS WHICH DISPLAYS THE MOST UNIQUE RATINGS
Table_of_Ratings <- table(rating_values) # cre?ting a count of movie ratings
Table_of_Ratings

#MOST VIEWED MOVIES VISUALIZATIONS
library(ggplot2)
movie_views <- colCounts(ratingMatrix) # count views for each movie
table_views <- data.frame(movie = names(movie_views),
                          views = ?ovie_views) # create dataframe of views
table_views <- table_views[order(table_views$views,
                                 decreasing = TRUE), ] # sort by number of views
table_views$title <- NA
for (index in 1:10325){
  table_views[index,3] <- as.charac?er(subset(movie_data,
                                              movie_data$movieId == table_views[index,1])$title)
}
table_views[1:6,]

#BARPLOTS OF TOTAL VIEWS OF THE TOP FILMS
ggplot(table_views[1:6, ], aes(x = title, y = views)) +
  geom_bar(stat="i?entity", fill = 'steelblue') +
  geom_text(aes(label=views), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Total Views of the Top Films")

#HEATMAP OF MOVIE RATINGS
image(ratingMatrix[1:20, 1:25], axes = FA?SE, main = "Heatmap of the first 25 rows and 25 columns")

#PERFORMING DATA PREPARATION
movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50,
                              colCounts(ratingMatrix) > 50]
movie_ratings

#From the above output of 'movie_?atings', we observe that there are 420 users and 447 films as opposed to the previous 668 users and 10325 films.
#We can now accurate our matrix of relevant users 
minimum_movies<- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCount?(movie_ratings), 0.98)
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                    colCounts(movie_ratings) > minimum_users],
      main = "Heatmap of the top users and movies")

#DISTRIBUTION OF AVERAGE RATING PER USER
average_ratin?s <- rowMeans(movie_ratings)
qplot(average_ratings, fill=I("steelblue"), col=I("red")) +
  ggtitle("Distribution of the average rating per user")

#DATA NORMALIZATION
normalized_ratings <- normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001?
image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
                         colCounts(normalized_ratings) > minimum_users],
      main = "Normalized Ratings of the Top Users")

#PERFORMING DATA BINARIZATION
binary_minimum_movies <- q?antile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)

movies_watched <- binarize(movie_ratings, minRating = 1)
good_rated_films <- binarize(movie_ratings, minRating = 3)
image(good_rated_films[rowCounts(mo?ie_ratings) > binary_minimum_movies,
                       colCounts(movie_ratings) > binary_minimum_users],
      main = "Heatmap of the top users and movies")

#SPLIT THA DATA INTO 80% TRAIN AND 20%TEST
sampled_data<- sample(x = c(TRUE, FALSE),
        ?             size = nrow(movie_ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]

#MODEL BUILDING- Recommendation System 
?ecommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
               ?              parameter = list(k = 30))
recommen_model
class(recommen_model)

#EXPLORE THE MODEL
model_info <- getModel(recommen_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items],
      main = "?eatmap of the first rows and columns")

#VISUALIZE BY DISTRIBUTION OF THE COLUMN COUNT
sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)
sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill=I("steelblue"), col=I("red"))+ ggtitle("Distribution?of the column count")

#BUILD TOP RECOMMENDATIONS
#as we get to know the users intrestson movies by ratings, now check the top recommendations.
#use predict() function, that will identify similar items and will rank them appropriately.
top_recommendations ?- 10 # the number of items to recommend to each user
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommen?ations

user1 <- predicted_recommendations@items[[1]] # recommendation for the first user
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10){
  movies_user2[index] <- as.character(subset(movie_data,?                                             movie_data$movieId == movies_user1[index])$title)
}
movies_user2

recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[?]) }) # matrix with the recommendations for each user
#dim(recc_matrix)
recommendation_matrix[,1:4]

