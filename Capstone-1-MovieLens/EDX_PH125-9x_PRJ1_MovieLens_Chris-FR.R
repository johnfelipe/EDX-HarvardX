################################################################################
# EDX_PH125-9x_PRJ1_MovieLens_Chris-FR.R
################################################################################
# This file is a conversion of the original RMD using knitr
#   library(knitr)
#   purl("EDX_PH125-9x_PRJ1_MovieLens_Chris-FR.Rmd", 
#          output = "EDX_PH125-9x_PRJ1_MovieLens_Chris-FR.R", documentation = 2)
################################################################################
#
#' ---
#' title: "PH125.9x Capstone Project 1: MovieLens"
#' author: "Chris-FR"
#' date: "29 janvier 2019"
#' output:
#'   pdf_document: default
#'   html_document:
#'     keep_md: yes
#' geometry: left=2cm,right=2cm,top=1cm,bottom=2cm
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set( fig.path = "figure/")
knitr::opts_chunk$set( fig.show = "hold")


################################################################################
############################# Introduction #####################################
################################################################################
#' 
#' # Introduction
#' 
#' For this first project of the EDX PH125.9x capstone course, we will be 
#' creating a movie recommendation system using the MovieLens dataset. 
#' We will use the **10M records** version of this dataset. As a large number 
#' of students, one of the issue of this project was the performance of my 
#' laptop (2003, 4Gb RAM) to process the `edx` dataset.  
#' In this report, we will:  
#' - do a quick data exploratory analysis,  
#' - then build 4 different models based on the `edx` dataset using Azure ML 
#' and the Penalized Least Squares regularization method  
#' - and to finish we will compute the RMSEs of a `validation` dataset.
#' 
#' 
################################################################################
####################### Data exploration and visualization ######################
################################################################################
#' # Data exploration and visualization
#' 
#' ## Loading data
#' 
#' To load the data, we used the code provided by the course page. We modified 
#' it to use the **fread** method of the **data.table** package.  
## ----edx_load, echo=FALSE, message=FALSE, cache=TRUE---------------------
#######################ORIGINAL##############################
#############################################################
# 
# NOTE: Updated 1/18/2019.
# 
# Create test and validation sets
#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- data.table::fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#' 
#' The `edx` dataset has **`r dim(edx)[1]`** rows and **`r dim(edx)[2]`** columns.  
#' The `validation` dataset has **`r dim(validation)[1]`** rows and **`r dim(validation)[2]`** columns.  
#'  
#' ## Movie genres dataset
#'   
#' We create a `movieGenres` matrix dataset containing the movieId as rows and genres as columns. We delete the "timestamp" and "title" columns from the `edx` and `validation` datasets.
#'   
## ----create_movies, cache=TRUE-------------------------------------------
# create the movieGenres dataset, containing the movieId as rows and the genres as columns
movieGenres <- edx %>% select(movieId, genres) %>% 
    unique() %>% 
    separate_rows(genres, sep = "\\|") %>%
    mutate(val=1) %>%
    spread(genres, val, fill=0)
# drop the "timestamp", "title" columns from edx and validation
edx$timestamp <- NULL
edx$title <- NULL
# we also drop the "genres" column from validation
validation$timestamp <- NULL
validation$title <- NULL
validation$genres <- NULL

head(movieGenres)

#'   
#' To store the different RMSE results, we define a `rmse_results` tibble.  
## ----create_rmse_results, cache=TRUE-------------------------------------
# creating the RMSE result table
rmse_results <- data_frame(method=character(0),  
                                     RMSE = numeric(0))

#' 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' ## Data visualization
#' 
#' This section displays some of the graphs used to analyse the data.  
#' We use the following consolidated data:    
#' 
## ----buil_stats_ds, cache=TRUE-------------------------------------------
stats_users <- edx %>% 
    group_by(userId) %>%
    summarize(countRating=n(), meanRating=mean(rating), medianRating=median(rating))

stats_movies <- edx %>% 
    group_by(movieId) %>%
    summarize(countRating=n(), meanRating=mean(rating), medianRating=median(rating))

stats_rating <- edx %>% 
    group_by(rating) %>%
    summarize(countRating=n())

stats_genres <- edx %>% 
    separate_rows(genres, sep = "\\|") %>%
    group_by(genres) %>%
    summarize(countRating=n(), meanRating=mean(rating), 
              medianRating=median(rating), countMovies=n_distinct(movieId))





# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' ### Ratings distribution
#' 
## ----plot_ratings, echo=FALSE, message=FALSE, cache=TRUE,fig.align='center', fig.height=3----
##### PLOT RATING 
cols<- c(Mean="darkgreen", Median="red")
ggplot(stats_rating,aes(x = rating, y = countRating)) +
    geom_bar(stat="identity",colour="darkblue", fill="#86A8D1") +
    geom_vline(aes(xintercept=median(edx$rating), color="Median"), size=2) +
    geom_vline(aes(xintercept=mean(edx$rating), color="Mean"), size=2) +
    scale_colour_manual(name="Bars",values=cols) +
    labs(title = "Ratings count",
         x = "Rating", y = "Count")

#' 
#' There are **`r length(edx$rating)`** ratings. 
#' 
## ----summary_ratings, echo=FALSE, cache=TRUE-----------------------------
# rating summary
summary(edx$rating)

#' 
#' **`r round(sum(edx$rating%%1==0) * 100 / length(edx$rating), 1)`%** of these ratings are **whole** numbers. There is an unbalance between whole and half rating.
#' 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' ### Users
#' 
## ----plot_users, cache=TRUE, message=FALSE, echo=FALSE, fig.align='center', fig.height=4, fig.width=8----
# Load the gridextra package
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

# plot : User average ratings
p1 <- ggplot(stats_users,aes(x = meanRating)) +
    geom_histogram(binwidth=0.1, colour = "darkblue", fill = "#86A8D1") +
    # geom_bar(stat="identity",colour="darkblue", fill="#86A8D1") +
    geom_vline(aes(xintercept=median(stats_users$meanRating), color="Median"), size=2) +
    geom_vline(aes(xintercept=mean(stats_users$meanRating), color="Mean"), size=2) +
    scale_colour_manual(name="Bars",values=cols) +
    labs(title = "User Average Ratings",
         x = "Average rating", y = "Count") +
    scale_x_continuous(breaks = c(1:5, round(median(stats_users$meanRating), 2)))  +
    theme(legend.position='bottom')

# plot : User number of ratings
p2 <- stats_users %>% 
    ggplot(aes(x = countRating)) +
    geom_histogram(bins=50, colour = "darkblue", fill = "#86A8D1") +
    # geom_bar(stat="identity",colour="darkblue", fill="#86A8D1") +
    geom_vline(aes(xintercept=median(stats_users$countRating), color="Median"), size=2) +
    geom_vline(aes(xintercept=mean(stats_users$countRating), color="Mean"), size=2) +
    scale_colour_manual(name="Bars",values=cols) +
    labs(title = "User Number of Ratings",
         x = "Number of rating", y = "Count") +
    scale_x_log10(breaks = c(10,50,100,250, 500, 1000,5000, 
                             round(median(stats_users$countRating), 0),
                             round(mean(stats_users$countRating), 0))) +
    theme(legend.position='bottom')
# display plots side by side
grid.arrange(p1, p2, ncol=2)

#' 
#' There are **`r dim(stats_users)[1]`** users. 
#' 
#' Number of ratings by users:
## ----summary_users, echo=FALSE, cache=TRUE-------------------------------
# rating summary
summary(stats_users$countRating)

#' The mean number of rating by user is : **`r round(mean(stats_users$countRating), 1)`**.  
#' 25% of the user have less than **`r round(summary(stats_users$countRating)[[2]], 1)`** ratings.  
#' There are some very high value : **`r sum(stats_users$countRating > 1000)`** users rated more than 1000 movies, with a max number of rating of **`r max(stats_users$countRating)`**.
#' 
#' 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' ### Movies
#' 
## ----plot_movies, cache=TRUE, echo=FALSE, fig.align='center', fig.height=4, fig.width=8----
# Load the gridextra package
p1 <- ggplot(stats_movies,aes(x = meanRating)) +
    geom_histogram(binwidth=0.1, colour = "darkblue", fill = "#86A8D1") +
    # geom_bar(stat="identity",colour="darkblue", fill="#86A8D1") +
    geom_vline(aes(xintercept=median(stats_movies$meanRating), color="Median"), size=2) +
    geom_vline(aes(xintercept=mean(stats_movies$meanRating), color="Mean"), size=2) +
    scale_colour_manual(name="Bars",values=cols) +
    labs(title = "Movie Average Ratings",
         x = "Average rating", y = "Count") +
    scale_x_continuous(breaks = c(1:5, round(median(stats_movies$meanRating), 2))) +
    theme(legend.position='bottom')


p2 <- stats_movies %>% 
    ggplot(aes(x = countRating)) +
    geom_histogram(bins=50, colour = "darkblue", fill = "#86A8D1") +
    # geom_bar(stat="identity",colour="darkblue", fill="#86A8D1") +
    geom_vline(aes(xintercept=median(stats_movies$countRating), color="Median"), size=2) +
    geom_vline(aes(xintercept=mean(stats_movies$countRating), color="Mean"), size=2) +
    scale_colour_manual(name="Bars",values=cols) +
    labs(title = "Movie Number of Ratings",
         x = "Number of rating", y = "Count") +
    scale_x_log10(breaks = c(1,10,50, 100,1000,10000, 
                             round(median(stats_movies$countRating), 0),
                             round(mean(stats_movies$countRating), 0))) +
    theme(legend.position='bottom')
# display plots side by side
grid.arrange(p1, p2, ncol=2)

#' 
#' There are **`r dim(stats_movies)[1]`** movies 
#' 
#' Number of ratings by movies:
## ----summary_movies, echo=FALSE, cache=TRUE------------------------------
# rating summary
summary(stats_movies$countRating)

#' The mean number of rating by movie is : **`r round(mean(stats_movies$countRating), 1)`**.  
#' 25% of the movies have less than **`r round(summary(stats_movies$countRating)[[2]], 1)`** ratings. 25% of the movies have more than **`r round(summary(stats_movies$countRating)[[5]], 1)`** ratings.  
#' There are some very high value : **`r sum(stats_movies$countRating > 10000)`** movies have more than 10000 movies, with a max number of rating of **`r max(stats_movies$countRating)`**.
#' 
## ----plot_movies_meanvsnb, cache=TRUE, echo=FALSE, fig.align='center', fig.height=4, fig.width=8----
# Movies count of Rating vs mean of Ratings
# limit to the 90th quantile
stats_movies %>% filter(countRating <= quantile(stats_movies$countRating, 0.90)) %>%
    ggplot(aes(x= countRating, y=meanRating)) +
    geom_point(colour = "darkblue",  alpha = 0.1) +
    geom_smooth(colour = "red", method="lm") +
    labs(title = "Movies count of Rating vs mean of Ratings",
         x = "rating Count", y = "Mean Count")

#' 
#' Movies with a high number of ratings seem to have higher means and less variabilities.  
#' 
#'

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' ### Genres
#' 
## ----plot_genres_movies, cache=TRUE, echo=FALSE, fig.align='center', fig.height=4----
stats_genres %>% ggplot(aes(x = genres, y=countMovies)) +
    geom_bar(stat="identity", colour = "darkblue", fill = "#86A8D1") +
    theme(axis.text.x = element_text(angle = 90, vjust=0.25, hjust = 1)) +
    labs(title = "Genre - Count distinct movies",
         x = "Genres", y = "Count distinct movies")

#' 
#' 'Drama' and 'Comedy' are the most represented genres.
#' 
## ----plot_genres, cache=TRUE, echo=FALSE, fig.align='center', fig.height=4, fig.width=8----
p1 <- stats_genres %>% ggplot(aes(x = genres, y=meanRating)) +
    geom_bar(stat="identity", colour = "darkblue", fill = "#86A8D1") +
    theme(axis.text.x = element_text(angle = 90, vjust=0.25, hjust = 1)) +
    labs(title = "Genre Average Ratings",
         x = "Genres", y = "Average rating")

p2 <- stats_genres %>% ggplot(aes(x = genres, y=countRating)) +
    geom_bar(stat="identity", colour = "darkblue", fill = "#86A8D1") +
    theme(axis.text.x = element_text(angle = 90, vjust=0.25, hjust = 1)) +
    labs(title = "Genre - Count Ratings",
         x = "Genres", y = "Count rating")

# display plots side by side
grid.arrange(p1, p2, ncol=2)

#' 
#' There is a genre effect.
#' 




################################################################################
############################# Data analysis ####################################
################################################################################
#' 
#' # Data analysis
#' 
#' Even if it was out of this course scope, I found intereting to test 
#' Azure ML studio (with R integration).
#' 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' ## Azure ML - Matchbox recommender
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' 
#' The Azure ML section has been removed as it contains mainly screenshots
#' 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' ## Regularized Movie + User Effect Model
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' 
#' The 3rd solution tested in this report is the `Regularized Movie + User Effect Model`.
#' 
#' #### Finding lambdas with 5-folds CV 
#' 
#' We will compute 2 different lambdas as the penalties may not be the same between the movie and user effects :  
#' - li : for the movie effect  
#' - lu : for the user effect  
#'   
#' To pick the best values of these 2 lambdas, we will do a **5-folds cross validation**. We will split `edx` is 5 folds, then take 4 folds as train set and 1 fold as test set and process every combination. We will then take the values having the better RMSE result average accross the 5 folds.  
#'   
#' To avoid the cases where the test set contains a movie or a user not present in the train set, we will use the same code than the first edx / validation creation (using a temp dataset first).  
#' 
## ----model3_10f, cache=TRUE, echo=TRUE, message=FALSE,warning=FALSE------
# create a dataframe to store ecach result
df.result <- data.frame( "k" = integer(0), 
                         "li" = numeric(0),
                         "lu" = numeric(0),
                         "RMSE" = numeric(0))


# we are doing k-folds cross validation
k<-5

# create the k folds with the CARET package
set.seed(1)
folds <- createFolds(edx$rating, k=k, returnTrain = FALSE)

# for the report compute only 3 to 7
lambdasI <- seq(3, 7, 0.25)
lambdasU <- seq(3, 7, 0.25)

# for each of the 10 folds:
for (i in 1:k){
    
    # print(paste("Starting fold:" , i))
    
    # separate the train / test set using the fold #i
    train_set <- edx[-folds[[i]],]
    temp <- edx[folds[[i]],]
    
    # Make sure userId and movieId in the test set are also in train set
    test_set <- temp %>% 
        semi_join(train_set, by = "movieId") %>%
        semi_join(train_set, by = "userId")
    
    # Add rows removed from test_set set back into train_set set
    removed <- anti_join(temp, test_set)
    train_set <- rbind(train_set, removed)
    rm(removed, temp)
    
    
    print(paste("Computing rmse for fold:" , i, dim(train_set)[1], dim(test_set)[1]))
    
    mu <- mean(train_set$rating)
    
    b_i <- train_set %>% 
        group_by(movieId) %>%
        summarize(sum_b_i = sum(rating - mu), n=n())
    
    for(li in lambdasI){
        
        b_i$b_i <- b_i$sum_b_i/(b_i$n+li)
        
        b_u <- train_set %>% 
            left_join(b_i, by="movieId") %>%
            group_by(userId) %>%
            summarize(sum_b_u = sum(rating - b_i - mu), n=n())
        
        for(lu in lambdasU){
        
            b_u$b_u <- b_u$sum_b_u / (b_u$n+lu)
            
            predicted_ratings <- 
                test_set %>% 
                left_join(b_i, by = "movieId") %>%
                left_join(b_u, by = "userId") %>%
                mutate(pred = mu + b_i + b_u) %>%
                .$pred
            
            myrmse <- RMSE(predicted_ratings, test_set$rating, na.rm = T)
            
            # print(paste(li, lu, myrmse))
            df.result[nrow(df.result) + 1,] = list(k=i, li=li, lu=lu, RMSE=myrmse)
        }
    }

}

#' 
#' 
## ----model3_best, cache=TRUE, echo=FALSE---------------------------------
df.result <- df.result %>% group_by(li, lu) %>%
    summarize(minRMSE=min(RMSE), meanRMSE=mean(RMSE), maxRMSE=max(RMSE))
df.best <- df.result[df.result$meanRMSE==min(df.result$meanRMSE),]
df.best %>% knitr::kable()

li <- df.best$li
lu <- df.best$lu

df.result %>% filter(li>=df.best$li-0.25 & li <=df.best$li+0.25) %>%
    ggplot(aes(1:51, y=meanRMSE)) +
    geom_point(color="red") +
    geom_errorbar(aes(min=minRMSE, max=maxRMSE))

#' Mean, Min and Max of 3 li and theirs lu over the 5-folds.
#'  
#' 
#' #### Compute the full model values (mu, b_i, b_u) with the best li and lu lambdas 
#' 
#' Computing the model with li = **`r li`** and lu = **`r lu`**.  
#'   
## ----model3_computefullmodel, cache=TRUE, echo=TRUE----------------------
mu <- mean(edx$rating)

b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+li))

b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lu))



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' ## Regularized Movie + User Effect + Genre effect Model
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' 
#' The last solution tested in this report is the `Regularized Movie + User Effect + Genre Effect Model`.  
#'   
#' As the computation of the optimal lambdas is very slow on my laptop (+5 hours), we will use the previously calculated lambdas (`lu` and `li`) and just add a genre effect to the result (i.e. as if we start from the residuals of the last models).  
#' 
#' The genre effect is given by the following formula:  
#' $Y_{u,i} = \mu + b_i + b_u + \sum{k=1}^K x_{u,i} \beta_k + \varepsilon_{u,i}$, with $x^k_{u,i} = 1$ if $g_{u,i}$ is genre $k$.
#'   
#' To simplify the model, we will group the movies using a **genre clustering**. A movie is now part on an unique cluster group (and not to multiple genres). The formula becomes:   
#' $Y_{u,i} = \mu + b_i + b_u + b_{u,g} + \varepsilon_{u,i}$
#' 
#' For the penalty, we will use the same than the user one.
#' 
#' #### Genres cluster
#' 
## ----cluster_colsums, cache=TRUE, echo=TRUE------------------------------
colSums(movieGenres[2:21])

#' 
#' As there is only one record of the "(no genres listed)", we remove this value.  
#' We then compute the euclidean distance matrix and build out a Hierarchical Cluster.
#' 
## ----cluster_dist, cache=TRUE, echo=TRUE---------------------------------
movieGenres$`(no genres listed)` <- NULL

# compute the distance
# no need to normalize the values are they have the same scale
distances <- dist(movieGenres[2:20], method="euclidean")
# Hierarchical Cluster
clusterMovies <- hclust(distances, method="ward.D")

#' 
#' We split in 10 groups.  
#' 
## ----cluster_cutree, cache=TRUE, echo=TRUE-------------------------------
plot(clusterMovies, labels=FALSE)
clusterGroups <- cutree(clusterMovies, k = 10)
rect.hclust(clusterMovies, k=10, border="red")

#' 
#' Here is a view of the mean of each genres in each cluster:  
#' 
## ----cluster_viewmean, cache=TRUE, echo=TRUE-----------------------------
view_cluster_means <- sapply(1:10, function(x){round(colMeans(movieGenres[clusterGroups==x,2:20]),2)})

view_cluster_means #%>% knitr::kable()

#' 
#' 
#' We save the pair : movieId / cluster.  
#' Than we add the `gcluster` (genre cluster) to the `edx` and `validation` dataset. (this step will save us a JOIN each time).    
#' 
## ----cluster_addmovieId, cache=TRUE, echo=TRUE---------------------------
movieGenres$gcluster <- clusterGroups
movieGenres <- movieGenres %>% select(movieId, gcluster)
# clean up
rm(distances, clusterMovies, view_cluster_means, clusterGroups)
# add the cluster group to EDX and validation dataset
edx <- edx %>% 
    inner_join(movieGenres, by = "movieId")
edx$genres <- NULL
head(edx)
validation <- validation %>% 
    inner_join(movieGenres, by = "movieId")
head(validation)

#' 
#' #### Genres effect
#' 
#' We can now compute the genres effect on the `edx`dataset.  
#' As stated in the introduction of this section, we will use the residuals of the previous model (we use li and lu values of the model 3 and do not recompute them) and we will use 5-folds cross validation to compute the penalization lambda of the genre (lg).  
#' 
## ----model4_computebestlg, cache=TRUE, include=FALSE, MESSAGE=FALSE, WARNING=FALSE----
# to save the results
df.result <- data.frame( "k" = integer(0), 
                         "lg" = numeric(0),
                         "RMSE" = numeric(0))

# we will test lambdas from 0 to 20
lambdasG <- seq(0, 20, 0.25)

# for each of the 5 folds:
for (i in 1:k){
    
    # print(paste("Starting fold:" , i))
    
    # separate the train / test set using the fold #i
    train_set <- edx[-folds[[i]],]
    temp <- edx[folds[[i]],]
    
    # Make sure userId and movieId in the test set are also in train set
    test_set <- temp %>% 
        semi_join(train_set, by = "movieId") %>%
        semi_join(train_set, by = "userId")
    
    # Add rows removed from test_set set back into train_set set
    removed <- anti_join(temp, test_set)
    train_set <- rbind(train_set, removed)
    rm(removed, temp)
    
    
    #print(paste("Computing rmse for fold:" , i, dim(train_set)[1], dim(test_set)[1]))

    mu <- mean(train_set$rating)
    
    b_i <- train_set %>% 
        group_by(movieId) %>%
        summarize(b_i = sum(rating - mu)/(n()+li))
    
    b_u <- train_set %>% 
        left_join(b_i, by="movieId") %>%
        group_by(userId) %>%
        summarize(b_u = sum(rating - b_i - mu)/(n()+lu))
    
        
    b_g <- train_set %>% 
        left_join(b_i, by="movieId") %>%
        left_join(b_u, by = "userId") %>%
        group_by(userId, gcluster) %>%
        summarize(sum_b_g = sum(rating - b_i -b_u - mu), n=n())
    
    for(lg in lambdasG){
        
        b_g$b_g <- b_g$sum_b_g / (b_g$n+lg)
        
        predicted_ratings <- 
                test_set %>% 
                left_join(b_i, by = "movieId") %>%
                left_join(b_u, by = "userId") %>%
                left_join(b_g, by = c("userId","gcluster")) %>%
                mutate(b_g = replace_na(b_g, 0)) %>%
                mutate(pred = mu + b_i + b_u + b_g) %>%
                .$pred
            
        myrmse <- RMSE(predicted_ratings, test_set$rating, na.rm = T)
            
        #print(paste(i, lg, myrmse))
        df.result[nrow(df.result) + 1,] = list(k=i, lg=lg, RMSE=myrmse)
    }
    
}

#' 
#' The best value for `lg` is:  
#' 
## ----model4_lg, cache=TRUE, echo=FALSE-----------------------------------
df.resultmean <- df.result %>% group_by(lg) %>% 
    summarise(minRMSE=min(RMSE), meanRMSE=mean(RMSE), maxRMSE=max(RMSE))
df.model4 <- df.resultmean[df.resultmean$meanRMSE==min(df.resultmean$meanRMSE),]
lg <- df.model4$lg

df.model4 %>% knitr::kable()


#' 
## ----model4_plot, cache=TRUE, message=FALSE, echo=FALSE, fig.align='center', fig.height=3----
# display the 5 folds RMSE values by lg
df.result %>% ggplot(aes(lg, RMSE, group=factor(k), color=factor(k))) +
    geom_point()  +
    labs(title = "RMSE of each folds by lg",
         x = "lg", y = "RMSE test_set") +
    geom_vline(xintercept=lg, color='red')

#' 
#' #### Compute the full model values (mu, b_i, b_u, b_g)
#' 
#' We can now compute the full model on the edx dataset:  
#' 
## ----model4_computemodel, cache=TRUE, echo=TRUE--------------------------

# computed value from cross validation
# li <- 4.5
# lu <- 5
# lg <- 16

print(paste('li=',li, 'lu=', lu, 'lg=' , lg))

mu <- mean(edx$rating)

b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+li))

b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lu))

b_g <- edx %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(userId, gcluster) %>%
    summarize(b_g = sum(rating - b_i -b_u - mu)/(n()+lg))

#' 
#' 
#' 

################################################################################
################################Results#########################################
################################################################################
#' # Results
#' 
#' We can now try the finalized models on our validation dataset.  
#' 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Azure ML
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## ----azureml_results, cache=TRUE, echo=FALSE-----------------------------
rmse_results <- dplyr::bind_rows(rmse_results,
                          data_frame(method="Azure ML Matchbox - ratings*2",  
                                     RMSE = 1.015422))
rmse_results <- dplyr::bind_rows(rmse_results,
                          data_frame(method="Azure ML Matchbox - ceiling",  
                                     RMSE = 0.939061))
rmse_results %>% knitr::kable()



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' ## Regularized Movie + User Effect Model (model 3)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' 
#'     
## ----model3_validation, cache=TRUE, echo=TRUE----------------------------
predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u ) %>%
    .$pred
              
rmse_model3 <- RMSE(predicted_ratings, validation$rating)

rmse_results <- dplyr::bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = rmse_model3))

rmse_model3



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' ## Regularized Movie + User Effect + Genre effect Model (model 4)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' 
#' For the genre effect, we just need to ensure that if the user / genrecluster does not exist, the value of the average b_g is 0.  
#' 
## ----model4_validation, cache=TRUE, echo=TRUE----------------------------
predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = c("userId","gcluster")) %>%
    mutate(b_g = replace_na(b_g, 0)) %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred

rmse_model4 <- RMSE(predicted_ratings, validation$rating)

rmse_results <- dplyr::bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect + Genre effect Model", 
                                     RMSE = rmse_model4))
rmse_model4

#' 
#' 





################################################################################
################################################################################
################################################################################
#' # Conclusion
#' The 4 RMSEs for the `validation` dataset are the following:  
## ----models_conclusion, cache=TRUE, echo=FALSE---------------------------
rmse_results %>% knitr::kable()

#' 
#' As expected from the average of the cross validations, the model with the 
#' best result is the 'Regularized Movie + User Effect + Genre effect Model' 
#' with an RMSE of **`r round(rmse_model4, 4)`**. This value is near the ones 
#' computed using 5-folds cross validation on the `edx` dataset: 
#' average= `r round(df.model4$meanRMSE, 4)` 
#' (min= `r round(df.model4$minRMSE, 4)`, max= `r round(df.model4$maxRMSE, 4)`).  
#' 
#' I wish i had a better hardware to test more methods (SVM,...).
