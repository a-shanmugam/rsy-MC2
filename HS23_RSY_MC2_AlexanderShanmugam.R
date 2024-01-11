compute_profiles <- function(data, binary) {
  # Convert the matrix to a data frame
  df <- as.data.frame(data)

  if (binary) {
    df$profile <- apply(df, 1, function(x) paste(ifelse(x == 1, names(x), ""), collapse = "-"))
  } else {
    df$profile <- apply(df, 1, function(x) paste(ifelse(x >= 1, x, ""), collapse = "-"))
  }

  df$profile <- gsub("--+", "-", df$profile)
  df$profile <- gsub("^-|-$", "", df$profile)

  # Unlist the profile column to get a vector of genre combinations
  name_combinations <- unlist(df$profile)

  # Get the counts of each genre combination
  combination_counts <- table(name_combinations)

  # Convert the table to a data frame
  df_combination_counts <- as.data.frame(combination_counts)
  colnames(df_combination_counts) <- c("Profile", "Count")

  return(df_combination_counts)
}

check_and_transpose <- function(A, B) {
  # Check if the number of columns in mat1 is equal to the number of rows in mat2
  if (ncol(A) != nrow(B)) {
    # If not, transpose mat2
    B <- t(B)
  }

  # Check again if the matrices are compatible for multiplication
  if (ncol(A) != nrow(B)) {
    stop("The matrices are not compatible for multiplication even after transposition.")
  }

  return(B)
}

calc_cos_similarity_twomtrx <- function(A, B) {
  B <- check_and_transpose(A, B)
  S_unnormed <- A %*% B
  S_normed <- rowSums(A^2)^(1 / 2) %*% t(colSums(B^2)^(1 / 2))

  S <- S_unnormed / S_normed
  return(S)
}

check_dimensions <- function(data, rowname, colname, dataname) {
  data(MovieLense)
  n_users <- dim(MovieLenseUser)[1]
  n_genres <- length(genres)
  n_movies <- dim(MovieLenseMeta)[1]

  arr <- c(
    "User" = n_users,
    "Genres" = n_genres,
    "Filme" = n_movies
  )

  print(paste("Stimmt die Anzahl Zeilen (", rowname, ") der ", dataname, " Matrix mit der Anzahl ", rowname, " aus dem MovieLense Dataset? ", dim(data)[1] == arr[rowname]), sep = "")
  print(paste("Stimmt die Anzahl Spalten (", colname, ") der ", dataname, " Matrix mit der Anzahl ", colname, " aus dem MovieLense Dataset? ", dim(data)[2] == arr[colname]), sep = "")
  print(paste("Die Dimensionen der ", dataname, " Matrix sind:", dim(data)[1], "Zeilen (", rowname, ") und", dim(data)[2], "Spalten (", colname, ")"), sep = "")
}

get_topn_recos <- function(matrix, N) {
  # Initialize an empty dataframe
  topN_df <- data.frame(User = integer(), Movie = character(), Similarity = numeric())

  # Get the column names (users)
  users <- seq(1, dim(matrix)[2])

  # Loop over each user
  for (user in users) {
    # Get the top N movies for this user
    topN_movies <- sort(matrix[, user], decreasing = TRUE)[1:N]

    # Create a dataframe for this user
    user_df <- data.frame(User = user, Movie = names(topN_movies), Similarity = topN_movies)

    # Append to the overall dataframe
    topN_df <- rbind(topN_df, user_df)
  }

  return(topN_df)
}

analyze_topn_recos <- function(matrix, list_N) {
  min_values_matrix <- matrix(nrow = 0, ncol = ncol(matrix))

  for (N in list_N) {
    topNSimilarities <- get_topn_recos(matrix, N)
    min_scores_per_user <- aggregate(Similarity ~ User, topNSimilarities, min)
    min_values_matrix <- rbind(min_values_matrix, min_scores_per_user$Similarity)
  }
  rownames(min_values_matrix) <- list_N
  return(min_values_matrix)
}

create_cleveland_plot <- function(topNList, userProfiles, movieProfiles, users) {
  # Loop over each user
  for (user in users) {
    # Subset the user profiles data frame for the current user
    userProfile <- userProfiles[user,]
    # Subset the current top N data frame for the current user
    topN_user <- topNList[topNList$User == user,]

    # Select rows from movieProfiles where the row name is in topNmovies
    topN_movieProfiles <- movieProfiles[rownames(movieProfiles) %in% topN_user$Movie,]

    combined_df <- rbind(userProfile, topN_movieProfiles)

    # Count matches of the movie profile with the user profile
    matches <- apply(combined_df, 1, function(row) { sum(row == userProfile) })

    # Create a dataframe with the movie names and the number of matches
    df <- data.frame(Movie = names(matches), Matches = matches)

    # Order the dataframe by the number of matches
    df <- df[order(df$Matches),]

    # Create the Cleveland plot
    dotchart(df$Matches, labels = df$Movie, xlab = "Number of matches", xlim = c(0, 19), main = paste("Number of genres matching user", user, "'s profile per movie"))
  }
}
