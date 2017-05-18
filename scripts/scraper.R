library(rvest)

# base URL
DOMAIN <- "https://www.youtube.com/watch?v="

# download and scrape
download.rating <- function(video_id) {
  url <- paste0(DOMAIN, video_id)
  
  rating <- NULL
  while (is.null(rating) || nrow(rating) == 0) {
    page <- read_html(url)
    buttons <- html_nodes(page, "button")
    
    upvote <- head(which(html_attr(buttons, "title") == "I like this"), 1)
    likes <- as.numeric(gsub(",", "", html_text(buttons[upvote])))
    
    downvote <- head(which(html_attr(buttons, "title") == "I dislike this"), 1)
    dislikes <- as.numeric(gsub(",", "", html_text(buttons[downvote])))
    
    rating <- data.frame(likes, dislikes)
  }

  row.names(rating) <- video_id
  return(rating)
}

# video_ids : vector of strings
download.ratings <- function(video_ids) {
  ratings <- data.frame()
  for (video_id in video_ids) {
    progress <- 100*nrow(ratings) / length(video_ids)
    cat(paste0("\rDownloading... ", round(progress), "% complete"))
    rating <- download.rating(video_id)
    ratings <- rbind(ratings, rating)
  }
  cat("\rDownloading... 100% complete\n")
  
  return(ratings)
}