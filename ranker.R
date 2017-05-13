source("scraper.R")

MAX_ERROR_THRESHOLD <- 1
ITERATION_TIERS <- 3  # TODO: implement

# acceptable type I error rate, given number of observations
error.type1 <- function(n_obs) {
  comparisons <- choose(n_obs, 2)
  return(MAX_ERROR_THRESHOLD/comparisons)
}

# -1 <= r1 < r2
#  0 <= insignificant difference
#  1 <= r1 > r2
compare.ratings <- function(r1, r2, cl) {
  contingency = matrix(c(r1$likes, r1$dislikes,
                         r2$likes, r2$dislikes),
                       byrow=F, nrow=2)
  odds_ratio <- fisher.test(contingency, conf.level=1-cl)$conf.int
  if (odds_ratio[1] > 1)
    return(1)
  if (odds_ratio[2] < 1)
    return(-1)
  return(0);
}

# score = wins - losses
round.robin <- function(ratings) {
  nr <- nrow(ratings)
  scores <- integer(nr)
  names(scores) <- rownames(ratings)
  for (r1 in 1:(nr-1))
    for (r2 in (r1+1):nr) {
      s <- compare.ratings(ratings[r1,], ratings[r2,], error.type1(nr))
      scores[r1] = scores[r1] + s
      scores[r2] = scores[r2] - s
    }
  return(scores)
}

# rank | video_id, performer, likes, dislikes, likes/dislikes
generate.rankings <- function(id_file) {
  video_ids <- read.csv(id_file, stringsAsFactors=F)
  print(video_ids)
  ratings <- download.ratings(video_ids$id)
  print(ratings)
  scores <- round.robin(ratings)
  print(scores)
  rankings <- data.frame(performer=video_ids$performer)
  row.names(rankings) <- video_ids$id
  rankings <- rankings[names(sort(scores, decreasing=T)), , drop=F]
  ratings <- ratings[names(sort(scores, decreasing=T)), , drop=F]
  rankings <- data.frame(video_id=row.names(rankings), performer=rankings$performer,
                         likes=ratings$likes, dislikes=ratings$dislikes,
                         ratio=ratings$likes/ratings$dislikes)
  return(rankings)
}