source("scraper.R")

# number of false positives ~ n^MET + n^(MET - 1) + ... + n^(MET - IT + 1)
MAX_ERROR_THRESHOLD <- 1
# number of terms in series
ITERATION_TIERS <- 3

# acceptable type I error rate, given number of observations
error.type1 <- function(n_obs, scale) {
  comparisons <- choose(n_obs, 2)
  return(scale*MAX_ERROR_THRESHOLD/comparisons)
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
  for (i in 0:(ITERATION_TIERS-1))
    for (r1 in 1:(nr-1))
      for (r2 in (r1+1):nr) {
        scale <- nr^(MAX_ERROR_THRESHOLD - i)
        score <- compare.ratings(ratings[r1,], ratings[r2,], error.type1(nr, scale))
        scores[r1] = scores[r1] + score/scale
        scores[r2] = scores[r2] - score/scale
      }
  return(scores)
}

# rank | video_id, performer, likes, dislikes, likes/dislikes
generate.rankings <- function(id_file) {
  video_ids <- read.csv(id_file, stringsAsFactors=F)
  unordered_ratings <- download.ratings(video_ids$id)
  unordered_scores <- round.robin(unordered_ratings)
  scores <- sort(unordered_scores, decreasing=T)
  ordering <- names(scores)
  unordered_rankings <- data.frame(performer=video_ids$performer)
  row.names(unordered_rankings) <- video_ids$id 
  rankings <- unordered_rankings[ordering, , drop=F]
  ratings <- unordered_ratings[ordering, , drop=F]
  output <- data.frame(video_id=row.names(rankings), performer=rankings$performer,
                       likes=ratings$likes, dislikes=ratings$dislikes,
                       ratio=ratings$likes/ratings$dislikes,
                       score=scores)
  row.names(output) <- 1:nrow(rankings)
  return(output)
}