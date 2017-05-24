source("scripts/scraper.R")
library(grid)
library(gridExtra)

# number of false positives ~ n^MET + n^(MET - 1) + ... + n^(MET - IT + 1)
MAX_ERROR_THRESHOLD <- 1
# number of terms in series
ITERATION_TIERS <- 3
# where to save results tables
SAVED_DIR <- "saved/"
OUTPUT_DIR <- "output/"
# pretty print settings
FONT_SIZE <- 8
PADDING <- unit(c(2,2), "mm")
RESULTS_PER_PAGE <- 50

# acceptable type I error rate, given number of observations
error.type1 <- function(n_obs, scale) {
  comparisons <- choose(n_obs, 2)
  return(scale*MAX_ERROR_THRESHOLD/comparisons)
}

# -1 <= r1 < r2
#  0 <= insignificant difference
# +1 <= r1 > r2
compare.ratings <- function(r1, r2, cl) {
  contingency <- matrix(c(r1$likes, r1$dislikes,
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
        scores[r1] <- scores[r1] + score/scale
        scores[r2] <- scores[r2] - score/scale
      }
  return(scores)
}

# helper function for generating filepath (with extension)
output.filepath <- function(directory, id_file, extension) {
  basepath <- tools::file_path_sans_ext(basename(id_file))
  return(paste0(directory, basepath, extension))
}

# intermediate cache
save.rdata <- function(output, id_file) {
  filepath <- output.filepath(SAVED_DIR, id_file, ".RData")
  save(output, file=filepath)
}

# save human-readable, final output
save.pdf <- function(output, id_file) {
  num_pages <- ceiling(nrow(output) / RESULTS_PER_PAGE)
  
  filepath <- output.filepath(OUTPUT_DIR, id_file, ".pdf")
  pdf(filepath, width=8.5, height=11)
  theme <- ttheme_default(base_size=FONT_SIZE, padding=PADDING)
  for (page in 1:num_pages) {
    rows <- intersect(1:RESULTS_PER_PAGE+(page-1)*RESULTS_PER_PAGE, 1:nrow(output))
    subtable <- output[rows, , drop=F]
    grid.table(subtable, rows=rownames(subtable), cols=colnames(subtable), theme)
    if (page < num_pages)
      grid.newpage()
  }
  invisible(dev.off()) 
}

# save intermediate and final output
save.rankings <- function(output, id_file) {
  save.rdata(output, id_file)
  save.pdf(output, id_file)
}

# rank | video_id, comments, likes, dislikes, likes/dislikes
generate.rankings <- function(id_file, save_to_file=T) {
  video_ids <- read.csv(id_file, stringsAsFactors=F)
  unordered_ratings <- download.all.ratings(video_ids$id)
  cat("Generating rankings...\n")
  unordered_scores <- round.robin(unordered_ratings)
  scores <- sort(unordered_scores, decreasing=T)
  ordering <- names(scores)
  comment_colname <- colnames(video_ids)[2]
  unordered_rankings <- data.frame(comment_colname=video_ids[,2,drop=F])
  row.names(unordered_rankings) <- video_ids$id 
  rankings <- unordered_rankings[ordering, , drop=F]
  ratings <- unordered_ratings[ordering, , drop=F]
  output <- data.frame(video_id=row.names(rankings), comment_colname=rankings[,1],
                       likes=ratings$likes, dislikes=ratings$dislikes,
                       ratio=ratings$likes/ratings$dislikes,
                       score=scores)
  colnames(output)[2] <- comment_colname
  row.names(output) <- 1:nrow(rankings)
  if (save_to_file)
    save.rankings(output, id_file)
  return(output)
}