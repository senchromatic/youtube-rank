# setwd("path/to/repository/root/directory/")
source("scripts/ranker.R")

# Ballade No. 1 in G minor, Op. 23 - Frederic Chopin
generate.rankings("video_ids/chopin_ballade_1.csv")

# La Marseillaise - Claude Joseph Rouget de Lisle
generate.rankings("video_ids/la_marseillaise.csv")  

# You Are My Sunshine - Jimmie Davis, Charles Mitchell
generate.rankings("video_ids/you_are_my_sunshine.csv")