# setwd("path/to/repository/root/directory/")
source("scripts/ranker.R")

# disable scientific notation
options(scipen=99)

# Ballade No. 1 in G minor, Op. 23 - Frédéric Chopin
generate.rankings("video_ids/chopin_ballade_1.csv")

# La Marseillaise - Claude Joseph Rouget de Lisle
generate.rankings("video_ids/la_marseillaise.csv")  

# You Are My Sunshine - Jimmie Davis, Charles Mitchell
generate.rankings("video_ids/you_are_my_sunshine.csv")

# ABBA discography (all songs)
generate.rankings("video_ids/abba.csv")

# The Four Seasons - Antonio Lucio Vivaldi
generate.rankings("video_ids/vivaldi_le_quattro_stagioni.csv")