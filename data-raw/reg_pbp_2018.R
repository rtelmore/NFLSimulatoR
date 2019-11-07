if (!file.exists("data-raw/reg_pbp_2018.csv")) {
  download.file(
    "https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2018.csv",
    "data-raw/reg_pbp_2018.csv")
}

reg_pbp_2018 <- read.csv("data-raw/reg_pbp_2018.csv", stringsAsFactors = FALSE)

write.csv(reg_pbp_2018, "data-raw/reg_pbp_2018.csv")
save(reg_pbp_2018, file = "data/reg_pbp_2018.rda", compress = "xz", version = 3)
