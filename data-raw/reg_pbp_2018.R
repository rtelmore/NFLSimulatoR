if (!file.exists("data-raw/reg_pbp_2018.csv")) {
  download.file(
    "https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2018.csv",
    "data-raw/reg_pbp_2018.csv")
}

raw <- read.csv("data-raw/reg_pbp_2018.csv",
                stringsAsFactors = FALSE)
write.csv(raw, "data-raw/reg_pbp_2018.csv")
save(raw, file = "data/reg_pbp_2018.rda", version = 3)
