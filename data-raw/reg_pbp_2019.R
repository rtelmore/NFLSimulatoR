if (!file.exists("data-raw/reg_pbp_2019.csv")) {
  download.file(
    "https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2019.csv",
    "data-raw/reg_pbp_2019.csv")
}

reg_pbp_2019 <- read.csv("data-raw/reg_pbp_2019.csv", stringsAsFactors = FALSE)

write.csv(reg_pbp_2019, "data-raw/reg_pbp_2019.csv")
save(reg_pbp_2019, file = "data/reg_pbp_2019.rda", compress = "xz", version = 3)
