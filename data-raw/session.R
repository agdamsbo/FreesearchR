## code to prepare `session` dataset goes here

session <- devtools::session_info()

platform <- c(session$platform,
      "FreesearchR"=paste(gsub("Version: ","",readLines(here::here('DESCRIPTION'))[3]),format(Sys.time(),format = '%y%m%d'),sep='.'))

platform <- data.frame(option=names(platform),value=Reduce(c,platform))

libs <- as.data.frame(session$packages)[names(session$packages) %in% c("package","loadedversion","date","source")]

rownames(libs) <- NULL

session_data <- list(
  platform=platform,
  libs=libs
)

usethis::use_data(session_data,internal = TRUE, overwrite = TRUE)
