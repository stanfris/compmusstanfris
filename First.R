library(tidyverse)
library(spotifyr)
library(ggplot2)
library(dplyr)
PVZ <- get_playlist_audio_features("", "7yzRYGQHgDawyg1cElGITX")
TD_5 <- get_playlist_audio_features("", "6f9bDISBNqZQ9HVP3TIqUB")
rownames(PVZ)
rownames(TD_5)
anyDuplicated(tolower(row.names(PVZ))) > 0
ecm
mutate(
  segments = map2(segments, key, compmus_c_transpose),
  map(segments,
      compmus_summarise, pitches,
      method = "mean", norm = "manhattan"
  ),
  timbre =
    map(
      segments,
      compmus_summarise, timbre,
      method = "mean"
    )
) |>
  mutate(pitches = map(pitches, compmus_normalise, "clr")) |>
  mutate_at(vars(pitches, timbre), map, bind_rows) |>
  unnest(cols = c(pitches, timbre))

halloween_juice <-
  recipe(
    track.name ~
      danceability +
      energy +
      loudness +
      speechiness +
      acousticness +
      instrumentalness +
      liveness +
      valence +
      tempo +
      duration +
      C + `C#|Db` + D + `D#|Eb` +
      E + `F` + `F#|Gb` + G +
      `G#|Ab` + A + `A#|Bb` + B +
      c01 + c02 + c03 + c04 + c05 + c06 +
      c07 + c08 + c09 + c10 + c11 + c12,
    data = halloween
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |> 
  # step_range(all_predictors()) |> 
  prep(halloween |> mutate(track.name = str_trunc(track.name, 20))) |>
  juice() |>
  column_to_rownames("track.name")

halloween_dist <- dist(halloween_juice, method = "euclidean")

halloween_dist |> 
  hclust(method = "single") |> # Try single, average, and complete.
  dendro_data() |>
  ggdendrogram()
install.packages("stringr")

rmarkdown::render("index.Rmd", output_format = "html_document")
install.packages(c("shinylive", "httpuv"))

shinylive::export(appdir = "shiny", destdir = "shinyapp")


ecm |>
  summarise(
    mean_speechiness = mean(speechiness),
    mean_acousticness = mean(acousticness),
    mean_liveness = mean(liveness),
    sd_speechiness = sd(speechiness),
    sd_acousticness = sd(acousticness),
    sd_liveness = sd(liveness),
    median_speechiness = median(speechiness),
    median_acousticness = median(acousticness),
    median_liveness = median(liveness),
    mad_speechiness = mad(speechiness),
    mad_acousticness = mad(acousticness),
    mad_liveness = mad(liveness)
  )

disney_hits  <- get_playlist_audio_features("", "37i9dQZF1DX8C9xQcOrE6T")
disney_hits_track <- get_playlist_tracks("37i9dQZF1DX8C9xQcOrE6T")

bruno <- get_track_audio_features("52xJxFP6TqMuO4Yt0eOkMz")
elsa <- get_track_audio_features("0qcr5FMsEO85NAQjrlDRKo")

elsa |>
  summarise(
    mean_speechiness = mean(speechiness),
    mean_acousticness = mean(acousticness),
    mean_liveness = mean(liveness),
    sd_speechiness = sd(speechiness),
    sd_acousticness = sd(acousticness),
    sd_liveness = sd(liveness),
    median_speechiness = median(speechiness),
    median_acousticness = median(acousticness),
    median_liveness = median(liveness),
    mad_speechiness = mad(speechiness),
    mad_acousticness = mad(acousticness),
    mad_liveness = mad(liveness)
  )

bruno |>
  summarise(
    mean_speechiness = mean(speechiness),
    mean_acousticness = mean(acousticness),
    mean_liveness = mean(liveness),
    sd_speechiness = sd(speechiness),
    sd_acousticness = sd(acousticness),
    sd_liveness = sd(liveness),
    median_speechiness = median(speechiness),
    median_acousticness = median(acousticness),
    median_liveness = median(liveness),
    mad_speechiness = mad(speechiness),
    mad_acousticness = mad(acousticness),
    mad_liveness = mad(liveness)
  )


disney_hits |>
  summarise(
    mean_speechiness = mean(speechiness),
    mean_acousticness = mean(acousticness),
    mean_liveness = mean(liveness),
    sd_speechiness = sd(speechiness),
    sd_acousticness = sd(acousticness),
    sd_liveness = sd(liveness),
    median_speechiness = median(speechiness),
    median_acousticness = median(acousticness),
    median_liveness = median(liveness),
    mad_speechiness = mad(speechiness),
    mad_acousticness = mad(acousticness),
    mad_liveness = mad(liveness)
  )



Monkey_city <- get_playlist_audio_features("", "6KwA52G2dp9P7MWkJY3JUA")
TD_6 <- get_playlist_audio_features("", "2lpTXCxSbaSvTBgVrtbdqt")
TD_5 <- get_playlist_audio_features("", "0AUei7z6FpCCUCsj9QhTCv")
Battles_2 <- get_playlist_audio_features("", "6f9bDISBNqZQ9HVP3TIqUB")

awards <-
  bind_rows(
    Monkey_city |> mutate(category = "Bloons Monkey City"),
  )


awards |>
  ggplot(aes(x = energy)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~category)


awards |>
  ggplot(aes(x = category, y = energy)) +
  geom_boxplot()

awards |>
  ggplot(aes(x = category, y = energy)) +
  geom_violin()


awards |>                    # Start with awards.
  mutate(
    mode = ifelse(mode == 0, "Minor", "Major")
  ) |>
  ggplot(                     # Set up the plot.
    aes(
      x = valence,
      y = energy,
      size = loudness,
      colour = mode
    )
  ) +
  geom_point() +              # Scatter plot.
  geom_rug(linewidth = 0.1) + # Add 'fringes' to show data distribution.
  facet_wrap(~ category) +    # Separate charts per playlist.
  scale_x_continuous(         # Fine-tune the x axis.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),   # Use grid-lines for quadrants only.
    minor_breaks = NULL       # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(         # Fine-tune the y axis in the same way.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),
    minor_breaks = NULL
  ) +
  scale_colour_brewer(        # Use the Color Brewer to choose a palette.
    type = "qual",            # Qualitative set.
    palette = "Paired"        # Name of the palette is 'Paired'.
  ) +
  scale_size_continuous(      # Fine-tune the sizes of each point.
    trans = "exp",            # Use an exp transformation to emphasise loud.
    guide = "none"            # Remove the legend for size.
  ) +
  theme_light() +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Valence",
    y = "Energy",
    colour = "Mode"
  )

