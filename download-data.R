library(compmus)
library(shiny)
library(flexdashboard)
library(tidyverse)
library(tidymodels)
library(spotifyr)
library(leaflet)
library(leaflet.extras)
library(cowplot)
library(stringr)
library(ggdendro)
library(heatmaply)
library(tidymodels)

Monkey_city <- get_playlist_audio_features("", "6KwA52G2dp9P7MWkJY3JUA")
TD_6 <- get_playlist_audio_features("", "2lpTXCxSbaSvTBgVrtbdqt")
TD_5 <- get_playlist_audio_features("", "6f9bDISBNqZQ9HVP3TIqUB")
Battles_2 <- get_playlist_audio_features("", "0AUei7z6FpCCUCsj9QhTCv")
KR <- get_playlist_audio_features("", "0iZmclc8t4SY5V3YHoXa5F")
PVZ <- get_playlist_audio_features("", "7yzRYGQHgDawyg1cElGITX")
TD_6_new <- get_playlist_audio_features("", "2lpTXCxSbaSvTBgVrtbdqt")
TD_5_new <- get_playlist_audio_features("", "6f9bDISBNqZQ9HVP3TIqUB")

saveRDS(object = Monkey_city,file = "data/Monkey_city-data.RDS")
saveRDS(object = TD_6,file = "data/TD_6-data.RDS")
saveRDS(object = TD_5,file = "data/TD_5-data.RDS")
saveRDS(object = Battles_2,file = "data/Battles_2-data.RDS")
saveRDS(object = KR,file = "data/KR-data.RDS")
saveRDS(object = PVZ,file = "data/PVZ-data.RDS")
saveRDS(object = TD_6_new,file = "data/TD_6_new-data.RDS")
saveRDS(object = TD_5_new,file = "data/TD_6_new-data.RDS")

bmc_theme <-
  get_tidy_audio_analysis("4FhRB48acntSlHqjnY77Po") |>
  compmus_align(bars, segments) |>                     # Change `bars`
  select(bars) |>                                      #   in all three
  unnest(bars) |>                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  )

btdb_theme <-
  get_tidy_audio_analysis("73Hvuy9eDRLP719wOOInuv") |>
  compmus_align(bars, segments) |>                     # Change `bars`
  select(bars) |>                                      #   in all three
  unnest(bars) |>                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  )

btd6_theme <-
  get_tidy_audio_analysis("23sMvQBOkqGBtV6CBRWEUf") |>
  compmus_align(bars, segments) |>                     # Change `bars`
  select(bars) |>                                      #   in all three
  unnest(bars) |>                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  )

btd5_theme <-
  get_tidy_audio_analysis("5cygoAvx90lrSM1Derm39d") |>
  compmus_align(bars, segments) |>                     # Change `bars`
  select(bars) |>                                      #   in all three
  unnest(bars) |>                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  )

saveRDS(object = bmc_theme,file = "data/bmc_theme-data.RDS")
saveRDS(object = btdb_theme,file = "data/btdb_theme-data.RDS")
saveRDS(object = btd6_theme,file = "data/btd6_theme-data.RDS")
saveRDS(object = btd5_theme,file = "data/btd5_theme-data.RDS")


Monkey_city$Game <- "Bloons Monkey City"
TD_6$Game <- "Bloons TD 6"
TD_5$Game <- "Bloons TD 5"
Battles_2$Game <- "Bloons TD Battles 2"
KR$Game <- "Kingdom Rush Frontiers"
PVZ$Game <- "Plants vs Zombies"


awards_5 <-
  ggplot(bind_rows(
    Monkey_city |> mutate(category = "Bloons Monkey City"),
    Battles_2 |> mutate(category = "Bloons TD Battles 2"),
    TD_6 |> mutate(category = "Bloons TD 6"),
    TD_5 |> mutate(category = "Bloons TD 5")
  ),                     # Set up the plot.
  aes(
    x = valence,
    y = energy,
    size = tempo,
    colour = Game
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
  theme_light() + scale_size_continuous(range = c(3, 5)) +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Valence",
    y = "Energy",
    size = "Tempo",
    colour = "Game"
  ) +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  )

awards <-
  
  
  ggplot(  bind_rows(
    Monkey_city |> mutate(category = "Bloons Monkey City"),
  ),                     # Set up the plot.
  aes(
    x = valence,
    y = energy,
    size = tempo,
    colour = loudness
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
  theme_light() + scale_size_continuous(range = c(3, 10)) +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Valence",
    y = "Energy",
    size = "Tempo",
    colour = "Loudness"
  )

awards_2 <-
  
  
  
  
  ggplot(  bind_rows(
    Battles_2 |> mutate(category = "Bloons TD Battles 2"),
  ),                     # Set up the plot.
  aes(
    x = valence,
    y = energy,
    size = tempo,
    colour = loudness
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
  theme_light() + scale_size_continuous(range = c(3, 10)) +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Valence",
    y = "Energy",
    size = "Tempo",
    colour = "Loudness"
  )

awards_3 <-
  
  ggplot(  bind_rows(
    TD_6 |> mutate(category = "Bloons TD 6"),
  )
  
  ,                     # Set up the plot.
  aes(
    x = valence,
    y = energy,
    size = tempo*2,
    colour = loudness
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
  theme_light() + scale_size_continuous(range = c(3, 10)) +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Valence",
    y = "Energy",
    size = "Tempo",
    colour = "Loudness"
  )

awards_4 <-
  
  ggplot(  bind_rows(
    TD_5 |> mutate(category = "Bloons TD 5"),
  )
  ,                     # Set up the plot.
  aes(
    x = valence,
    y = energy,
    size = tempo*2,
    colour = loudness
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
  theme_light() + scale_size_continuous(range = c(3, 10)) +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Valence",
    y = "Energy",
    size = "Tempo",
    colour = "Loudness"
  )

saveRDS(object = awards_5,file = "data/awards_5-data.RDS")
saveRDS(object = awards,file = "data/awards-data.RDS")
saveRDS(object = awards_2,file = "data/awards_2-data.RDS")
saveRDS(object = awards_3,file = "data/awards_3-data.RDS")
saveRDS(object = awards_4,file = "data/awards_4-data.RDS")


bmc_theme_plot <- 
  bmc_theme |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  geom_vline(xintercept = 60, colour = "grey") +
  labs(
    x = "Time (s)", 
    y = NULL, 
    fill = "Magnitude", 
    title = "Title song, Bloons Monkey City"
  ) +
  theme_minimal() +
  scale_fill_viridis_c() 


btdb_theme_plot <- 
  btdb_theme |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  geom_vline(xintercept = 100, colour = "grey") +
  labs(
    x = "Time (s)", 
    y = NULL, 
    fill = "Magnitude", 
    title = "Title song, Bloons TD battles 2"
  ) +
  theme_minimal() +
  scale_fill_viridis_c() 


btd5_theme_plot <- 
  btd5_theme |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  geom_vline(xintercept = 53, colour = "grey") +
  labs(
    x = "Time (s)", 
    y = NULL, 
    fill = "Magnitude", 
    title = "Title song, Bloons TD 5"
  ) +
  theme_minimal() +
  scale_fill_viridis_c() 


btd6_theme_plot <- 
  btd6_theme |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(
    x = "Time (s)", 
    y = NULL, 
    fill = "Magnitude", 
    title = "Title song, Bloons TD 6"
  ) +
  theme_minimal() +
  scale_fill_viridis_c() 

saveRDS(object = bmc_theme_plot,file = "data/bmc_theme_plot-data.RDS")
saveRDS(object = btdb_theme_plot,file = "data/btdb_theme_plot-data.RDS")
saveRDS(object = btd5_theme_plot,file = "data/btd5_theme_plot-data.RDS")
saveRDS(object = btd6_theme_plot,file = "data/btd6_theme_plot-data.RDS")


bmc_theme_side_by_side <-
  bind_rows(
    bmc_theme |>
      compmus_self_similarity(pitches, "aitchison") |>
      mutate(d = d / max(d), type = "Chroma"),
    bmc_theme |>
      compmus_self_similarity(timbre, "euclidean") |>
      mutate(d = d / max(d), type = "Timbre")
  ) |>
  mutate() |>
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  facet_wrap(~type) +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(
    x = "Time (s)", 
    y = "Time (s)", 
    fill = "Magnitude", 
    title = "Title song, Bloons Monkey City"
  )

btdb_theme_side_by_side <-
  bind_rows(
    btdb_theme |>
      compmus_self_similarity(pitches, "aitchison") |>
      mutate(d = d / max(d), type = "Chroma"),
    btdb_theme |>
      compmus_self_similarity(timbre, "euclidean") |>
      mutate(d = d / max(d), type = "Timbre")
  ) |>
  mutate() |>
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  facet_wrap(~type) +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(
    x = "Time (s)", 
    y = "Time (s)", 
    fill = "Magnitude", 
    title = "Title song, Bloons TD battles 2"
  )

btd5_theme_side_by_side <-
  bind_rows(
    btd5_theme |>
      compmus_self_similarity(pitches, "aitchison") |>
      mutate(d = d / max(d), type = "Chroma"),
    btd5_theme |>
      compmus_self_similarity(timbre, "euclidean") |>
      mutate(d = d / max(d), type = "Timbre")
  ) |>
  mutate() |>
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  facet_wrap(~type) +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(
    x = "Time (s)", 
    y = "Time (s)", 
    fill = "Magnitude", 
    title = "Title song, Bloons TD 5"
  )

btd6_theme_side_by_side <-
  bind_rows(
    btd6_theme |>
      compmus_self_similarity(pitches, "aitchison") |>
      mutate(d = d / max(d), type = "Chroma"),
    btd6_theme |>
      compmus_self_similarity(timbre, "euclidean") |>
      mutate(d = d / max(d), type = "Timbre")
  ) |>
  mutate() |>
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  facet_wrap(~type) +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(
    x = "Time (s)", 
    y = "Time (s)", 
    fill = "Magnitude", 
    title = "Title song, Bloons TD 6"
  )

saveRDS(object = bmc_theme_side_by_side,file = "data/bmc_theme_side_by_side-data.RDS")
saveRDS(object = btdb_theme_side_by_side,file = "data/btdb_theme_side_by_side-data.RDS")
saveRDS(object = btd5_theme_side_by_side,file = "data/btd5_theme_side_by_side-data.RDS")
saveRDS(object = btd6_theme_side_by_side,file = "data/btd6_theme_side_by_side-data.RDS")

circshift <- function(v, n) {
  if (n == 0) v else c(tail(v, n), head(v, -n))
}

#      C     C#    D     Eb    E     F     F#    G     Ab    A     Bb    B
major_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    0,    0)
minor_chord <-
  c(   1,    0,    0,    1,    0,    0,    0,    1,    0,    0,    0,    0)
seventh_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    1,    0)

chord_templates <-
  tribble(
    ~name, ~template,
    "Gb:7", circshift(seventh_chord, 6),
    "Gb:maj", circshift(major_chord, 6),
    "Bb:min", circshift(minor_chord, 10),
    "Db:maj", circshift(major_chord, 1),
    "F:min", circshift(minor_chord, 5),
    "Ab:7", circshift(seventh_chord, 8),
    "Ab:maj", circshift(major_chord, 8),
    "C:min", circshift(minor_chord, 0),
    "Eb:7", circshift(seventh_chord, 3),
    "Eb:maj", circshift(major_chord, 3),
    "G:min", circshift(minor_chord, 7),
    "Bb:7", circshift(seventh_chord, 10),
    "Bb:maj", circshift(major_chord, 10),
    "D:min", circshift(minor_chord, 2),
    "F:7", circshift(seventh_chord, 5),
    "F:maj", circshift(major_chord, 5),
    "A:min", circshift(minor_chord, 9),
    "C:7", circshift(seventh_chord, 0),
    "C:maj", circshift(major_chord, 0),
    "E:min", circshift(minor_chord, 4),
    "G:7", circshift(seventh_chord, 7),
    "G:maj", circshift(major_chord, 7),
    "B:min", circshift(minor_chord, 11),
    "D:7", circshift(seventh_chord, 2),
    "D:maj", circshift(major_chord, 2),
    "F#:min", circshift(minor_chord, 6),
    "A:7", circshift(seventh_chord, 9),
    "A:maj", circshift(major_chord, 9),
    "C#:min", circshift(minor_chord, 1),
    "E:7", circshift(seventh_chord, 4),
    "E:maj", circshift(major_chord, 4),
    "G#:min", circshift(minor_chord, 8),
    "B:7", circshift(seventh_chord, 11),
    "B:maj", circshift(major_chord, 11),
    "D#:min", circshift(minor_chord, 3)
  )

btdb_theme_chord_analysis <- btdb_theme |> 
  compmus_match_pitch_template(chord_templates, "euclidean", "manhattan") |>
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(
    x = "Time (s)",
    y = NULL,
    title = "Title song, Bloons TD Battles 2"
  ) +
  theme(axis.text = element_text(size = 5))

bmc_theme_chord_analysis <- bmc_theme |> 
  compmus_match_pitch_template(chord_templates, "euclidean", "manhattan") |>
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(
    x = "Time (s)",
    y = NULL,
    title = "Title song, Bloons Monkey City"
  ) +
  theme(axis.text = element_text(size = 5))

btd5_theme_chord_analysis <- btd5_theme |> 
  compmus_match_pitch_template(chord_templates, "euclidean", "manhattan") |>
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)",
       y = NULL,
       title = "Title song, Bloons TD 5") +
  theme(axis.text = element_text(size = 5))

btd6_theme_chord_analysis <- btd6_theme |> 
  compmus_match_pitch_template(chord_templates, "euclidean", "manhattan") |>
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)",
       y = NULL,
       title = "Title song, Bloons TD 6") +
  theme(axis.text = element_text(size = 5))

saveRDS(object = btdb_theme_chord_analysis,file = "data/btdb_theme_chord_analysis-data.RDS")
saveRDS(object = bmc_theme_chord_analysis,file = "data/bmc_theme_chord_analysis-data.RDS")
saveRDS(object = btd5_theme_chord_analysis,file = "data/btd5_theme_chord_analysis-data.RDS")
saveRDS(object = btd6_theme_chord_analysis,file = "data/btd6_theme_chord_analysis-data.RDS")
circshift <- function(v, n) {
  if (n == 0) v else c(tail(v, n), head(v, -n))
}

#      C     C#    D     Eb    E     F     F#    G     Ab    A     Bb    B
major_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    0,    0)
minor_chord <-
  c(   1,    0,    0,    1,    0,    0,    0,    1,    0,    0,    0,    0)
seventh_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    1,    0)

major_key <-
  c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
minor_key <-
  c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

chord_templates <-
  tribble(
    ~name, ~template,
    "Gb:7", circshift(seventh_chord, 6),
    "Gb:maj", circshift(major_chord, 6),
    "Bb:min", circshift(minor_chord, 10),
    "Db:maj", circshift(major_chord, 1),
    "F:min", circshift(minor_chord, 5),
    "Ab:7", circshift(seventh_chord, 8),
    "Ab:maj", circshift(major_chord, 8),
    "C:min", circshift(minor_chord, 0),
    "Eb:7", circshift(seventh_chord, 3),
    "Eb:maj", circshift(major_chord, 3),
    "G:min", circshift(minor_chord, 7),
    "Bb:7", circshift(seventh_chord, 10),
    "Bb:maj", circshift(major_chord, 10),
    "D:min", circshift(minor_chord, 2),
    "F:7", circshift(seventh_chord, 5),
    "F:maj", circshift(major_chord, 5),
    "A:min", circshift(minor_chord, 9),
    "C:7", circshift(seventh_chord, 0),
    "C:maj", circshift(major_chord, 0),
    "E:min", circshift(minor_chord, 4),
    "G:7", circshift(seventh_chord, 7),
    "G:maj", circshift(major_chord, 7),
    "B:min", circshift(minor_chord, 11),
    "D:7", circshift(seventh_chord, 2),
    "D:maj", circshift(major_chord, 2),
    "F#:min", circshift(minor_chord, 6),
    "A:7", circshift(seventh_chord, 9),
    "A:maj", circshift(major_chord, 9),
    "C#:min", circshift(minor_chord, 1),
    "E:7", circshift(seventh_chord, 4),
    "E:maj", circshift(major_chord, 4),
    "G#:min", circshift(minor_chord, 8),
    "B:7", circshift(seventh_chord, 11),
    "B:maj", circshift(major_chord, 11),
    "D#:min", circshift(minor_chord, 3)
  )

key_templates <-
  tribble(
    ~name, ~template,
    "Gb:maj", circshift(major_key, 6),
    "Bb:min", circshift(minor_key, 10),
    "Db:maj", circshift(major_key, 1),
    "F:min", circshift(minor_key, 5),
    "Ab:maj", circshift(major_key, 8),
    "C:min", circshift(minor_key, 0),
    "Eb:maj", circshift(major_key, 3),
    "G:min", circshift(minor_key, 7),
    "Bb:maj", circshift(major_key, 10),
    "D:min", circshift(minor_key, 2),
    "F:maj", circshift(major_key, 5),
    "A:min", circshift(minor_key, 9),
    "C:maj", circshift(major_key, 0),
    "E:min", circshift(minor_key, 4),
    "G:maj", circshift(major_key, 7),
    "B:min", circshift(minor_key, 11),
    "D:maj", circshift(major_key, 2),
    "F#:min", circshift(minor_key, 6),
    "A:maj", circshift(major_key, 9),
    "C#:min", circshift(minor_key, 1),
    "E:maj", circshift(major_key, 4),
    "G#:min", circshift(minor_key, 8),
    "B:maj", circshift(major_key, 11),
    "D#:min", circshift(minor_key, 3)
  )
btdb_theme_chordogram <- btdb_theme |> 
  compmus_match_pitch_template(
    key_templates,         # Change to chord_templates if descired
    method = "euclidean",  # Try different distance metrics
    norm = "manhattan"     # Try different norms
  ) |>
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)", y = "", title="Title song, Bloons TD Battles 2")+
  theme(axis.text = element_text(size = 5))

bmc_theme_chordogram <- bmc_theme |>
  compmus_match_pitch_template(
    key_templates,         # Change to chord_templates if descired
    method = "euclidean",  # Try different distance metrics
    norm = "manhattan"     # Try different norms
  ) |>
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)", y = "", title="Title song, Bloons Monkey City")+
  theme(axis.text = element_text(size = 5))


btd5_theme_chordogram <- btd5_theme |>
  compmus_match_pitch_template(
    key_templates,         # Change to chord_templates if descired
    method = "euclidean",  # Try different distance metrics
    norm = "manhattan"     # Try different norms
  ) |>
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)", y = "", title="Title song, Bloons TD 5")+
  theme(axis.text = element_text(size = 5))

btd6_theme_chordogram <- btd6_theme |>
  compmus_match_pitch_template(
    key_templates,         # Change to chord_templates if descired
    method = "euclidean",  # Try different distance metrics
    norm = "manhattan"     # Try different norms
  ) |>
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)", y = "", title="Title song, Bloons TD 6")+
  theme(axis.text = element_text(size = 5))

saveRDS(object = btdb_theme_chordogram,file = "data/btdb_theme_chordogram-data.RDS")
saveRDS(object = bmc_theme_chordogram,file = "data/bmc_theme_chordogram-data.RDS")
saveRDS(object = btd5_theme_chordogram,file = "data/btd5_theme_chordogram-data.RDS")
saveRDS(object = btd6_theme_chordogram,file = "data/btd6_theme_chordogram-data.RDS")


bmc_theme_raw <- get_tidy_audio_analysis("4FhRB48acntSlHqjnY77Po")

btdb_theme_raw <- get_tidy_audio_analysis("73Hvuy9eDRLP719wOOInuv")

btd6_theme_raw <- get_tidy_audio_analysis("23sMvQBOkqGBtV6CBRWEUf")

btd5_theme_raw <- get_tidy_audio_analysis("5cygoAvx90lrSM1Derm39d")
# 
# 
# bmc_theme_tempogram <- bmc_theme_raw |> tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) |>
#   ggplot(aes(x = time, y = bpm, fill = power)) +
#   geom_raster() +
#   scale_fill_viridis_c(guide = "none") +
#   labs(x = "Time (s)", y = "Tempo (BPM)", title="Title song, Bloons Monkey City") +
#   theme_classic()
# saveRDS(object = bmc_theme_tempogram,file = "data/bmc_theme_tempogram-data.RDS")
# 
# btdb_theme_tempogram <- btdb_theme_raw |>tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) |>
#   ggplot(aes(x = time, y = bpm, fill = power)) +
#   geom_raster() +
#   scale_fill_viridis_c(guide = "none") +
#   labs(x = "Time (s)", y = "Tempo (BPM)", title="Title song, Bloons TD Battles 2") +
#   theme_classic()
# 
# saveRDS(object = btdb_theme_tempogram,file = "data/btdb_theme_tempogram-data.RDS")
# 
# btd5_theme_tempogram <- btd5_theme_raw |> tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) |>
#   ggplot(aes(x = time, y = bpm, fill = power)) +
#   geom_raster() +
#   scale_fill_viridis_c(guide = "none") +
#   labs(x = "Time (s)", y = "Tempo (BPM)", title="Title song, Bloons TD 5") +
#   theme_classic()
# saveRDS(object = btd5_theme_tempogram,file = "data/btd5_theme_tempogram-data.RDS")
# 
# btd6_theme_tempogram <- btd6_theme_raw |> tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) |>
#   ggplot(aes(x = time, y = bpm, fill = power)) +
#   geom_raster() +
#   scale_fill_viridis_c(guide = "none") +
#   labs(x = "Time (s)", y = "Tempo (BPM)", title="Title song, Bloons TD 6") +
#   theme_classic()
# 
# saveRDS(object = btd6_theme_tempogram,file = "data/btd6_theme_tempogram-data.RDS")


get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit |>
    collect_predictions() |>
    conf_mat(truth = outcome, estimate = .pred_class)
}

get_pr <- function(fit) {
  fit |>
    conf_mat_resampled() |>
    group_by(Prediction) |> mutate(precision = Freq / sum(Freq)) |>
    group_by(Truth) |> mutate(recall = Freq / sum(Freq)) |>
    ungroup() |> filter(Prediction == Truth) |>
    select(class = Prediction, precision, recall)
}

Battles_2$`track.name` <- paste0(Battles_2$`track.name`, "_", seq_len(nrow(Battles_2)))

halloween <-
  Battles_2  |>
  add_audio_analysis() |>
  mutate(
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
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
  # step_range(all_predictors())
  prep(halloween |> mutate(track.name = str_trunc(track.name, 20000))) |>
  juice() |>
  column_to_rownames("track.name")

halloween_dist_1 <- dist(halloween_juice, method = "euclidean")
halloween_dist_1_plot <-
halloween_dist_1 |>
  hclust(method = "single") |> # Try single, average, and complete.
  dendro_data() |>
  ggdendrogram() +
  labs(title="Single linkage")

halloween_dist_2 <- dist(halloween_juice, method = "euclidean")

halloween_dist_2_plot <-
halloween_dist_2 |>
  hclust(method = "average") |> # Try single, average, and complete.
  dendro_data() |>
  ggdendrogram() +
  labs(title="Average linkage")

halloween_dist_3 <- dist(halloween_juice, method = "euclidean")

halloween_dist_3_plot <-
  halloween_dist_3 |>
  hclust(method = "complete") |> # Try single, average, and complete.
  dendro_data() |>
  ggdendrogram() +
  labs(title="Complete linkage")

saveRDS(object = halloween_dist_1_plot,file = "data/halloween_dist_1-data.RDS")
saveRDS(object = halloween_dist_2_plot,file = "data/halloween_dist_2-data.RDS")
saveRDS(object = halloween_dist_3_plot,file = "data/halloween_dist_3-data.RDS")



Tempogram_new_Games <-
  ggplot(bind_rows(
    KR |> mutate(category = "Kingdom Rush Frontiers"),
    PVZ |> mutate(category = "Plants"),
    TD_6 |> mutate(category = "Bloons TD 6"),
    TD_5 |> mutate(category = "Bloons TD 5")
  ),                     # Set up the plot.
  aes(
    x = tempo,
    colour = Game
  )
  ) +
  geom_histogram() +          # Scatter plot.
  facet_wrap(~ category) +    # Separate charts per playlist.
  theme_light() + scale_size_continuous(range = c(3, 5)) +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Tempo",
    y = "Count",
    colour = "Game"
  ) +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  )

saveRDS(object = Tempogram_new_Games,file = "data/Tempogram_new_Games-data.RDS")



