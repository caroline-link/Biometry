library(tidyverse)
avo <- read_rds("https://tinyurl.com/avonetbirddata")


# filtering using base R and dpylr

parrot_fam1 <- avo[which(avo$Family1 == "Psittacidae"),]

parrot_fam2 <- avo |>
                filter(Family1 == "Psittacidae")


# filtering on multiple columns using &

forest_parrots1 <- avo[which(avo$Family1 == "Psittacidae" & # Base R
                               avo$Habitat == "Forest"),]
forest_parrots2 <- avo |>
                    filter(Family1 == "Psittacidae" & # Combined search
                             Habitat == "Forest")

forest_parrots3 <- avo |>
                      filter(Family1 == "Psittacidae") |> # Sequential search
                      filter(Habitat == "Forest")


# Filter to a single order and single trophic level 
unique(avo$Order1)
unique(avo$Trophic.Level)

forest_parrots4 <- avo |>
                    filter(Order1 == "Trogoniformes" & Trophic.Level == "Omnivore")


# Filter on multiple columns using |
parrots_or_cockatoos <- avo |>
                        filter(Family1 == "Psittacidae" |
                                Family1 == "Cacatuidae")

parrot_or_cockatoos2 <- avo |>
                          filter(Family1 %in% c("Psittacidae", "Cacatuidae"))


# Filtering on multiple columns using & and |
fp_or_wc <- avo |>
            filter((Family1 == "Psittacidae" &
                     Habitat == "Forest") |
                     (Family1 == "Cacatuidae" &
                        Habitat == "Woodland"))

forest_carn <- avo |>
                filter((Family1 == "Accipitridae" |
                        Family1 == "Falconidae") &
                        (Habitat == "Forest") &
                         (Trophic.Level == "Carnivore"))


# sorting using arrange()
avo_by_mass <- avo |>
                arrange(Mass)

avo_by_mass_dec <- avo |>
                    arrange(-Mass)

avo_by_mass_dec2 <- avo |>
                    arrange(desc(Mass))

avo_by_wingspan <- avo |>
                    arrange(-(Wing.Length))


# using distinct() to remove duplicates
avo_no_dupes <- avo |>
                distinct()

avo_fams <- avo |>
            distinct(Family1)

family_trophic_levels <- avo |>
                          distinct(Family1,Trophic.Level)

# Using mutate to create or modify 
avo_w_beak_diff <- avo |>
                    mutate(beak_length_difference =
                             Beak.Length_Culmen - Beak.Length_Nares,
                           .before=Beak.Width)

avo_w_beak_diff2 <- avo_w_beak_diff |> 
                    arrange(-beak_length_difference)

# Select() to keep or remove
fsm <- avo |>
        select(Family1, Species1, Mass)

no_rangesize <- avo |>
                select(!Range.Size)

avo_tax <- avo |>
            select(Species1:Order1)

avo_tax2 <- avo |>
            select(!Primary.Lifestyle:Range.Size)


# Renaming columns with rename() or select()
avo_tax4 <- avo |>
              rename(Species = Species1,
                     Family = Family1,
                     Order = Order1)

avo_tax3 <- avo |>
            select(Species = Species1,
                   Family = Family1,
                   Order = Order1,
                   Mass)


# Relocating columns
avo_relocated <- avo |>
                  relocate(Order1, Family1, Species1, Mass, Wing.Length) # makes these the first 5 columns


# Grouping data
avo_w_family_mass <- avo |>
                      group_by(Family1) |>
                      mutate(fam_mean_mass = mean(Mass),
                              .after = Mass)
family_mass <- avo |>
                 group_by(Family1) |>
                 summarize(mean_mass = mean(Mass))

# Summarize
summary <- avo |> 
            group_by(Trophic.Level) |>
            summarize( n = n (),
                       mean_mass = mean(Mass),
                       median_mass = median(Mass))

trophic_range <- avo |>
                  group_by(Trophic.Level) |>
                  summarise(mean_range = mean(Range.Size, na.rm=TRUE))