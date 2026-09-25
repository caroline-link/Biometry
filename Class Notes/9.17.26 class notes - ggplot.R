library(tidyverse)
library(ggridges)

avo <- read_rds("https://tinyurl.com/avonetbirddata")


# Distributions in base R and ggplot
hist(avo$Hand.Wing.Index)

avo |>
    ggplot(mapping = aes( x = Hand.Wing.Index)) +
    geom_histogram()

avo |> 
    ggplot(mapping = aes( x = Hand.Wing.Index)) +
    geom_density()


# geom_point()
avo |> ggplot(mapping = aes( x = Hand.Wing.Index,
                             y = Kipps.Distance)) +
      geom_point()


avo |> ggplot(mapping = aes( x = Beak.Depth,
                             y = Beak.Width)) +
  geom_point() +
  geom_smooth()

avo |> ggplot(mapping = aes( x = Beak.Depth,
                             y = Beak.Width)) +
  geom_line()


# geom_line()
avo|> 
    mutate(rounded_hwi = round(Hand.Wing.Index)) |>
    group_by(rounded_hwi) |>
    summarise(mean_kipps = mean(Kipps.Distance)) |>
    ggplot(mapping = aes( x = rounded_hwi,
                          y = mean_kipps)) +
    geom_line()


# geom_abline()
kipps_v_hwi_model <- avo |>
    lm(formula = Kipps.Distance ~ Hand.Wing.Index)


# geom_smooth(method ="lm")
ggplot( data = avo,
        mapping = aes(x = Hand.Wing.Index,
                      y = Kipps.Distance)) +
  geom_point() +
  geom_smooth(method = "lm")


# geom_bar()
avo |>
  group_by(Order1) |>
  summarise(mean_mass = mean(Mass)) |>
  slice_max(n=5, order_by = mean_mass) |>
  ggplot(mapping = aes(y=mean_mass, x=Order1)) +
  geom_bar(stat= "identity")

avo |>
  filter(Order1 %in% c("Struthionformes",
                       "Cathartiformes",
                       "Gaviiformes",
                       "Sphenisciformes",
                       "Ciconiiformes")) |>
  ggplot(mapping = aes (x=Order1)) +
  geom_bar()


# geom_boxplot()
avo |>
  filter(Order1 %in% c("Struthionformes",
                       "Cathartiformes",
                       "Gaviiformes",
                       "Sphenisciformes",
                       "Ciconiiformes")) |>
  ggplot(mapping = aes (x=Order1,
                        y=Wing.Length)) +
  geom_boxplot()


# geom_violin()
avo |>
  filter(Order1 %in% c("Struthionformes",
                       "Cathartiformes",
                       "Gaviiformes",
                       "Sphenisciformes",
                       "Ciconiiformes")) |>
  ggplot(mapping = aes (x=Order1,
                        y=Wing.Length)) +
  geom_violin()


# can use coord_flip() to flip x and y
avo |>
  group_by(Order1) |>
  summarise(mean_mass = mean(Mass)) |>
  slice_max(n=5, order_by = mean_mass) |>
  ggplot(mapping = aes(y=mean_mass, x=Order1)) +
  geom_bar(stat= "identity")

avo |>
  group_by(Order1) |>
  summarise(mean_mass = mean(Mass)) |>
  slice_max(n=5, order_by = mean_mass) |>
  ggplot(mapping = aes(y=mean_mass, x=Order1)) +
  geom_bar(stat= "identity") +
  coord_flip()


# geo_density_rides()
avo |>
  group_by(Order1) |>
  mutate(mean_mass = mean(Mass)) |>
  filter(Order1 %in% c("Struthionformes",
                       "Cathartiformes",
                       "Gaviiformes",
                       "Sphenisciformes",
                       "Ciconiiformes")) |>
  ggplot(mapping = aes (x=Wing.Length,
                        y= Order1)) +
  geom_density_ridges()


# Categorical x and categorical y , geom_count()
avo |> filter(Family1 %in% c ("Psittacidae", "Cacatuidae")) |>
  ggplot(mapping = aes(x=Family1, y=Habitat)) +
  geom_count()


# geom_tile()
avo |>
  group_by(Family1, Trophic.Level) |>
  mutate(n_species = n(),
         log10_species = n() |> log10()) |>
  filter(Family1 %in% c ("Psittacidae", "Cacatuidae")) |>
  ggplot(mapping = aes(x=Family1, 
                       y=Trophic.Level,
                       fill = log10_species)) +
  geom_tile()

# color and fill
avo |> filter(Family1 %in% c ("Psittacidae", "Cacatuidae")) |>
      group_by(Family1,Habitat) |>
  summarise(mean_mass = mean(Mass)) |>
  slice_max(n=5, order_by = mean_mass) |>
  ggplot(mapping = aes(y=mean_mass,
                       x=Family1, fill = Habitat)) +
  geom_bar(stat = "identity", position = "dodge")

# faceting
avo |> filter(Family1 %in% c ("Psittacidae", "Cacatuidae")) |>
  ggplot(mapping=aes(x=Mass,
                     y=Wing.Length))+
  geom_point()+
  facet_wrap(~Family1)

# facet_grid() allows you to have plots divided into rows/columns
avo |> filter(Family1 %in% c ("Psittacidae", "Cacatuidae")) |>
  ggplot(mapping=aes(x=Mass,
                     y=Wing.Length))+
  geom_point()+
  facet_grid(Family1~Trophic.Niche)

# Labeling with ggplot
avo |>
    ggplot(mapping = aes(x=Hand.Wing.Index,
                         y= Kipps.Distance)) +
    geom_point(alpha=0.1) +
    geom_smooth(method ="lm") +
    labs(title = "Kipp's distances rises with hand-wing index",
         x= "hand-Wing Index",
         y = "Kipp's distances (mm)",
         caption = "Data:Avonet")+
  theme_bw()

# Saving a ggplot figure

my_plot <- avo |>
  ggplot(mapping=aes(x=Mass, y=Wing.Length)) +
  geom_point()

ggsave( filename = "test",
        plot = my_plot,
        width =8,
        height =5,
        dpi=300)