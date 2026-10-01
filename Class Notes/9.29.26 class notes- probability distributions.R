avonet <- read.csv("https://raw.githubusercontent.com/bmaitner/biometry_course/refs/heads/main/data/Avonet/AVONET1_BirdLife.csv")

# 1. Generate histogram of Hand-Wing Index
hist(avonet$Hand.Wing.Index)

# 2. Calculate mean and median Hand-Wing Index
mean(avonet$Hand.Wing.Index)
median(avonet$Hand.Wing.Index)

# 3. Generate histogram of body mass
hist(avonet$Mass)

log_mass <- log(avonet$Mass)
hist(log_mass)

# 4. Calculate mean and median body mass
mean(avonet$Mass)
median(avonet$Mass) # mean is much higher than median, skewed by a few large birds

# 5. Calculate mode
library(DescTools)

DescTools::Mode(avonet$Mass)

# 6. Calculate variance and sd of Mass
var(avonet$Mass)
sd(avonet$Mass)

# 7. Check that variance equals SD^2
var(avonet$Mass) == (sd(avonet$Mass)^2)

# 8. Calculate mean and variance of range.size
mean(avonet$Range.Size, na.rm = TRUE)
var(avonet$Range.Size, na.rm = TRUE)
