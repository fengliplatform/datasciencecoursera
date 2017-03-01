#
# R coding standard
# 1. text file
# 2. indent your code
# 3.limit width to 80 columns
# 4. limit length of one function


# R markdown --knitr--> standard Markdown
# R markdown --slidify --> slides deck

# rmarkdown in RStudio

# read.table(na.string="NA")

setwd("~/course/ds_coursera/course5/project1")
df <- read.csv("activity.csv", header=TRUE, na.strings="NA")

head(df)

steps.by.day <- aggregate(steps ~ date, data=df, FUN=sum)
str(steps.by.day)
hist(steps.by.day$steps)

#steps.by.day$date <- as.Date(as.character(steps.by.day$date), "%Y-%m-%d")
#with(steps.by.day, plot(date, steps))
#library(ggplot2)
#ggplot(steps.by.day, aes(date, steps)) + geom_bar(stat = "identity")


#2. mean and median number of steps of each day
mean(steps.by.day$steps)
median(steps.by.day$steps)

###########
dat <- read.table(text = "id    taxa        length  width
101   collembola  2.1     0.9
102   mite        0.9     0.7
103   mite        1.1     0.8
104   collembola  NA      NA
105   collembola  1.5     0.5
106   mite        NA      NA", header=TRUE)

dat

library(dplyr)

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))


dat %>%
    group_by(taxa) %>%
    mutate(
        length = impute.mean(length),
        width = impute.mean(width)  
  )

library(dplyr)
mtcars %>% mutate(new_column = mpg + wt)

### mutate + ifelse
section <- c("MATH111", "MATH111", "ENG111")
grade <- c(78, 93, 56)
student <- c("David", "Kristina", "Mycroft")
gradebook <- data.frame(section, grade, student)
gradebook
mutate(gradebook, Pass.Fail = ifelse(grade > 60, "Pass", "Fail"))

mutate(gradebook, letter = ifelse(grade %in% 60:69, "D",
                                  ifelse(grade %in% 70:79, "C",
                                         ifelse(grade %in% 80:89, "B",
                                                ifelse(grade %in% 90:99, "A", "F")))))

mutate(gradebook, department = ifelse(grepl("MATH", section), "Math Department",
                                      ifelse(grepl("ENG", section), "English Department", "Other")))

#section grade  student         department
#1 MATH111    78    David    Math Department
#2 MATH111    93 Kristina    Math Department
#3  ENG111    56  Mycroft English Department

install.packages("mice")
library(mice)
md.pattern(df)

as.POSIXct("5:30", format="%H:%M")


