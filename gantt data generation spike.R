#gantt data generation spike

# generate realistic data for a gantt chart for a large project

if ("tidyr" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyr", repos = "http://cran.ma.imperial.ac.uk/")}
if ("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr", repos = "http://cran.ma.imperial.ac.uk/")}
if ("readr" %in% rownames(installed.packages()) == FALSE) {install.packages("readr", repos = "http://cran.ma.imperial.ac.uk/")}
if ("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2", repos = "http://cran.ma.imperial.ac.uk/")}
if ("forcats" %in% rownames(installed.packages()) == FALSE) {install.packages("forcats", repos = "http://cran.ma.imperial.ac.uk/")}
if ("scales" %in% rownames(installed.packages()) == FALSE) {install.packages("scales", repos = "http://cran.ma.imperial.ac.uk/")}
if ("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr", repos = "http://cran.ma.imperial.ac.uk/")}
if ("openxlsx" %in% rownames(installed.packages()) == FALSE) {install.packages("openxlsx", repos = "http://cran.ma.imperial.ac.uk/")}
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate", repos = "http://cran.ma.imperial.ac.uk/")}

library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(stringr)
library(scales)
library(openxlsx)


# principles 1 and 2 have many milestones, 10 and 11 have relatively fewer
principle_dist <- c(rep(1:11, c(30, 35, 10, 7, 8, 17, 22, 10, 10, 5, 3)))
principle_dist1 <- c(rep(1:11, c(10, 11, 10, 7, 45, 17, 22, 40, 10, 5, 2)))
principle_dist2 <- c(rep(1:11, c(5, 35, 10, 7, 8, 2, 22, 10, 30, 5, 1)))

project_length_days = 365 * 3 # 3 years
project_start_date <- as.Date("2016-01-01")
project_critical_date1 <- as.Date("2016-12-05")
project_critical_date2 <- as.Date("2017-12-01")

current_date <- as.Date("2017-06-12")
milestone_rag_values <- c("Red", "Amber", "Green")
milestone_status_values <- c("Active", "Completed")

weight_values <- c(0.1, 0.3, 0.5, 0.9, 2.7, 5)

df_p <-readr::read_csv("Principle.csv")

# let's generate a baseline of evenly spread milestones
n <- 1000
df <- data_frame(
  PrincipleKey = sample(principle_dist, n, replace = TRUE),
  MilestoneDate = sample(project_start_date + 1:project_length_days, n, replace = TRUE)
)

n <- 500
df_bump1 <- data_frame(
  PrincipleKey = sample(principle_dist1, n, replace = TRUE),
  MilestoneDate = project_critical_date1 + floor(rnorm(n, 0, 20))
)

n <- 600
df_bump2 <- data_frame(
  PrincipleKey = sample(principle_dist2, n, replace = TRUE),
  MilestoneDate = project_critical_date2 + floor(rnorm(n, 0, 30))
)

df_total <- bind_rows(df, df_bump1, df_bump2)

n_total <- nrow(df_total)
df_total <-
  df_total %>%
  mutate(
    MilestoneKey = 1:n_total,
    MilestoneStatus = ifelse(MilestoneDate > current_date + rnorm(n_total, 0, 120), "Active", "Completed"),
    MilestoneStatus = ifelse(MilestoneDate < current_date & MilestoneStatus == "Active", "Overdue", MilestoneStatus),
    MilestoneRAG = sample(milestone_rag_values, n_total, replace = TRUE),
    Weight = sample(weight_values, n_total, replace = TRUE)
  )

df_final <- df_total %>%
  inner_join(df_p) %>%
  mutate(PrincipleName = fct_reorder(PrincipleName, -PrincipleKey))

save(df_final, file = "Project Status.rData")
write_csv(df_final, "Project Status.csv")

ggplot(df_final, aes(x =  MilestoneDate, y = PrincipleName, col = MilestoneStatus, 
                fill = MilestoneStatus)) +
  geom_jitter(aes(alpha = 0.4, size = Weight),  width = 0.3, height = 0.3, shape = 21) +
  scale_x_date("Date", date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_discrete("Principle") +
  scale_fill_manual(values = c("green","royalblue2", "red"), labels = c("Active","Completed", "Overdue")) +
  scale_size(guide = FALSE) +
  scale_alpha(guide = FALSE) +
  geom_vline(xintercept = as.numeric(current_date), linetype = "dotted", col = "gray22") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggtitle(label = "Milestones  By Principle",
          subtitle = "The more important milestones have a larger size")

ggsave("Gantt style Project Overview Wider.png", width = 15, height = 7,device = "png")
