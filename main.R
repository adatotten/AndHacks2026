# Install from CRAN
# install.packages("tidyverse")

# to check your installation
library(tidyverse)
options(tidyverse.quiet = TRUE)

data_jobposts <- read_csv("content/data job posts.csv")
posts2023_4 <- read.csv("content/postings.csv")
industries <- read_csv("content/industries.csv")
# View(posts2023_4)
# View(industries)

jobposts.clean <- data_jobposts %>% 
  select(-c(Duration, StartDate, Audience, Eligibility, Term, AnnouncementCode, Company, IT, Month, Attach, AboutC, Notes, Deadline, 
            OpeningDate, ApplicationP, Salary, RequiredQual, JobRequirment))
# View(jobposts.clean)

clean.posts2023 <- posts2023_4 %>% 
  select(-c(max_salary, pay_period, location, company_id, views, med_salary, skills_desc, listed_time, posting_domain, sponsored, work_type,
            currency, compensation_type, normalized_salary, zip_code, fips, min_salary, applies, original_listed_time, remote_allowed,
            application_url, application_type, expiry, closed_time, formatted_experience_level, job_posting_url, formatted_work_type))
# View(clean.posts2023)

joined <- clean.posts2023 %>%
  left_join(industries %>% select(industry_id),
            by = c("job_id" = "industry_id"))
# View(joined)

masc_words <- c("active", "adventurous", "aggressive", "ambitious", "analyitic", "athletic", "autonomous", "boast",
                "challenger", "challenging", "competent", "confident", "couragous", "decide", "decisive", "decision",
                "determination", "dominant", "domination", "force", "forceful", "greedy", "headstrong", "hierarchy",
                "heirarchical", "hostile", "hostility", "impulsive", "independence", "independant", "individual",
                "individualistic", "intellectual", "intellect", "lead", "leader", "leading", "logic", "masculine",
                "objective", "opinion", "opinionated", "outspoken", "persist", "principle", "reckless", "stubborn",
                "superior", "self-confident", "self-confidence", "self-sufficient", "self-sufficience", "self-reliant",
                "self-reliance")

fem_words  <- c("affectionate","child", "childlike", "childish", "childhood", "cheerful", "cheer", "commit", "committed",
                "communal", "compassion", "compassionate", "connect", "connected", "considerate", "cooporative", "cooporate",
                "dependable", "depend", "dependant", "emotional", "emotion", "empathetic", "empathy", "empath", "feminine",
                "flatterable", "gentle", "honest", "interpersonal", "interpersonally", "interdependent", "interdependence", "interdependently",
                "kind", "kinship", "loyal", "loyalty", "modesty", "modest", "nag", "nurture", "nurturing", "pleasant", "pleasantly",
                "polite", "quiet", "quietly", "quietness", "responsive", "responsiveness", "sensitivity", "sensitive", "sensitiveness",
                "submissive", "support", "supportive", "supporting", "sympathetic", "sympathy", "sympath", "tender", "tenderness",
                "together", "togetherness", "trust", "trusting", "understanding", "understand", "warm", "warmness", "whining", "yield", "yielding")

jobposts.clean <- jobposts.clean %>%
  mutate(
    has_masc = str_detect(jobpost, str_c(masc_words, collapse = "|")),
    has_fem  = str_detect(jobpost, str_c(fem_words,  collapse = "|"))
  )

clean.posts2023 <- clean.posts2023 %>%
  mutate(
    has_masc = str_detect(description, str_c(masc_words, collapse = "|")),
    has_fem  = str_detect(description, str_c(fem_words,  collapse = "|"))
  )






masc_count = str_count(jobposts.clean$jobpost, str_c(masc_words, collapse = "|"))
word_count = str_count(jobposts.clean$jobpost, "\\w+")
masc_rate  = masc_count / word_count * 100

fem_count = str_count(jobposts.clean$jobpost, str_c(fem_words, collapse = "|"))
femword_count = str_count(jobposts.clean$jobpost, "\\w+")
fem_rate  = fem_count / femword_count * 100


masc_count2 = str_count(clean.posts2023$description, str_c(masc_words, collapse = "|"))
word_count2 = str_count(clean.posts2023$description, "\\w+")
masc_rate2  = masc_count2 / word_count2 * 100

fem_count2 = str_count(clean.posts2023$description, str_c(fem_words, collapse = "|"))
femword_count2 = str_count(clean.posts2023$description, "\\w+")
fem_rate2  = fem_count2 / femword_count2 * 100


clean.posts2023 <- clean.posts2023 %>% mutate(fem_rate2) %>% mutate(masc_rate2)

jobposts.clean %>% group_by(Year) %>%
  summarize(mu=mean(masc_rate, na.rm = TRUE)) -> plot

jobposts.masc.plot <- ggplot(plot, aes(x = Year, y = mu)) +
  geom_line(color = "orange", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "masculine average over time", x = "Year", y = "masculine word rate") +
  theme_grey()

jobposts.masc.plot


jobposts.clean %>% group_by(Year) %>%
  summarize(mu=mean(fem_rate, na.rm = TRUE)) -> plot2


jobposts.fem.plot <- ggplot(plot2, aes(x = Year, y = mu)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "feminine average over time", x = "Year", y = "feminine word rate") +
  theme_grey()

jobposts.fem.plot

model <- lm(masc_rate ~ Year * industry, data = clean.posts2023)

jobposts.clean %>%
  group_by(industry, Year) %>%
  summarize(mean_masc_rate = mean(masc_rate, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = mean_masc_rate, color = industry)) +
  geom_line() +
  geom_point()

jobposts.clean %>%
  group_by(industry ="finance", Year) %>%
  summarize(Average_Masculine = mean(masc_rate, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = Average_Masculine, color = industry)) +
  geom_line(color = "green") +
  geom_point()

jobposts.clean %>%
  group_by(industry ="healthcare", Year) %>%
  summarize(Average_Masculine = mean(masc_rate, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = Average_Masculine, color = industry)) +
  geom_line() +
  geom_point()

jobposts.clean %>%
  group_by(industry ="tech", Year) %>%
  summarize(Average_Masculine = mean(masc_rate, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = Average_Masculine, color = industry)) +
  geom_line(color = "blue") +
  geom_point()

jobposts.clean %>%
  group_by(industry, Year) %>%
  summarize(mean_fem_rate = mean(fem_rate, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = mean_fem_rate, color = industry)) +
  geom_line() +
  geom_point()


# VIEW AND WRITE EVERYTHING
View(jobposts.clean)
View(clean.posts2023)

write.csv(jobposts.clean, "jobposts.clean.csv", row.names = FALSE)
write.csv(clean.posts2023, "clean.posts2023.csv", row.names = FALSE)







































# counts of words
# jobposts.clean <- jobposts.clean %>%
#   mutate(
#     masc_count = str_count(jobpost, str_c(masc_words, collapse = "|")),
#     fem_count = str_count(jobpost, str_c(fem_words,  collapse = "|"))
#   )
# 
# clean.posts2023 <- clean.posts2023 %>%
#   mutate(
#     masc_count = str_count(clean.posts2023, str_c(masc_words, collapse = "|")),
#     fem_count  = str_count(clean.posts2023, str_c(fem_words,  collapse = "|"))
#   )


# masc_count2 = str_count(jobposts.clean$description, str_c(masc_words, collapse = "|"))
# word_count2 = str_count(jobposts.clean$description, "\\w+")
# masc_rate2  = masc_count2 / word_count2 * 100
# 
# fem_count2 = str_count(jobposts.clean$description, str_c(fem_words, collapse = "|"))
# femword_count2 = str_count(jobposts.clean$description, "\\w+")
# fem_rate2  = fem_count2 / femword_count2 * 100
# 
# jobposts.clean <- jobposts.clean %>% mutate(fem_rate2) %>% mutate(masc_rate2)

# masc_rate2 = NULL
# fem_rate2 = NULL


# clean.posts2023 <- clean.posts2023 %>%
#   mutate(
#     has_masc = str_count(description, str_c(masc_words, collapse = "|")),
#     has_fem  = str_count(description, str_c(fem_words,  collapse = "|"))
#   )
# 
# masc_count2 = str_count(clean.posts2023$description, str_c(masc_words, collapse = "|"))
# word_count2 = str_count(clean.posts2023$description, "\\w+")
# masc_rate2  = masc_count2 / word_count2
# 
# fem_count2 = str_count(clean.posts2023$description, str_c(fem_words, collapse = "|"))
# femword_count2 = str_count(clean.posts2023$description, "\\w+")
# fem_rate2  = fem_count2 / femword_count2
# 
# clean.posts2023 <- clean.posts2023 %>% mutate(fem_rate2) %>% mutate(masc_rate2)















# jobposts.clean <- data_jobposts %>% select(-c(Duration, StartDate, Audience, Eligibility, Term, AnnouncementCode, Company, IT, Month, Attach, AboutC, Notes, Deadline, OpeningDate, ApplicationP, Salary, RequiredQual, JobRequirment))
#
# View(jobposts.clean)
#
# clean.posts2023 <- posts2023_4 %>% select(-c(max_salary, pay_period, location, company_id, views, med_salary, skills_desc, listed_time, posting_domain, sponsored, work_type,
#                                              currency, compensation_type, normalized_salary, zip_code, fips, min_salary, applies, original_listed_time, remote_allowed,
#                                              application_url, application_type, expiry, closed_time, formatted_experience_level, job_posting_url, formatted_work_type))
#
# masc_words <- c("active", "adventurous", "aggressive", "ambitious", "analyitic", "athletic", "autonomous", "boast",
#                 "challenger", "challenging", "competent", "confident", "couragous", "decide", "decisive", "decision",
#                 "determination", "dominant", "domination", "force", "forceful", "greedy", "headstrong", "hierarchy",
#                 "heirarchical", "hostile", "hostility", "impulsive", "independence", "independant", "individual",
#                 "individualistic", "intellectual", "intellect", "lead", "leader", "leading", "logic", "masculine",
#                 "objective", "opinion", "opinionated", "outspoken", "persist", "principle", "reckless", "stubborn",
#                 "superior", "self-confident", "self-confidence", "self-sufficient", "self-sufficience", "self-reliant",
#                 "self-reliance")
#
# fem_words  <- c("affectionate","child", "childlike", "childish", "childhood", "cheerful", "cheer", "commit", "committed",
#                 "communal", "compassion", "compassionate", "connect", "connected", "considerate", "cooporative", "cooporate",
#                 "dependable", "depend", "dependant", "emotional", "emotion", "empathetic", "empathy", "empath", "feminine",
#                 "flatterable", "gentle", "honest", "interpersonal", "interpersonally", "interdependent", "interdependence", "interdependently",
#                 "kind", "kinship", "loyal", "loyalty", "modesty", "modest", "nag", "nurture", "nurturing", "pleasant", "pleasantly",
#                 "polite", "quiet", "quietly", "quietness", "responsive", "responsiveness", "sensitivity", "sensitive", "sensitiveness",
#                 "submissive", "support", "supportive", "supporting", "sympathetic", "sympathy", "sympath", "tender", "tenderness",
#                 "together", "togetherness", "trust", "trusting", "understanding", "understand", "warm", "warmness", "whining", "yield", "yielding")
#
# jobposts.clean <- jobposts.clean %>%
#   mutate(
#     has_masc = str_detect(jobpost, str_c(masc_words, collapse = "|")),
#     has_fem  = str_detect(jobpost, str_c(fem_words,  collapse = "|"))
#   )
#
# clean.posts2023 <- clean.posts2023 %>%
#   mutate(
#     has_masc = str_detect(, str_c(masc_words, collapse = "|")),
#     has_fem  = str_detect(jobpost, str_c(fem_words,  collapse = "|"))
#   )