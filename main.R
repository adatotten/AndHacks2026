# Install from CRAN
# install.packages("tidyverse")

# to check your installation 
library(tidyverse)

data_jobposts <- read_csv("OnlineJobPostings/data job posts.csv")
industries <- read_csv("LinkedInJobPostings20232024/mappings/industries.csv")
View(data_jobposts)
View(industries)
 
jobposts.clean <- data_jobposts %>% select(-c(Duration, StartDate, Audience, Eligibility, Term, AnnouncementCode, Company, IT, Month, Attach, AboutC, Notes, Deadline, OpeningDate, ApplicationP, Salary, RequiredQual, JobRequirment))

View(jobposts.clean)