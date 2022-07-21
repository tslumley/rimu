\name{rstudiosurvey}
\alias{rstudiosurvey}
\docType{data}
\title{
Subset of RStudio 2019 Community Survey
}
\description{
The `rstudiosurvey` data set contains 1838 rows of responses from the 2019 RStudio Community Survey, where columns are the 51 questions and a column for the timestamp. The variable names are the full questions. Multiple responses are separated by a comma and space. A few of the questions included in the survey are:
}
\usage{data("rstudiosurvey")}
\format{
  A data frame with 1811 observations on the following 52 variables.
  \describe{
    \item{\code{Timestamp}}{a character object}
    \item{\samp{How would you rate your level of experience using R?}}{a character vector}
    \item{\samp{What year did you first start learning R?}}{a numeric vector}
    \item{\samp{How did you learn R? If you used multiple methods, please select the one you used the most.}}{a character vector}
    \item{\samp{Compared with other technical topics you've learned in school and on the job, on a scale of 1 to 5, how difficult has it been for you to learn R?}}{a numeric vector}
    \item{\samp{Roughly how long did it take you to achieve proficiency in R?}}{a character vector}
    \item{\samp{Which statement most closely reflects the primary reason why you learned R?}}{a character vector}
    \item{\samp{What do you think was the biggest obstacle you had to overcome in learning R? The choices below are only suggestions; if we haven't listed your obstacle, please choose "Other" and add your obstacle in the text.}}{a character vector}
    \item{\samp{How often do you use R today, either for professional or personal projects?}}{a character vector}
    \item{\samp{What applications do you use R for most? (check all that apply)}}{a character vector}
    \item{\samp{Please rate how much you enjoy using R on a scale of 1 to 5, where 1 is you don't enjoy it at all, and 5 is that you enjoy it a great deal.}}{a numeric vector}
    \item{\samp{How likely are you to recommend R to a colleague, friend, or family member?}}{a numeric vector}
    \item{\samp{Which tools do you use with your R applications? (please check all that apply)}}{a character vector}
    \item{\samp{Did you use tidyverse packages such as ggplot2 or dplyr to learn R?}}{a character vector}
    \item{\samp{Do you use tidyverse packages when you use R now?}}{a character vector}
    \item{\samp{What do you like best about using R?}}{a character vector}
    \item{\samp{What do you like least about using R?}}{a character vector}
    \item{\samp{When you have problems in R, where do you go for help?}}{a character vector}
    \item{\samp{How do you discover new packages or packages that are unfamiliar to you?}}{a character vector}
    \item{\samp{How do you share the results that you create in R? Check all that apply.}}{a character vector}
    \item{\samp{Looking ahead, how do you expect your use of R to change in 2020?}}{a character vector}
    \item{\samp{To help us ensure that you are not a robot, please enter the number of characters in the word "analysis" in the text box below. Please type your answer as a word; for example if you want 3 to be your answer, type "three".}}{a character vector}
    \item{\samp{Do you currently use R Markdown? Choose the statement that most closely matches your use.}}{a character vector}
    \item{\samp{What applications do you use R Markdown for? Check all that apply.}}{a character vector}
    \item{\samp{Looking forward, how do you expect your use of R Markdown to change in 2020?}}{a character vector}
    \item{\samp{How often do you currently use Shiny? Choose the statement that most closely matches your use.}}{a character vector}
    \item{\samp{Looking forward, how do you expect your use of Shiny to change in 2020?}}{a character vector}
    \item{\samp{Do you currently use Python? Choose the statement that most closely matches your use.}}{a character vector}
    \item{\samp{What applications do you use Python for most? (check all that apply)}}{a character vector}
    \item{\samp{Please rate how much you enjoy using Python on a scale of 1 to 5, where 1 is you don't enjoy it at all, and 5 is that you enjoy it a great deal.}}{a numeric vector}
    \item{\samp{How likely are you to recommend Python to a colleague, friend, or family member?}}{a numeric vector}
    \item{\samp{Looking forward, how do you expect your use of Python to change in 2020?}}{a character vector}
    \item{\samp{What computer tools and/or languages have you used besides R?}}{a character vector}
    \item{\samp{What was the FIRST computer language or tool that you learned?}}{a character vector}
    \item{\samp{What year were you born?}}{a numeric vector}
    \item{\samp{What gender do you identify with?}}{a character vector}
    \item{\samp{I identify my ethnicity as (select all that apply):}}{a character vector}
    \item{\samp{What is the highest degree or level of school you have completed? If currently enrolled, please use the highest degree received.}}{a character vector}
    \item{\samp{In what country do you currently reside?}}{a character vector}
    \item{\samp{What industry do you work or participate in?}}{a character vector}
    \item{\samp{What is your job title, if any?}}{a character vector}
    \item{\samp{Which category best describes the work you do?}}{a character vector}
    \item{\samp{How many people in your organization or work group do you feel that you can ask for help or support when working with R?}}{a numeric vector}
    \item{\samp{Which of the following events have you attended, if any? Check all that apply.}}{a character vector}
    \item{\samp{How did you hear about this survey?}}{a character vector}
  }
}

\source{
\url{https://github.com/rstudio/r-community-survey/tree/master/2019}
}
\examples{
data(rstudiosurvey)
names(rstudiosurvey)[40]
## Other software being used
other_software<- as.mr(rstudiosurvey[[40]])
mtable(other_software)
## only those with at least 20 responses
common<-mr_lump(other_software, n=20)
mtable(common)
## 'None' isn't really another package
common<-mr_drop(common, "None")
mtable(common)

## UpSet plot
plot(common)

## Excel users filled in the survey later 
timestamp<-as.Date(rstudiosurvey[[1]],format="\%m/\%d/\%y")
boxplot(timestamp~I(common \%has\% "Excel"))



## names in order of popularity
t<-mtable(common)
popular<-colnames(t)[order(t,decreasing=TRUE)]
## most popular package for each user
cuml_users <- mr_flatten(common, popular, sort=TRUE)
class(cuml_users)
table(cuml_users)

## two-way tables
## people who also use Stata or Julia are less happy with R than those who don't
names(rstudiosurvey)[18]
happy<-factor(rstudiosurvey[[18]])
mtable(happy, common)
round(prop.table(mtable(happy,common),2),2)

## mr objects can be dataframe columns, or expanded to individual levels
df<-data.frame(timestamp, happy, common)
dim(df)
head(df)
df_raw<-data.frame(timestamp, happy, as.matrix(common))
dim(df_raw)
head(df_raw)




}
\keyword{datasets}
