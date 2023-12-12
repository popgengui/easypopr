\name{get.population.parameters}
\alias{get.population.parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.population.parameters(gploidy, g2sex)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gploidy}{
%%     ~~Describe \code{gploidy} here~~
}
  \item{g2sex}{
%%     ~~Describe \code{g2sex} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (gploidy, g2sex) 
{
    gnbpop = NULL
    lv.pop = list()
    v.user.values = prompt.for.values.and.return.user.entries("Number of populations ?", 
        1, "integer", c(1, MAX_NUMBER_POPULATIONS))
    gnbpop = v.user.values[1]
    lv.pop[["number_populations"]] = gnbpop
    if (gnbpop > 1) {
        v.user.values = prompt.for.values.and.return.user.entries("Same number of individuals in each populations ?:y/n", 
            1, "character", c("y", "n"))
        s.unif.pop.size = v.user.values[1]
    }
    else {
        s.unif.pop.size = "y"
    }
    i.unif.pop.size = ifelse(s.unif.pop.size == "y", TRUE.AS.INT, 
        FALSE.AS.INT)
    lv.pop[["same_number_individuals_each_population"]] = i.unif.pop.size
    if (gploidy != 0) {
        if (g2sex == FALSE.AS.INT) {
            if (i.unif.pop.size == FALSE.AS.INT) {
                s.prompt.with.index.holder = paste("enter number of individuals of population", 
                  INDEX.HOLDER.STRING)
                v.user.values = prompt.for.values.and.return.user.entries(s.prompt.with.index.holder, 
                  gnbpop, "integer", c(1, MAX_NUMBER_INDIVIDUALS_IN_POP))
                s.per.pop.size = paste(v.user.values, collapse = ",")
                lv.pop[["per_population_individual_counts"]] = s.per.pop.size
            }
            else {
                v.user.values = prompt.for.values.and.return.user.entries("Number of individuals ?\n", 
                  1, "integer", c(1, MAX_NUMBER_INDIVIDUALS_IN_POP))
                lv.pop[["number_of_individuals"]] = v.user.values[1]
            }
        }
        else {
            if (i.unif.pop.size == FALSE.AS.INT) {
                s.prompt.with.index.holder = paste("enter number of females of population", 
                  INDEX.HOLDER.STRING)
                v.user.values = prompt.for.values.and.return.user.entries(s.prompt.with.index.holder, 
                  gnbpop, "integer", c(1, MAX_NUMBER_INDIVIDUALS_IN_POP))
                s.per.pop.female.count = paste(v.user.values, 
                  collapse = ",")
                lv.pop[["per_population_number_of_females"]] = s.per.pop.female.count
                s.prompt.with.index.holder = paste("enter number of males of population", 
                  INDEX.HOLDER.STRING)
                v.user.values = prompt.for.values.and.return.user.entries(s.prompt.with.index.holder, 
                  gnbpop, "integer", c(1, MAX_NUMBER_INDIVIDUALS_IN_POP))
                s.per.pop.male.count = paste(v.user.values, collapse = ",")
                lv.pop[["per_population_number_of_males"]] = s.per.pop.male.count
            }
            else {
                v.user.values = prompt.for.values.and.return.user.entries("Number of females in each population ?\n", 
                  1, "integer", c(1, MAX_NUMBER_INDIVIDUALS_IN_POP))
                i.females.per.pop = v.user.values[1]
                lv.pop[["number_females_each_population"]] = i.females.per.pop
                v.user.values = prompt.for.values.and.return.user.entries("Number of males in each population ?\n", 
                  1, "integer", c(1, MAX_NUMBER_INDIVIDUALS_IN_POP))
                i.males.per.pop = v.user.values[1]
                lv.pop[["number_males_each_population"]] = i.females.per.pop
            }
        }
    }
    else {
        for (s.indtype in c("queens", "males", "workers")) {
            if (i.unif.pop.size == FALSE.AS.INT) {
                s.prompt.with.index.holder = paste("enter number of", 
                  s.indtype, "of population", INDEX.HOLDER.STRING)
                v.user.values = prompt.for.values.and.return.user.entries(s.prompt.with.index.holder, 
                  gnbpop, "integer", c(1, MAX_NUMBER_INDIVIDUALS_IN_POP))
                s.per.pop.count = paste(v.user.values, collapse = ",")
                s.key = paste("per_population_number_of_", s.indtype, 
                  sep = "")
                lv.pop[[s.key]] = s.per.pop.count
            }
            else {
                s.prompt = paste("Number of ", s.indtype, " in each population?", 
                  sep = "")
                v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
                  1, "integer", c(1, MAX_NUMBER_INDIVIDUALS_IN_POP))
                s.key = paste("number_", s.indtype, "_each_population", 
                  sep = "")
                lv.pop[[s.key]] = v.user.values[1]
            }
        }
    }
    return(lv.pop)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
