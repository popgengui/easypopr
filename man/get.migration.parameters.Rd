\name{get.migration.parameters}
\alias{get.migration.parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.migration.parameters(g2sex, gnbpop)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g2sex}{
%%     ~~Describe \code{g2sex} here~~
}
  \item{gnbpop}{
%%     ~~Describe \code{gnbpop} here~~
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
function (g2sex, gnbpop) 
{
    lv.migr = list()
    lv.scheme = list()
    gtypemigr = NULL
    if (gnbpop <= 2) {
        lv.migr[["migration_model"]] = 3
        gtypemigr = 3
        if (gnbpop == 1) {
            lv.migr[["proportion_female_migration"]] = 0
            if (g2sex == TRUE.AS.INT) {
                lv.migr[["proportion_male_migration"]] = 0
            }
            return(lv.migr)
        }
    }
    v.user.values = prompt.for.values.and.return.user.entries("Same migration scheme over all simulation? (y/n)", 
        1, "character", c("y", "n"))
    s.unif.migr.scheme = v.user.values[1]
    i.unif.migr.scheme = ifelse(s.unif.migr.scheme == "y", TRUE.AS.INT, 
        FALSE.AS.INT)
    lv.migr[["same_migration_scheme_all_simulation"]] = i.unif.migr.scheme
    if (i.unif.migr.scheme == TRUE.AS.INT) {
        lv.scheme = get.migration.scheme.details(g2sex, gnbpop, 
            FALSE)
    }
    else {
        print("migration model for the first part of the simulation?")
        lv.scheme.one = get.migration.scheme.details(g2sex, gnbpop, 
            FALSE)
        print("migration model for the second part of the simulation?")
        lv.scheme.two = get.migration.scheme.details(g2sex, gnbpop, 
            TRUE)
        lv.scheme = append(lv.scheme, lv.scheme.one)
        lv.scheme = append(lv.scheme, lv.scheme.two)
    }
    lv.migr = append(lv.migr, lv.scheme)
    return(lv.migr)
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
