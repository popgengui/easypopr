\name{get.migr.hier.step}
\alias{get.migr.hier.step}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.migr.hier.step(g2sex, gnbpop, b.second.scheme)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g2sex}{
%%     ~~Describe \code{g2sex} here~~
}
  \item{gnbpop}{
%%     ~~Describe \code{gnbpop} here~~
}
  \item{b.second.scheme}{
%%     ~~Describe \code{b.second.scheme} here~~
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
function (g2sex, gnbpop, b.second.scheme) 
{
    lv.params = list()
    gfirstrace = NULL
    gfirstrace2 = NULL
    v.user.values = prompt.for.values.and.return.user.entries("How many populations form the first group of populations", 
        1, "integer", c(1, MAX_NUMBER_POPULATIONS))
    if (!b.second.scheme) {
        gfirstrace = v.user.values[1]
        lv.params[["number_populations_first_population_group"]] = gfirstrace
    }
    else {
        gfirstrace2 = v.user.values[1]
        lv.params[["number_populations_first_population_group_second_scheme"]] = gfirstrace2
    }
    lv.props = get.migr.proportions.hier(g2sex, b.second.scheme)
    lv.params = append(lv.params, lv.props)
    return(lv.params)
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
