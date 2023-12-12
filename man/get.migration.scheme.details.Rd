\name{get.migration.scheme.details}
\alias{get.migration.scheme.details}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.migration.scheme.details(g2sex, gnbpop, b.second.scheme)
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
    lv.scheme = list()
    lv.migr.params = NULL
    lv.user.values = prompt.for.values.and.return.user.entries(MIGRATION_SCHEME_PROMPT, 
        1, "integer", c(1, MAX_NUMBER_POPULATIONS))
    gtypemigr = lv.user.values[1]
    lv.scheme[["migration_model"]] = gtypemigr
    if (gtypemigr == 1) {
        lv.migr.params = get.migr.one.dim.step(g2sex, gnbpop, 
            b.second.scheme)
    }
    else if (gtypemigr == 2) {
        lv.migr.params = get.migr.two.dim.step(g2sex, gnbpop, 
            b.second.scheme)
    }
    else if (gtypemigr == 3) {
        lv.migr.params = get.migr.island(g2sex, gnbpop, b.second.scheme)
    }
    else if (gtypemigr == 4) {
        lv.migr.params = get.migr.hier.step(g2sex, gnbpop, b.second.scheme)
    }
    else if (gtypemigr == 5) {
        lv.migr.params = get.migr.hier.island(g2sex, gnbpop, 
            b.second.scheme)
    }
    else if (gtypemigr == 6) {
        lv.migr.params = get.migr.spatial(g2sex, gnbpop, b.second.scheme)
    }
    else {
        s.msg = (paste("error: unknown migration model number, ", 
            gtypemigr))
        stop(s.msg)
    }
    lv.scheme = append(lv.scheme, lv.migr.params)
    return(lv.scheme)
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
