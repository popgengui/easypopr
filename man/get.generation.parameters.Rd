\name{get.generation.parameters}
\alias{get.generation.parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.generation.parameters(i.unif.migr.scheme)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i.unif.migr.scheme}{
%%     ~~Describe \code{i.unif.migr.scheme} here~~
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
function (i.unif.migr.scheme) 
{
    lv.generations = list()
    v.user.values = NULL
    s.prompt = "Number of generations ?"
    v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
        1, "integer", c(1, MAX_NUMBER_GENERATIONS))
    i.num.gens = v.user.values[1]
    lv.generations[["number_of_generations"]] = i.num.gens
    if (i.unif.migr.scheme == FALSE.AS.INT) {
        s.prompt = "Number of generations before the migration scheme changes (< number of generations)?"
        v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
            1, "integer", c(1, (i.num.gens - 1)))
        lv.generations[["number_of_generations_before_migration_scheme_change"]] = v.user.values[1]
    }
    return(lv.generations)
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
