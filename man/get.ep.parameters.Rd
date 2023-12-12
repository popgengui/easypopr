\name{get.ep.parameters}
\alias{get.ep.parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.ep.parameters()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    lv.param.vals = list()
    lv.repro = get.reproduction.parameters()
    lv.param.vals = append(lv.param.vals, lv.repro)
    lv.mating = get.mating.parameters(lv.param.vals$ploidy, lv.param.vals$two_sexes)
    lv.param.vals = append(lv.param.vals, lv.mating)
    lv.pop = get.population.parameters(lv.param.vals$ploidy, 
        lv.param.vals$two_sexes)
    lv.param.vals = append(lv.param.vals, lv.pop)
    lv.migr = get.migration.parameters(lv.param.vals$two_sexes, 
        lv.param.vals$number_populations)
    lv.param.vals = append(lv.param.vals, lv.migr)
    lv.genetics = get.genetic.parameters()
    lv.param.vals = append(lv.param.vals, lv.genetics)
    lv.generations = get.generation.parameters(lv.param.vals[["same_migration_scheme_all_simulation"]])
    lv.param.vals = append(lv.param.vals, lv.generations)
    lv.output = get.output.parameters(lv.param.vals$number_populations, 
        lv.param.vals$two_sexes)
    lv.param.vals = append(lv.param.vals, lv.output)
    return(lv.param.vals)
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
