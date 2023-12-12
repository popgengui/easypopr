\name{get.selfing.parameters}
\alias{get.selfing.parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.selfing.parameters()
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
    lv.selfing = list()
    clonality = NULL
    grecombination = NULL
    gpropauto = NULL
    v.user.values = NULL
    if (s.random.mating == "n") {
        v.user.values = prompt.for.values.and.return.user.entries("Proportion of clonal reproduction (between 0 and 1)", 
            1, "numeric", c(0, 1))
        clonality = v.user.values[1]
        lv.selfing[["proportion_clonal_mating"]] = clonality
        if (clonality != 1) {
            v.user.values = prompt.for.values.and.return.user.entries("Proportion of selfing (between 0 and 1)\nof the individuals not born as clones", 
                1, "numeric", c(0, 1))
            gpropauto = v.user.values[1]
        }
        else {
            gpropauto = 0
        }
        lv.selfing[["proportion_selfing_of_nonclones"]] = gpropauto
    }
    return(lv.selfing)
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
