\name{get.mating.parameters}
\alias{get.mating.parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.mating.parameters(gploidy, g2sex)
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
    lv.mating = list()
    v.user.values = NULL
    gmatesyst = NULL
    gpropmono = NULL
    gproppoly = NULL
    if (gploidy == 2 && g2sex == 1) {
        s.prompt = paste("Mating system?\n", "1= Random mating\n", 
            "2= Polygyny\n", "3= Monogyny\n")
        v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
            1, "integer", c(1, 3))
        gmatesyst = v.user.values[1]
        lv.mating[["mating_system"]] = gmatesyst
        if (gmatesyst == 3) {
            v.user.values = prompt.for.values.and.return.user.entries("Proportion of extra pair copulations (between 0 and 1)", 
                1, "numeric", c(0, 1))
            gpropmono = v.user.values[1]
            lv.mating[["proportion_extra_pair_copulation"]] = gpropmono
        }
        else if (gmatesyst == 2) {
            v.user.values = prompt.for.values.and.return.user.entries("Proportion of matings by subordinate males (between 0 and 1)", 
                1, "numeric", c(0, 1))
            gproppoly = v.user.values[1]
            lv.mating[["proportion_matings_by_subordinate_males"]] = gproppoly
        }
    }
    return(lv.mating)
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
