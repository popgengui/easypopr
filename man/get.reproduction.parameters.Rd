\name{get.reproduction.parameters}
\alias{get.reproduction.parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.reproduction.parameters()
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
    lv.repro = list()
    lv.selfing = NULL
    gploidy = NULL
    grecombination = NULL
    g2sex = NULL
    gploidyfem = NULL
    gploidymal = NULL
    gpropauto = NULL
    v.user.values = prompt.for.values.and.return.user.entries("Ploidy level ? (0=haplo-diploid; 1=haploid; 2=diploid)", 
        1, "integer", c(0, 2))
    gploidy = v.user.values[1]
    lv.repro[["ploidy"]] = gploidy
    if (gploidy == 0) {
        print("Note: colonies will be referred to as populations !")
        g2sex = 1
        lv.repro[["two_sexes"]] = g2sex
    }
    else if (gploidy == 1) {
        g2sex = 0
        gploidyfem = 1
        v.user.values = prompt.for.values.and.return.user.entries("Proportion of recombination (between 0 and 1)", 
            1, "numeric", c(0, 1))
        grecombination = v.user.values[1]
        lv.repro[["proportion_recombination"]] = grecombination
        lv.repro[["two_sexes"]] = g2sex
    }
    else if (gploidy == 2) {
        gploidyfem = 2
        gploidymal = 2
        g2sex = 1
        v.user.values = prompt.for.values.and.return.user.entries("Two sexes?:y/n", 
            1, "character", c("y", "n"))
        s.val = v.user.values[1]
        if (s.val == "n") {
            g2sex = 0
        }
        else {
            g2sex = 1
        }
        if (g2sex == 0) {
            v.user.values = prompt.for.values.and.return.user.entries("Random mating?:y/n", 
                1, "character", c("y", "n"))
            s.random.mating = v.user.values[1]
            i.random.mating = ifelse(s.random.mating == "y", 
                TRUE.AS.INT, FALSE.AS.INT)
            lv.selfing[["random_mating"]] = i.random.mating
            if (i.random.mating == FALSE.AS.INT) {
                lv.selfing = get.selfing.parameters()
                lv.repro = append(lv.repro, lv.selfing)
            }
        }
        lv.repro[["two_sexes"]] = g2sex
    }
    return(lv.repro)
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
