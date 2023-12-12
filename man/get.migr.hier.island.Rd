\name{get.migr.hier.island}
\alias{get.migr.hier.island}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.migr.hier.island(g2sex, gnbpop, b.second.scheme)
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
    v.user.values = prompt.for.values.and.return.user.entries("Number of archipelagos?", 
        1, "integer", c(1, MAX_NUMBER_POPULATIONS))
    i.num.arch = v.user.values[1]
    if (!b.second.scheme) {
        gfirstrace = i.num.arch
        lv.params[["number_of_archipelagos"]] = gfirstrace
    }
    else {
        gfirstrace2 = i.num.arch
        lv.params[["number_of_archipelagos_second_scheme"]] = gfirstrace2
    }
    v.user.values = prompt.for.values.and.return.user.entries("Same number of populations in each archipelago:y/n", 
        1, "character", c("y", "n"))
    s.unif.pops.per.arch = v.user.values[1]
    i.unif.pops.per.arch = ifelse(s.unif.pops.per.arch == "y", 
        TRUE.AS.INT, FALSE.AS.INT)
    if (!b.second.scheme) {
        lv.params[["same_number_populations_each_archipelago"]] = i.unif.pops.per.arch
    }
    else {
        lv.params[["same_number_populations_each_archipelago_second_scheme"]] = i.unif.pops.per.arch
    }
    if (i.unif.pops.per.arch == FALSE.AS.INT) {
        s.prompt.with.index.holder = paste("enter number of populations of archipelago", 
            INDEX.HOLDER.STRING)
        v.user.values = prompt.for.values.and.return.user.entries(s.prompt.with.index.holder, 
            i.num.arch, "integer", c(1, MAX_NUMBER_POPULATIONS))
        s.per.arch.num.pops = paste(v.user.values, collapse = ",")
        if (!b.second.scheme) {
            lv.params[["per_archipelago_number_of_populations"]] = s.per.arch.num.pops
        }
        else {
            lv.params[["per_archipelago_number_of_populations_second_scheme"]] = s.per.arch.num.pops
        }
    }
    else {
        i.pops.per.arch = as.integer(gnbpop/i.num.arch)
        i.recalc.gnbpop = i.pops.per.arch * i.num.arch
        if (i.recalc.gnbpop != gnbpop) {
            stop("Your values do not match the number of populations!")
        }
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
