\name{get.migr.proportions.hier}
\alias{get.migr.proportions.hier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.migr.proportions.hier(g2sex, b.second.scheme)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g2sex}{
%%     ~~Describe \code{g2sex} here~~
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
function (g2sex, b.second.scheme) 
{
    lv.props = list()
    v.user.values = NULL
    v.user.values.a = NULL
    v.user.values.b = NULL
    v.user.values.c = NULL
    if (g2sex == FALSE.AS.INT) {
        v.user.values = prompt.for.values.and.return.user.entries("proportion of migration within groups?(between 0 and 1)", 
            1, "numeric", c(0, 1))
        v.user.values.a = prompt.for.values.and.return.user.entries("proportion of migration between groups?(between 0 and 1)", 
            1, "numeric", c(0, 1))
        f.sum.within.and.between = v.user.values[1] + v.user.values.a[1]
        if (f.sum.within.and.between > 1) {
            stop("The sum of migration within and between must not exceed one")
        }
        if (!b.second.scheme) {
            lv.props[["proportion_migration_within_groups"]] = v.user.values[1]
            lv.props[["proportion_migration_between_groups"]] = v.user.values.a[1]
        }
        else {
            lv.props[["proportion_migration_within_groups_second_scheme"]] = v.user.values[1]
            lv.props[["proportion_migration_between_groups_second_scheme"]] = v.user.values.a[1]
        }
    }
    else {
        v.user.values = prompt.for.values.and.return.user.entries("proportion of female migration within groups?(between 0 and 1)", 
            1, "numeric", c(0, 1))
        v.user.values.a = prompt.for.values.and.return.user.entries("proportion of female migration between groups?(between 0 and 1)", 
            1, "numeric", c(0, 1))
        f.sum.within.and.between = v.user.values[1] + v.user.values.a[1]
        if (f.sum.within.and.between > 1) {
            stop("The sum of migration within and between must not exceed one")
        }
        v.user.values.b = prompt.for.values.and.return.user.entries("proportion of male migration within groups?(between 0 and 1)", 
            1, "numeric", c(0, 1))
        v.user.values.c = prompt.for.values.and.return.user.entries("proportion of male migration between groups?(between 0 and 1)", 
            1, "numeric", c(0, 1))
        f.sum.within.and.between = v.user.values.b[1] + v.user.values.c[1]
        if (f.sum.within.and.between > 1) {
            stop("The sum of migration within and between must not exceed one")
        }
        if (!b.second.scheme) {
            lv.props[["proportion_female_migration_within_groups"]] = v.user.values[1]
            lv.props[["proportion_female_migration_between_groups"]] = v.user.values.a[1]
            lv.props[["proportion_male_migration_within_groups"]] = v.user.values.b[1]
            lv.props[["proportion_male_migration_between_groups"]] = v.user.values.c[1]
        }
        else {
            lv.props[["proportion_female_migration_within_groups_second_scheme"]] = v.user.values[1]
            lv.props[["proportion_female_migration_between_groups_second_scheme"]] = v.user.values.a[1]
            lv.props[["proportion_male_migration_within_groups_second_scheme"]] = v.user.values.b[1]
            lv.props[["proportion_male_migration_between_groups_second_scheme"]] = v.user.values.c[1]
        }
    }
    return(lv.props)
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
