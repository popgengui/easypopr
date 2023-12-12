\name{get.migr.spatial}
\alias{get.migr.spatial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.migr.spatial(g2sex, gnbpop, b.second.scheme)
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
    v.user.values = NULL
    i.num.dimen = NULL
    f.mean.dispersal = NULL
    f.mean.female.dispersal = NULL
    f.mean.male.dispersal = NULL
    s.coord.matrix = NULL
    lv.proportions = get.migr.proportions.non.hier(g2sex, b.second.scheme)
    lv.params = append(lv.params, lv.proportions)
    v.user.values = prompt.for.values.and.return.user.entries("How many dimensions define the space(at least one)", 
        1, "integer", c(1, MAX_NUMBER_DIMENSIONS))
    i.num.dimen = v.user.values[1]
    for (i.pop in 1:gnbpop) {
        s.prompt = paste("enter coordinate ", INDEX.HOLDER.STRING, 
            " of population ", i.pop)
        v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
            i.num.dimen, "integer", c(1, MAX_NUMBER_DIMENSIONS))
        v.user.values = format.vals.as.nonscientific.strings(v.user.values)
        s.coords.this.pop = paste(v.user.values, collapse = ",")
        if (i.pop == 1) {
            s.coord.matrix = s.coords.this.pop
        }
        else {
            s.coord.matrix = paste(s.coord.matrix, s.coords.this.pop, 
                sep = ";")
        }
    }
    if (!b.second.scheme) {
        lv.params[["per_population_per_dimension_coordinates"]] = s.coord.matrix
    }
    else {
        lv.params[["per_population_per_dimension_coordinates_second_scheme"]] = s.coord.matrix
    }
    if (g2sex == FALSE.AS.INT) {
        v.user.values = s.prompt.with.index.holder = paste("enter mean dispersal distance", 
            1, "numeric", c(0, MAX_DISPERSAL_DISTANCE))
        f.mean.dispersal = v.user.values[1]
        if (!b.second.scheme) {
            lv.params[["mean_dispersal_distance"]] = f.mean.dispersal
        }
        else {
            lv.params[["mean_dispersal_distance_second_scheme"]] = f.mean.dispersal
        }
    }
    else {
        v.user.values = prompt.for.values.and.return.user.entries("enter mean female dispersal distance", 
            1, "numeric", c(0, MAX_DISPERSAL_DISTANCE))
        f.mean.female.dispersal = v.user.values[1]
        v.user.values = prompt.for.values.and.return.user.entries("enter mean male dispersal distance", 
            1, "numeric", c(0, MAX_DISPERSAL_DISTANCE))
        f.mean.male.dispersal = v.user.values[1]
        if (!b.second.scheme) {
            lv.params[["mean_female_dispersal_distance"]] = f.mean.female.dispersal
            lv.params[["mean_male_dispersal_distance"]] = f.mean.male.dispersal
        }
        else {
            lv.params[["mean_female_dispersal_distance_second_scheme"]] = f.mean.female.dispersal
            lv.params[["mean_male_dispersal_distance_second_scheme"]] = f.mean.male.dispersal
        }
    }
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
