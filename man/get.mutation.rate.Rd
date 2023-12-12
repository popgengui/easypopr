\name{get.mutation.rate}
\alias{get.mutation.rate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.mutation.rate(i.num.loci, i.unif.mut.scheme)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i.num.loci}{
%%     ~~Describe \code{i.num.loci} here~~
}
  \item{i.unif.mut.scheme}{
%%     ~~Describe \code{i.unif.mut.scheme} here~~
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
function (i.num.loci, i.unif.mut.scheme) 
{
    lv.mutation.rates = list()
    v.user.values = NULL
    if (i.num.loci > 1 && i.unif.mut.scheme == FALSE.AS.INT) {
        s.prompt = paste("Mutation rate of locus", INDEX.HOLDER.STRING, 
            "? (between 0 and 1, e.g. 0.0001)")
        v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
            i.num.loci, "numeric", c(0, 1))
        v.user.values = format.vals.as.nonscientific.strings(v.user.values)
        s.per.locus.mut.rate = paste(v.user.values, collapse = ",")
        lv.mutation.rates[["per_locus_mutation_rate"]] = s.per.locus.mut.rate
    }
    else {
        v.user.values = prompt.for.values.and.return.user.entries("Mutation rate? (between 0 and 1, e.g. 0.0001)", 
            1, "numeric", c(0, 1))
        f.mut.rate = v.user.values[1]
        lv.mutation.rates[["mutation_rate"]] = f.mut.rate
    }
    return(lv.mutation.rates)
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
