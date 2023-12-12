\name{get.allele.parameters}
\alias{get.allele.parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.allele.parameters(i.num.loci, i.unif.mut.scheme)
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
    lv.allele = list()
    v.user.values = NULL
    if (i.num.loci > 1 && i.unif.mut.scheme == FALSE.AS.INT) {
        s.prompt = paste("Number of possible allelic states of locus,", 
            INDEX.HOLDER.STRING, "? (below 1000)")
        v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
            i.num.loci, "integer", c(1, MAX_NUMBER_ALLELIC_STATES))
        s.per.locus.allelic.states = paste(v.user.values, collapse = ",")
        lv.allele[["per_locus_number_possible_allelic_states"]] = s.per.locus.allelic.states
    }
    else {
        s.prompt = paste("Number of possible allelic states? (below 1000)")
        v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
            1, "integer", c(1, MAX_NUMBER_ALLELIC_STATES))
        lv.allele[["number_possible_allelic_states"]] = v.user.values[1]
    }
    s.prompt = paste("Variability of the initial population", 
        "1= Maximal, (randomly assigned alleles)", "2= Minimal, (all individuals start with the same allele", 
        sep = "\n")
    v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
        1, "integer", c(1, 2))
    lv.allele[["variability_initial_population"]] = v.user.values[1]
    return(lv.allele)
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
