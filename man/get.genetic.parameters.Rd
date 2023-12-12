\name{get.genetic.parameters}
\alias{get.genetic.parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.genetic.parameters()
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
    lv.genetics = list()
    v.user.values = NULL
    v.user.values = prompt.for.values.and.return.user.entries("Number of loci?", 
        1, "integer", c(1, MAX_NUMBER_LOCI))
    i.num.loci = v.user.values[1]
    lv.genetics[["number_of_loci"]] = i.num.loci
    v.user.values = prompt.for.values.and.return.user.entries("Free recombination between loci?: y/n", 
        1, "character", c("y", "n"))
    s.free.recomb = v.user.values[1]
    i.free.recomb = ifelse(s.free.recomb == "y", TRUE.AS.INT, 
        FALSE.AS.INT)
    lv.genetics[["free_recombination_between_loci"]] = i.free.recomb
    if (i.free.recomb == FALSE) {
        s.prompt = paste("Recombination rate between adjacent loci (ie between locus n and n+1)", 
            "The recombination rate must be comprised between 0.0 and 0.5", 
            sep = "\n")
        v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
            1, "numeric", c(0, 0.5))
        f.recomb = v.user.values[1]
        lv.genetics[["recombination_rate_between_adjacent_loci"]] = f.recomb
    }
    v.user.values = prompt.for.values.and.return.user.entries("Do all loci have the same mutation scheme?:y/n", 
        1, "character", c("y", "n"))
    s.unif.mut.scheme = v.user.values[1]
    i.unif.mut.scheme = ifelse(s.unif.mut.scheme == "y", TRUE.AS.INT, 
        FALSE.AS.INT)
    lv.genetics[["all_loci_same_mutation_scheme"]] = i.unif.mut.scheme
    lv.mutrates = get.mutation.rate(i.num.loci, i.unif.mut.scheme)
    lv.genetics = append(lv.genetics, lv.mutrates)
    lv.mut.scheme = get.mutation.scheme(i.num.loci, i.unif.mut.scheme)
    lv.genetics = append(lv.genetics, lv.mut.scheme)
    lv.allele = get.allele.parameters(i.num.loci, i.unif.mut.scheme)
    lv.genetics = append(lv.genetics, lv.allele)
    return(lv.genetics)
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
