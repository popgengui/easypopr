\name{get.mutation.scheme}
\alias{get.mutation.scheme}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.mutation.scheme(i.num.loci, i.unif.mut.scheme)
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
    lv.mut.scheme = list()
    lv.mixed.models = list()
    i.this.scheme = NULL
    v.user.values = NULL
    if (i.num.loci > 1 && i.unif.mut.scheme == FALSE.AS.INT) {
        for (i.locus in 1:i.num.loci) {
            s.prompt = paste("Mutation model for locus ", i.locus, 
                "?", MUTATION_MODEL_LIST, sep = "")
            v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
                1, "integer", c(1, MAX_NUMBER_MUTATION_MODEL))
            i.this.scheme = v.user.values[1]
            if (i.locus == 1) {
                lv.mut.scheme[["per_locus_mutation_model"]] = i.this.scheme
            }
            else {
                lv.mut.scheme[["per_locus_mutation_model"]] = paste(lv.mut.scheme[["per_locus_mutation_model"]], 
                  i.this.scheme, sep = ",")
            }
            if (i.this.scheme \%in\% c(3, 4)) {
                lv.this.scheme.details = NULL
                if (i.this.scheme == 3) {
                  lv.this.scheme.details = get.mixed.mutation.scheme.details("kam")
                  s.key.for.single = "proportion_kam_mutation_events"
                  s.key.for.list = "locus_numbers_and_proportions_kam_mutation_events"
                }
                else {
                  lv.this.scheme.details = get.mixed.mutation.scheme.details("step")
                  s.key.for.single = "proportion_double_step_mutation_events"
                  s.key.for.list = "locus_numbers_and_proportions_double_step_mutation_events"
                }
                f.prop = lv.this.scheme.details[[s.key.for.single]]
                s.prop = format(f.prop, scientific = FALSE)
                if (!(s.key.for.list \%in\% names(lv.mixed.models))) {
                  lv.mixed.models[[s.key.for.list]] = paste(i.locus, 
                    s.prop, sep = ",")
                }
                else {
                  s.duple = paste(i.locus, s.prop, sep = ",")
                  s.current.list = lv.mixed.models[[s.key.for.list]]
                  lv.mixed.models[[s.key.for.list]] = paste(s.current.list, 
                    s.duple, sep = ";")
                }
            }
        }
        lv.mut.scheme = append(lv.mut.scheme, lv.mixed.models)
    }
    else {
        s.prompt = paste("Mutation model?", MUTATION_MODEL_LIST, 
            sep = "\n")
        v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
            1, "integer", c(1, MAX_NUMBER_MUTATION_MODEL))
        i.this.scheme = v.user.values[1]
        lv.mut.scheme[["mutation_model"]] = i.this.scheme
        if (i.this.scheme \%in\% c(3, 4)) {
            lv.this.scheme.details = NULL
            s.mixed.type = ifelse(i.this.scheme == 3, "kam", 
                "step")
            lv.this.scheme.details = get.mixed.mutation.scheme.details(s.mixed.type)
            lv.mut.scheme = append(lv.mut.scheme, lv.this.scheme.details)
        }
    }
    return(lv.mut.scheme)
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
