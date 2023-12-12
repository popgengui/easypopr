\name{get.mixed.mutation.scheme.details}
\alias{get.mixed.mutation.scheme.details}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.mixed.mutation.scheme.details(s.type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s.type}{
%%     ~~Describe \code{s.type} here~~
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
function (s.type) 
{
    lv.mut.details = list()
    v.user.values = NULL
    s.prompt = NULL
    if (s.type == "kam") {
        s.prompt = "Proportion of Kam mutation events (between 0 and 1)"
        s.key = "proportion_kam_mutation_events"
    }
    else if (s.type == "step") {
        s.prompt = "Proportion of double step mutation events (between 0 and 1)"
        s.key = "proportion_double_step_mutation_events"
    }
    v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
        1, "numeric", c(0, 1))
    f.prop = v.user.values[1]
    lv.mut.details[[s.key]] = f.prop
    return(lv.mut.details)
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
