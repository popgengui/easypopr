\name{get.parameter.list.for.ep.processing}
\alias{get.parameter.list.for.ep.processing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.parameter.list.for.ep.processing(lv.param.vals, lv.ep.list.with.types)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lv.param.vals}{
%%     ~~Describe \code{lv.param.vals} here~~
}
  \item{lv.ep.list.with.types}{
%%     ~~Describe \code{lv.ep.list.with.types} here~~
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
function (lv.param.vals, lv.ep.list.with.types) 
{
    lv.param.list = list()
    vs.types.already.character = c("array.integer", "array.numeric", 
        "matrix.numeric", "list.numeric", "character")
    for (s.name in names(lv.param.vals)) {
        v.val = lv.param.vals[[s.name]]
        s.valtype = lv.ep.list.with.types[[s.name]][["valtype"]]
        v.converted.value = NULL
        if (s.valtype == "logical") {
            if (v.val == TRUE.AS.INT) {
                v.converted.value = "y"
            }
            else if (v.val == FALSE.AS.INT) {
                v.converted.value = "n"
            }
            else {
                stop("error:  found value of logical param", 
                  s.name, ", ", v.val, ", expecting ", TRUE.AS.INT, 
                  " or ", FALSE.AS.INT, sep = "")
            }
        }
        else if (s.valtype == "integer") {
            v.converted.value = as.character(v.val)
        }
        else if (s.valtype == "numeric") {
            v.converted.value = format(v.val, scientific = FALSE)
        }
        else if (s.valtype \%in\% vs.types.already.character) {
            v.converted.value = v.val
        }
        else {
            stop("error:  format type not recognized: ", s.valtype)
        }
        lv.param.list[[s.name]] = v.converted.value
    }
    return(lv.param.list)
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
