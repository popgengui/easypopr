\name{setup.easypop}
\alias{setup.easypop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setup.easypop(s.file.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s.file.name}{
%%     ~~Describe \code{s.file.name} here~~
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
function (s.file.name) 
{
    b.exists = file.exists(s.file.name)
    if (b.exists) {
        stop("error:  the file", s.file.name, "already exists.  Please provide ", 
            "a file name not already in use.")
    }
    lv.params = get.ep.parameters()
    lv.as.list = get.parameter.list.for.ep.processing(lv.params, 
        ALL.EP.PARAMS.WITH.TYPE)
    print.param.list(lv.as.list, s.file.name)
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
