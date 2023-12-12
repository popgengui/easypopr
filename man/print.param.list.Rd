\name{print.param.list}
\alias{print.param.list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.param.list(lv.param.list, s.filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lv.param.list}{
%%     ~~Describe \code{lv.param.list} here~~
}
  \item{s.filename}{
%%     ~~Describe \code{s.filename} here~~
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
function (lv.param.list, s.filename) 
{
    vs.lines = c()
    for (s.name in names(lv.param.list)) {
        b.include = check.for.inclusion.in.config.file(s.name, 
            lv.param.list)
        if (b.include) {
            s.line = paste(s.name, ":\t", lv.param.list[[s.name]], 
                sep = "")
            vs.lines = c(vs.lines, s.line)
        }
    }
    s.combined.lines = paste(vs.lines, collapse = "\n")
    o.file.conn = file(s.filename)
    writeLines(s.combined.lines, o.file.conn)
    close(o.file.conn)
    return()
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
