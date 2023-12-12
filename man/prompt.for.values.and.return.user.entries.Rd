\name{prompt.for.values.and.return.user.entries}
\alias{prompt.for.values.and.return.user.entries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
prompt.for.values.and.return.user.entries(s.prompt, i.num.values, s.type.of.values, v.valid.range)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s.prompt}{
%%     ~~Describe \code{s.prompt} here~~
}
  \item{i.num.values}{
%%     ~~Describe \code{i.num.values} here~~
}
  \item{s.type.of.values}{
%%     ~~Describe \code{s.type.of.values} here~~
}
  \item{v.valid.range}{
%%     ~~Describe \code{v.valid.range} here~~
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
function (s.prompt, i.num.values, s.type.of.values, v.valid.range) 
{
    v.values.entered = NULL
    if (!s.type.of.values \%in\% c("integer", "numeric", "character")) {
        s.msg = paste("In function promp.for.values,", "arg s.type.of.values passed invalid value:", 
            s.type.of.values, ".  Expecting one of", "\"integer\", \"numeric\", or \"character\"")
        stop(s.msg)
    }
    b.is.number = (s.type.of.values == "integer" || s.type.of.values == 
        "numeric")
    i.value.counter = 0
    for (idx in 1:i.num.values) {
        i.value.counter = i.value.counter + 1
        v.scan.buff = NULL
        b.is.in.range = FALSE
        s.prompt.with.index.number = sub(pattern = INDEX.HOLDER.STRING, 
            replacement = as.character(idx), x = s.prompt)
        while (!b.is.in.range) {
            cat(paste(s.prompt.with.index.number, "\n"))
            v.scan.buff = scan(nmax = 1, what = s.type.of.values, 
                flush = TRUE, multi.line = FALSE)
            i.result.of.try = tryCatch({
                lr.conversion.functions[[s.type.of.values]](v.scan.buff[1])
            }, error = function(e) {
                return(NULL)
            }, warning = function(w) {
                return(NULL)
            })
            if (is.null(i.result.of.try)) {
                b.is.in.range = FALSE
            }
            else if (!b.is.number) {
                if (is.null(v.valid.range)) {
                  b.is.in.range = TRUE
                }
                else {
                  b.is.in.range = FALSE
                  if (i.result.of.try \%in\% v.valid.range) {
                    b.is.in.range = TRUE
                  }
                }
            }
            else {
                b.is.in.range = (i.result.of.try >= v.valid.range[1] && 
                  i.result.of.try <= v.valid.range[2])
            }
        }
        v.values.entered = c(v.values.entered, i.result.of.try)
    }
    return(v.values.entered)
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
