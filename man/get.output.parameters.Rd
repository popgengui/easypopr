\name{get.output.parameters}
\alias{get.output.parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mytitle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.output.parameters(i.number.of.populations, i.two.sexes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i.number.of.populations}{
%%     ~~Describe \code{i.number.of.populations} here~~
}
  \item{i.two.sexes}{
%%     ~~Describe \code{i.two.sexes} here~~
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
function (i.number.of.populations, i.two.sexes) 
{
    lv.output = list()
    v.user.values = NULL
    i.complete.data = NULL
    i.pops.out = NULL
    s.prompt = "Do you want the complete dataset in the '.dat' and '.gen' result files ?:y/n"
    v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
        1, "character", c("y", "n"))
    s.complete.data = v.user.values[1]
    i.complete.data = ifelse(s.complete.data == "y", TRUE.AS.INT, 
        FALSE.AS.INT)
    lv.output[["complete_dataset_in_dat_and_gen_files"]] = i.complete.data
    if (i.complete.data == FALSE.AS.INT) {
        s.prompt = "How many populations in the result files (less than simulated populations)"
        v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
            1, "integer", c(1, i.number.of.populations - 1))
        i.pops.out = v.user.values[1]
        lv.output[["number_populations_in_result_files"]] = i.pops.out
        if (i.two.sexes == TRUE.AS.INT) {
            s.prompt = "How many females in each population (less than simulated females)"
            v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
                1, "integer", c(1, MAX_NUMBER_INDIVIDUALS_IN_POP))
            lv.output[["number_females_per_population_in_result_files"]] = v.user.values[1]
            s.prompt = "How many males in each population (less than simulated females)"
            v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
                1, "integer", c(1, MAX_NUMBER_INDIVIDUALS_IN_POP))
            lv.output[["number_males_per_population_in_result_files"]] = v.user.values[1]
        }
        else {
            s.prompt = "How many individuals in each population (less than simulated individuals)"
            v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
                1, "integer", c(1, MAX_NUMBER_INDIVIDUALS_IN_POP))
            lv <- output[["number_individuals_in_each_population_in_result_files"]] = v.user.values[1]
        }
    }
    s.prompt = paste("Do you want a file giving the complete pedigree of the simulation ?:y/n", 
        "(Please notice that this file can be very huge and will slow down simulations)", 
        sep = "\n")
    v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
        1, "character", c("y", "n"))
    s.pedigree = v.user.values[1]
    i.pedigree = ifelse(s.pedigree == "y", TRUE.AS.INT, FALSE.AS.INT)
    lv.output[["file_giving_pedigrees"]] = i.pedigree
    s.prompt = "Name of the file?"
    v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
        1, "character", NULL)
    lv.output[["name_of_file"]] = v.user.values[1]
    s.prompt = "Number of replicates? (between 1 and 999)"
    v.user.values = prompt.for.values.and.return.user.entries(s.prompt, 
        1, "integer", c(1, MAX_NUMBER_REPLICATES))
    lv.output[["number_of_replicates"]] = v.user.values[1]
    return(lv.output)
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
