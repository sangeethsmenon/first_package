\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dijkstra(graph, init_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{init_node}{
%%     ~~Describe \code{init_node} here~~
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
function (graph, init_node) 
{
    actual_vertices <- unique(c(graph$v1, graph$v2))
    no_vertices <- length(actual_vertices)
    short <- rep(Inf, no_vertices)
    short[init_node] <- 0
    checked_vertices <- rep(FALSE, no_vertices)
    for (no in 1:(no_vertices - 1)) {
        initial_length <- Inf
        start_vertex <- -1
        for (i in 1:no_vertices) {
            if (!checked_vertices[i] && short[i] < initial_length) {
                initial_length <- short[i]
                start_vertex <- i
            }
        }
        checked_vertices[start_vertex] <- TRUE
        for (element in 1:nrow(graph)) {
            if (!checked_vertices[graph$v2[element]] && graph$v1[element] == 
                actual_vertices[start_vertex] && (short[start_vertex] + 
                graph$w[element] < short[graph$v2[element]])) {
                short[graph$v2[element]] <- short[start_vertex] + 
                  graph$w[element]
            }
        }
    }
    return(short)
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
