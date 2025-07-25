% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tensor_fields.R
\name{g_mink}
\alias{g_mink}
\title{Minkowski metric tensor}
\usage{
g_mink(n, coords = paste0("x", 1:n - 1))
}
\arguments{
\item{n}{The dimension of the metric tensor.}

\item{coords}{A character vector of coordinate names. The length needs
to match the tensor dimensions.}
}
\value{
The covariant metric tensor as array imputed with coordinate names.
}
\description{
Provides the covariant metric tensor in \code{n} dimensions in
Cartesian coordinates with signature \eqn{(-1, 1, 1, ...)}{`c(-1, 1, 1, ...)`}.
}
\examples{
g_mink(4)
g_mink(4) \%_\% .(+i, +j)
}
\seealso{
Wikipedia \href{https://en.wikipedia.org/wiki/Minkowski_space#Minkowski_metric}{Minkowski metric tensor}

Other metric tensors: 
\code{\link{g_eucl_cart}()},
\code{\link{metric_field}()}
}
\concept{metric tensors}
\concept{metric_tensors}
