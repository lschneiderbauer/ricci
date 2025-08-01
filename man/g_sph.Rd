% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tensor_fields.R
\name{g_sph}
\alias{g_sph}
\title{Metric tensor of the sphere}
\usage{
g_sph(n, coords = paste0("ph", 1:n))
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
Provides the metric tensor of the sphere \eqn{S^n} with radius 1.
\code{g_sph()} returns a symbolic tensor field in generalized spherical
coordinates \eqn{{\phi_1, \phi_2, ..., \phi_{n-1}}}{{r, `ph1`, `ph2`, ..., `ph(n-2)`}}.
\deqn{d\Omega^2= d\phi_1^2 + \sum_{i=1}^{n-1} \prod_{m=1}^{i-1} sin(\phi_m)^2 d\phi_i^2}
}
\details{
As usual, spherical coordinates are degenerate at \eqn{\phi_l = 0}, so be
careful around those points.
}
\examples{
g_sph(3)
g_sph(3) \%_\% .(+i, +j)
}
\seealso{
Wikipedia: \href{https://en.wikipedia.org/wiki/Sphere}{Sphere}

Other metric tensors: 
\code{\link{g_eucl_cart}()},
\code{\link{g_mink_cart}()},
\code{\link{g_ss}()},
\code{\link{metric_field}()}
}
\concept{metric tensors}
\concept{metric_tensors}
