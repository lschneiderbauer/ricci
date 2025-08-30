## R CMD check results

0 errors \| 0 warnings \| 1 note

> Possibly misspelled words in DESCRIPTION: Christoffel (17:54) Civita
> (17:10) Ricci (2:8, 11:23, 18:28) antisymmetric (15:34)

These are false positives: "Christoffel", "Civita", and "Ricci" are
names, and "antisymmetric" is a common term in several branches of
Mathematics.


> Found the following (possibly) invalid URLs:
>  URL: https://en.wikipedia.org/wiki/Riemann_curvature_tensor
>    From: man/riemann.Rd
>    Status: 429
>    Message: Too Many Requests
>  URL: https://en.wikipedia.org/wiki/Riemann_curvature_tensor#Ricci_curvature
>    From: man/ricci.Rd
>    Status: 429
>    Message: Too Many Requests
>  URL: https://en.wikipedia.org/wiki/Scalar_curvature
>    From: man/ricci_sc.Rd
>    Status: 429
>    Message: Too Many Requests
>  URL: https://en.wikipedia.org/wiki/Schwarzschild_metric
>    From: man/g_ss.Rd
>    Status: 429
>    Message: Too Many Requests
>  URL: https://en.wikipedia.org/wiki/Sphere
>    From: man/g_sph.Rd
>    Status: 429
>    Message: Too Many Requests
>  URL: https://en.wikipedia.org/wiki/Tensor_field
>    From: inst/doc/tensor_fields.html
>    Status: 429
>    Message: Too Many Requests

These are false positives.


-   This is a resubmission

## Response

> If there are references describing the methods in your package, please
> add these in the description field of your DESCRIPTION file in the
> form authors (year) \<doi:...\> authors (year, ISBN:...) or if those
> are not available: \<https:...\> with no space after 'doi:', 'https:'
> and angle brackets for auto-linking. (If you want to add a title as
> well please put it in quotes: "Title")

There are no singular references, the topic is considered "textbook
knowledge" in theoretical physics. We could cite a random textbook, but
I do not think there is a book that stands out among the available
textbooks.

> Please add \value to .Rd files regarding exported methods and explain
> the functions results in the documentation. Please write about the
> structure of the output (class) and also what the output means. (If a
> function does not return a value, please document that too, e.g.
> \value{No return value, called for side effects} or similar) For more
> details:
> <https://contributor.r-project.org/cran-cookbook/docs_issues.html#missing-value-tags-in-.rd-files>
> Missing Rd-tags: as.array.tensor.Rd: \value metric_field.Rd: \value

I added the missing documentation.

> \dontrun{} should only be used if the example really cannot be
> executed (e.g. because of missing additional software, missing API
> keys, ...) by the user. That's why wrapping examples in \dontrun{}
> adds the comment ("\# Not run:") as a warning for the user. Does not
> seem necessary. Please replace \dontrun with \donttest. -\>
> Ops.tensor.Rd Please unwrap the examples if they are executable in \<
> 5 sec, or replace dontrun{} with \donttest{}. If you want to show an
> error, please wrap the example in try() instead. For more details:
> <https://contributor.r-project.org/cran-cookbook/general_issues.html#structuring-of-examples>

I replaced dontrun{} with `try()`. Thanks for the information.
