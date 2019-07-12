# performanceTestResolve -------------------------------------------------------
performanceTestResolve <- function
(
  x, dict,
  N = c(1, 5, 10, 50, 100, 200, 500, 1000, 2000, 5000, 10000)[1:n.points],
  n.points = 11
)
{
  runtimes <- sapply(N, function(n) {
    cat("n =", n, "\n")
    c(n = n, system.time(kwb.utils::resolve(x[seq_len(n)], dict)))
  })

  graphics::plot(runtimes["n", ], runtimes["elapsed", ], type = "l")

  runtimes
}
