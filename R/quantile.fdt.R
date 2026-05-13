quantile.fdt <- function(x,
                         ...,
                         i=1,
                         probs=seq(0, 1, 0.25))
{
  if (!is.numeric(i) || length(i) < 1 || any(is.na(i)) || any(!is.finite(i)))
    stop("'i' must be a finite numeric vector without missing values.")

  if (any(i != as.integer(i)))
    stop("'i' must contain integer values only.")

  if (!is.numeric(probs) || length(probs) < 2 || any(is.na(probs)) || any(!is.finite(probs)))
    stop("'probs' must be a finite numeric vector with at least two values.")

  if (is.unsorted(probs))
    stop("'probs' must be sorted in non-decreasing order.")

  if (any(probs < 0) || any(probs > 1))
    stop("'probs' values must be between 0 and 1.")

  i.max <- length(probs) - 1
  if (any(i < 1) || any(i > i.max))
    stop(sprintf("'i' values must be between 1 and %d for the supplied 'probs'.", i.max))

  fdt <- with(x,
              table)

  n <- fdt[nrow(fdt), 5]
  brk <- with(x,
              seq(breaks['start'],
                  breaks['end'],
                  breaks['h']))

  h <- as.vector(with(x,
                      breaks['h']))

  getQ <- function(ii)
  {
    qpos <- probs[ii + 1L] * n

    posQ <- which(qpos <= fdt[, 5] &
                    fdt[, 2] > 0)[1]

    if (is.na(posQ))
      stop("Unable to locate a valid class interval for the requested quantile.")

    liQ <- brk[posQ]

    # It is important if 'posQ ' is inside of the first class
    if (posQ - 1 <= 0)
      sfaQ <- 0
    else
      sfaQ <- fdt[(posQ - 1), 5]

    fQ <- fdt[posQ, 2]

    Q <- liQ + ((qpos - sfaQ) * h) / fQ
  }

  res <- sapply(i,
                getQ)

  # Keep names aligned with the selected probability levels (e.g., 25%, 50%).
  q.pct <- sprintf("%.6f", 100 * probs[i + 1L])
  q.pct <- sub("0+$", "", q.pct)
  q.pct <- sub("\\.$", "", q.pct)
  names(res) <- paste0(q.pct, "%")

  return(res)
}
