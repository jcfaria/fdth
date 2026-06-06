## Internal helpers shared by print and summary methods.
## Not exported; prefixed with '.' to signal internal use.

.fdt.format.num <- function(tbl,
                            brk,
                            columns,
                            round.digits,
                            format.classes,
                            pattern,
                            row.names,
                            right,
                            ...)
{
  res <- cbind(tbl[, 1], round(tbl[, 2:6], round.digits))[columns]

  right.tmp <- as.logical(brk['right'])

  if (format.classes) {
    tmp      <- as.character(res[, 1])
    res[, 1] <- make.fdt.format.classes(tmp,
                                        right.tmp,
                                        pattern)
  }

  names(res) <- c('Class limits',
                  'f',
                  'rf',
                  'rf(%)',
                  'cf',
                  'cf(%)')[columns]

  print.data.frame(res,
                   row.names = row.names,
                   right = right, ...)
}

.fdt.format.cat <- function(tbl,
                            columns,
                            round.digits,
                            row.names,
                            right,
                            ...)
{
  res <- cbind(tbl[, 1], round(tbl[, 2:6], round.digits))[columns]

  names(res) <- c('Category',
                  'f',
                  'rf',
                  'rf(%)',
                  'cf',
                  'cf(%)')[columns]

  print.data.frame(res,
                   row.names = row.names,
                   right = right, ...)
}

.fdt.xtable.prep.df <- function(df,
                                math.limits = FALSE)
{
  res_DF <- as.data.frame(df)
  col1 <- trimws(as.character(res_DF[, 1]))

  if (math.limits)
    col1 <- paste0("$", col1, "$")

  res_DF[, 1] <- format(col1,
                        justify = "left")
  res_DF[1, 1] <- paste0("  ", res_DF[1, 1])
  names(res_DF) <- gsub("\\%",
                        "\\\\%",
                        names(res_DF))

  res_DF
}

.fdt.xtable.prep.quantile <- function(x)
{
  qnames <- gsub("\\%",
                 "\\\\%",
                 names(x))

  data.frame(Quantile = qnames,
             Value = as.numeric(x),
             row.names = NULL,
             check.names = FALSE)
}

.fdt.xtable.cat.item <- function(xi)
{
  if (is.list(xi) && !is.null(xi$table))
    return(xi$table)

  as.data.frame(xi)
}
