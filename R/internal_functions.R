# internal function not to be called by users
# stop function that doesn't print call
stop2 <- function (...)
{
  stop(..., call. = FALSE)
}


warning2 <- function (...)
{
  warning(..., call. = FALSE)
}


# message function that changes colors
message2 <- function(x, color = "black")
  message(colortext(x, as = color))


colortext <- function(text, as = c("red", "blue", "green", "magenta", "cyan", "orange", "black", "silver")) {
  if (has_color()) {
    unclass(cli::make_ansi_style(ohun_style(as))(text))
  } else {
    text
  }
}

has_color <- function() {
  cli::num_ansi_colors() > 1
}

ohun_style <- function(color = c("red", "blue", "green", "magenta", "cyan", "orange", "black", "silver")) {
  type <- match.arg(color)

  c(
    red = "red",
    blue = "blue",
    green = "green",
    magenta = "magenta",
    cyan = "cyan",
    orange = "orange",
    black = "black",
    silver = "silver"
  )[[color]]
}

