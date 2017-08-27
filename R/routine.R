#' @useDynLib your-package-name
#' @importFrom Rcpp sourceCpp
NULL

#' Square a number
#'
#' Take in any numeric value and square it
#' @param x A numeric value to be squared
#' @return The square of the input
#' @export
square <- function(x){
  return(x^2)
}

#' An extention of fasttime::fastPOSIXct
#'
#' Adds flexibity to both input and output timezones
#' @param x strig vector to interpret as timestamps
#' @param from_tz the time zone x lies in
#' @param the time zone we are converting to
#' @param required.components same as fasttime::fastPOSIXct
#' @examples
#' fastPOSIXctWrapper("2017-04-29 20:26:11", from_tz = "America/New_York", to_tz = "UTC")
#' @export
fastPOSIXctWrapper <- function(x, from_tz = NULL, to_tz = NULL, required.components = 3L){

  xx <- Sys.time()
  yy <- as.character(as.POSIXlt(xx, tz = "UTC"))%>%
    substr(1, 19)%>%
    as.POSIXct(tz = from_tz)

  tdff <- round(as.numeric(yy - xx)/.5)*.5*60*60  # seconds

  return(fasttime::fastPOSIXct(x, tz = to_tz, required.components = 3L) + tdff)

}

#' Apply a boolean function to each entry of x
#'
#' [f(x_1), f(x_2), ..., f(x_n)]
#' @param f a boolean function
#' @param x a numeric vector
#' @return [(f(x_1), f(x_2), ..., f(x_n))]
#' @examples
#' where(function(x) x>0, -2:2)
#' @export
where <- function(f, x) {
  vapply(x, f, logical(1))
}

#' CJ.dt CJ on data.table
#'
#' @param X the first data table
#' @param Y the second data table
#' @examples
#' CJ.dt(data.table(X = 1:2), data.table(Y = c('a','b')))
#' @export
CJ.dt <- function(X, Y) {

  stopifnot(is.data.table(X), is.data.table(Y))

  X = X[, c(k = 1L, .SD)]%>%
  setkey(k)

  Y = Y[, c(k = 1L, .SD)]%>%
  setkey(NULL)

  return( X[Y, allow.cartesian=TRUE][, k := NULL][] )

}

#' sample random rows from the data table X
#'
#' @param X input data table
#' @param nrow number of rows to be sampled
#'
#' @examples
#' sample.dt(data.table(num = 1:10, lett = letters[1:10], Lett = LETTERS[1:10]))
#' @export
sample.dt <- function(X, nrow = 1, ...){

  return(X[sample(.N, nrow, ...)])

}

#' Multiple plot function
#'
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' - cols:   Number of columns in layout
#' - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#'
#' @export
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' Split a vector x into a list of n
#'
#' This function is pretty useful to use together with parallel computing,
#' e.g. when using clusterApply
#'
#' @param xx a vector to be split
#' @param n # of group we want xx to be split into
#'
#' @examples
#' split_into_n(1:100,10)
#' split_into_n(letters[1:20],3)
#'
#' @export
split_into_n <- function(xx, n){

  xx <- unique(xx)
  max <- length(xx)/n
  x <- seq_along(xx)
  return(split(xx, ceiling(x/max)))

}

#' Save dt into several json files
#'
#' @param dt the input data table
#' @param out_folder place to put output files
#' @param json_file_name names of the output json files
#' @param nt number of threads to use in parallel computing
#'
#' @examples
#' dt <- data.table(num = 1:26, lett = letters, LETT = LETTERS)
#' cm::save_json(dt, json_file_name = 'test', nt = 3)
#' jsonlite::read_json('out/file_2.json', simplifyVector = TRUE)
#' @export
save_json <- function(dt, out_folder = 'out/', json_file_name = 'file', nt = 5){

  # save as rds, so that we can parallel
  message("dt is stored at ", paste0(getwd(), '/', out_folder, "dt.rds"), ',\n')
  saveRDS(dt, paste0(out_folder, "dt.rds"))

  out_json_file <- paste0(getwd(), '/', out_folder, json_file_name)

  cl<-snow::makeCluster(nt, type="SOCK", outfile = paste0(out_folder, 'creat_json.txt'))
  snow::clusterMap(cl, create_json_single, idx = split_into_n(seq_len(dim(dt)[1]), nt),
                   cnt = 1:nt, out_json_file = out_json_file,
                   in_file = paste0(getwd(), '/', out_folder, "dt.rds")) #paste0(out_folder, "dt.rds"))
  stopCluster(cl)

}

#' Create json file for one part
#'
#' @param idx the rows to be put in this json file
#' @param cnt specifies the thread number
#' @param out_json_file names of the output json files
#' @in_file where to read the rds file
create_json_single <- function(idx, cnt, out_json_file, in_file){

  message('this is the ', cnt, '-th part in creating json files for dt. \n')

  yy <- readRDS(in_file)

  yy <- yy[idx,]

  yy_json <- jsonlite::toJSON(yy, pretty = TRUE)
  writeLines(yy_json, paste0(out_json_file, '_', cnt, '.json'))

  return(NULL)

}

#' find the final retired id and active id after infinity step
#'
#' @examples
#' one_step_map <- data.table(retired_id = c('A','B','C', 'E'), active_id = c('B','C','D','F'))
#' map_infty_step <- find_infty_step_map(one_step_map)
#' @export
find_infty_step_map <- function(map_one_step){

  xx <- map_one_step[retired_id != active_id]
  map_n_step <- xx[, .(from = retired_id, to_p = 'ZzZzZzZ', to = active_id)]
  map_n_step[, done:=ifelse(to == to_p, 1, 0)]
  map_n_step[, step:= 1]
  ratio <- map_n_step[, sum(done)]/dim(map_n_step)[1]
  ratio_p <- -1

  while(ratio < 1){


    map_n_step[, to_p := to]
    map_n_step[done == 0, to:=plyr::mapvalues(map_n_step[done==0, to], from = xx$retired_id, to = xx$active_id,
                                        warn_missing = FALSE)]

    map_n_step[, done:=ifelse(to == to_p, 1, 0)] #1: no change

    map_n_step[, step:=step + 1 - done]

    ratio <- map_n_step[, sum(done)]/dim(map_n_step)[1]

    message('the ratio is ', ratio, '\n')

    if(ratio_p == ratio) {

      message('cycles might exist in the directed graph, \n')
      break;

    }

    ratio_p <- ratio

  }

  map_infty_step <- map_n_step[, .(retired_id = from, active_id = to,
                                   status = done, step)]

  return(map_infty_step)

}





