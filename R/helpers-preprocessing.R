## Area ----
is_in_area <- function(x, y, area){
  if(area$type == "square"){
    return(is_in_area_square(x, y, area$points))
  }
}

is_in_area_square <- function(x, y, points){
  return((x >= points$xmin & x <= points$xmax) & (y >= points$ymin & y <= points$ymax))
}
