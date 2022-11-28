spat_lagp=function(day,data,weights,var){
  day_data=data[as.character(data$date)==day,var]
  lag_vector=lag.listw(var=day_data,x=mat2listw(weights))
  return(lag_vector)
}

apply_weights=function(data,weights,var){
  days=as.data.frame(unique(data$date))
  lag_vector=apply(days,FUN=spat_lagp,data=data,weights=weights,var=var,MARGIN=1)
  return(as.vector(lag_vector))
}
