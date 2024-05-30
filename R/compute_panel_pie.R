compute_panel_pie <- function(data, scales, digits = 1, r_nudge = 0, r_prop = 1){
  
  if(!("weight" %in% names(data))){data$weight <- 1}
  # order matters... Need to add text aesthetics
  # get aes names as they appear in the data
  data_mapped_aes_names <- names(data)[names(data) %in% 
                                         c("fill", "alpha", 
                                             "colour", "group", "linewidth", 
                                             "linetype")]
  
  if(is.null(data$area)){data$area <- 1}
  
  data %>% 
    group_by(across(data_mapped_aes_names)) ->
  data
  
out <- data %>% 
  summarize(count = sum(weight), .groups = 'drop') %>% 
  ungroup() %>% 
  mutate(group = 1:n()) %>% 
  mutate(cum_n = cumsum(.data$count)) %>% 
  mutate(xmax = .data$cum_n/sum(.data$count)) %>% 
  mutate(xmin = lag(.data$xmax)) %>% 
  mutate(xmin = replace_na(.data$xmin, 0)) %>% 
  mutate(r = sqrt(sum(.data$count)/pi)) %>% 
  mutate(r0 = 0) %>% 
  mutate(ymin = 0, 
         ymax = .data$r) %>% 
  mutate(y = 0) # always see zero, donuts.


  if("r" %in% names(data)){out$ymax <- data$r[1]}
  if("r" %in% names(data)){out$r <- data$r[1]}
  if("r0" %in% names(data)){out$ymin <- data$r0[1]}
  # idea that didn't work and I don't know why
  # since rect doesn't use x and y (but xmin xmax etc) this is not as interesting
  # but it seems like a nice unified strategy for supercharging compute functions to do
  # some nice labeling comput for us...  But, not working! Why?
  # if(is_label){out$y <- out$y_text}   

# routine for labels; we do this after r's overridden because y is computed based on this...
out <- out %>% 
  mutate(prop = .data$count/sum(.data$count)) %>% 
  mutate(percent = paste0(round(100*.data$prop, digits), "%")) %>% 
  mutate(r_prop = r_prop) %>% 
  mutate(r_nudge = r_nudge) %>% 
  mutate(x = (.data$xmin + .data$xmax)/2) %>% 
  mutate(y_text = .data$r*.data$r_prop + .data$r_nudge) %>% 
  mutate(angle_wedge =  90 -x*360)

  out
  
}


# compute_panel_pietext <- function(...){
#   
#   compute_panel_pie(..., is_label = T) # overwriting y
#   
#   
# }


