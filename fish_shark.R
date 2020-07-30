rm(list = ls())
library(tidyverse)

n_fish <- 10
n_shark <- 3

x_fish_position <- runif(n_fish, min = -50, max = 50) 
y_fish_position <- runif(n_fish, min = -50, max = 50)
x_shark_position <- runif(n_shark, min = -50, max = 50)
y_shark_position <- runif(n_shark, min = -50, max = 50)
fish <- rep('fish', n_fish)
shark <- rep('shark', n_shark)

data <- tibble(
  x = c(x_fish_position, x_shark_position),
  y = c(y_fish_position, y_shark_position),
  name = c(fish, shark),
  id = 1:(n_fish + n_shark)
)


# detect the nearest agent if you input target agent
detect_nearest_agent <- function(agent){
  
  get_distance <- function(x1, y1, x2, y2){
    dist <-  sqrt((x1 - x2)^2  + (y1 - y2)^2)
    return(dist)
  }
  
  selected_id <- 
    agent %>% 
    select(id) %>% 
    unlist()
  
  others_data <- 
    data %>% 
    filter(id != selected_id)
  
  target_data <- 
    data %>% 
    filter(id == selected_id)
  
  target_x <- 
    target_data %>% 
    select(x) %>% 
    unlist()
  
  target_y <- 
    target_data %>% 
    select(y) %>% 
    unlist()
  
  dist_list <- NULL
  id_list <- NULL
  for(idx in 1:nrow(others_data)){
    others_x <- 
      others_data %>% 
      .[idx, ] %>% 
      select(x) %>% 
      unlist()
    
    others_y <- 
      others_data %>% 
      .[idx, ] %>% 
      select(y) %>% 
      unlist()
    
    id <- 
      others_data %>% 
      .[idx, ] %>% 
      select(id) %>%
      unlist()
    
    dist <- get_distance(target_x, target_y, others_x, others_y)
    dist_list[idx] <- dist
    id_list[idx] <- id
  }
  
  nearest_id <- 
    tibble(dist = dist_list, id = id_list) %>% 
    filter(dist == min(.$dist)) %>% 
    select(id) %>% 
    unlist()
  
  data %>% 
    filter(id == nearest_id) %>% 
    return()
}

follow <- function(agent, nearest_agent){
  if(agent$x < nearest_agent$x){
    agent$x <- agent$x + 1
  } else if(nearest_agent$x < agent$x){
    agent$x <- agent$x - 1
  } else{ # agent$x == nearest_agent$x
    agent$x <- agent$x
  }
  
  if(agent$y < nearest_agent$y){
    agent$y <- agent$y + 1
  } else if(nearest_agent$y < agent$y){
    agent$y <- agent$y - 1
  } else{ # agent$y == nearest_agent$y
    agent$y <- agent$y
  }
  
  return(agent)
}

escape <- function(agent, nearest_agent){
  if(agent$x < nearest_agent$x){
    agent$x <- agent$x - 1
  } else if(nearest_agent$x < agent$x){
    agent$x <- agent$x + 1
  } else{ # agent$x == nearest_agent$x
    agent$x <- agent$x + runif(1, min = -1, max = +1) %>% round()
  }
  
  if(agent$y < nearest_agent$y){
    agent$y <- agent$y - 1
  } else if(nearest_agent$y < agent$y){
    agent$y <- agent$y + 1
  } else{ # agent$y == nearest_agent$y
    agent$y <- agent$y + runif(1, min = -1, max = +1) %>% round()
  }
  
  return(agent)
}

movement_shark <- function(agent, nearest_agent){
  if(nearest_agent$name == 'fish'){
    follow(agent, nearest_agent) %>% return()
  } else if(nearest_agent$name == 'shark'){
    escape(agent, nearest_agent) %>% return()
  }
}

movement_fish <- function(agent, nearest_agent){
  if(nearest_agent$name == 'shark'){
    escape(agent, nearest_agent) %>% return()
  } else{
    agent %>% return()
  }
}

get_new_position <- function(target_agent, nearest_agent){
  
  if(target_agent$name == 'fish'){
    target_agent_new_position <- target_agent %>% 
      movement_fish(nearest_agent)  
  } else if(target_agent$name == 'shark'){
    target_agent_new_position <- target_agent %>% 
      movement_shark(nearest_agent)
  }
  
  return(target_agent_new_position)
}

g_init <- 
  data %>% 
  ggplot() +
  geom_point(aes(x = x, y = y, color = name), size = 3)+
  theme_bw() +
  scale_color_manual(values = c("blue", "red"))

g <- NULL
for(t in 1:1000){
  for(i in data$id){
    target_agent <- data %>% 
      .[i, ]  # change
    nearest_agent <- target_agent %>% 
      detect_nearest_agent()
    
    data[i, ] <- target_agent %>% get_new_position(nearest_agent)
    
    g[[t]] <- 
      data %>% 
      ggplot() +
      geom_point(aes(x = x, y = y, color = name), size = 3)+
      theme_bw() +
      scale_color_manual(values = c("blue", "red"))
    
    print(i)
  }
  print('finish every i')
  print('-----------')
  print(t)
  print('-----------')
}



  
  
  

