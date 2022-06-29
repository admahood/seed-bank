# setup ========================================================================
libs <- c("tidyverse", "tidygraph","cowplot", "ggraph", "lavaan", "ggpubr", 
          "classInt", "ggtext","HDInterval")
# install.packages("ggraph")
lapply(libs, library, character.only = TRUE)

# if there's a font error:
# extrafont::font_import()
# then restart rstudio
# extrafont::loadfonts()

# plotting functions adapted, heavily modified, from: ==========================
# https://drsimonj.svbtle.com/ggsem-plot-sem-models-with-ggplot2 
# also this:
# https://cran.r-project.org/web/packages/ggraph/vignettes/Edges.html

ggsem <- function(fit, filename, title="Path Model",layout_df = NA,
                  layout = "auto", alpha = 0.05, exclude = "none") {
  
  # Extract standardized parameters
  params <- lavaan::standardizedSolution(fit) %>%
    filter(lhs != "exclude",
           rhs != "exclude")
  # Edge properties
  
  param_edges <- params %>% 
    filter(op %in% c("=~", "~", "~~"), lhs != rhs) %>% #, pvalue < .10) %>%
    transmute(to = lhs,
              from = rhs,
              pvalue=pvalue,
              # sig=sig,
              val = est.std,
              type = dplyr::case_when(
                op == "~"  ~ "regression",
                op == "~~" ~ "correlation",
                TRUE ~ NA_character_)) %>%
    mutate(val = ifelse(pvalue >= alpha, 0, val))
  
  lut_cols<-c("< -0.75" = "red",
              "-0.5 - -0.75" = "orange",
              "0 - -0.5" = "peachpuff", 
              "0" = "grey70",
              "0 - 0.5" = "grey80", 
              "0.5 - 0.75" = "grey40", 
              "> 0.75"= "black")
  lut_lty<-c("< -0.75" = 6,
             "-0.5 - -0.75" = 6,
             "0 - -0.5" = 6, 
             "0" = 3,
             "0 - 0.5" = 1, 
             "0.5 - 0.75" = 1, 
             "> 0.75"= 1)
  
  param_edges <- param_edges %>%
    mutate(class = val)%>%
    mutate(class = replace(class, val >= 0.75, "> 0.75")) %>%
    mutate(class = replace(class, val >= 0.5 & val< 0.75, "0.5 - 0.75")) %>%
    mutate(class = replace(class, val > 0 & val< 0.5, "0 - 0.5")) %>%
    mutate(class = replace(class, val >= -0.5 & val < 0, "0 - -0.5")) %>%
    mutate(class = replace(class, val >= -0.75 & val < -0.5, "-0.5 - -0.75")) %>%
    mutate(class = replace(class, val <= -0.75, "< -0.75")) %>%
    mutate(class = replace(class, val == 0, "0")) %>%
    mutate(class = factor(class, levels = c("> 0.75",
                                            "0.5 - 0.75",
                                            "0 - 0.5",
                                            "0",
                                            "0 - -0.5",
                                            "-0.5 - -0.75",
                                            "< -0.75"))) %>%
    mutate(sign = ifelse(val>0, "+","-")) %>%
    mutate(sign = replace(sign, val == 0, "ns"))%>%
    mutate(sign = replace(sign, is.na(val), "ns"))
  
  param_edges <- param_edges %>%
    mutate(hlab = "") %>%
    mutate(hlab = replace(hlab, to == "burn_sev" & from == "prefire_TVC", "H1"),
           hlab = replace(hlab, to == "Bromus_seeds_post" & from == "burn_sev", "H2"),
           hlab = replace(hlab, to == "postfire_TVC" & from == "Bromus_seeds_post", "H3"),
           hlab = replace(hlab, to == "Bromus_seeds_post" & from == "sb_div_pre", "H2a   "),
           hlab = replace(hlab, to == "sb_div_pre" & from == "Bromus_cv_pre", "H2a   "),
           hlab = replace(hlab, to == "sb_div_pre" & from == "prefire_TVC", "H2a   "))
  
  # Node properties
  param_nodes <- params %>% 
    filter(lhs == rhs) %>% 
    transmute(metric = lhs, e = est.std)
  
  # Complete Graph Object
  param_graph1 <- tidygraph::tbl_graph(param_nodes, 
                                       param_edges)
  # setting up the manual layout
  
  if(layout == "manual"){
    lut_x <- layout_df$x; names(lut_x) <- layout_df$metric
    lut_y <- layout_df$y; names(lut_y) <- layout_df$metric
  
    layout_man <- create_layout(param_graph1, layout = "linear") %>%
      mutate(x = lut_x[metric],
             y=lut_y[metric]) %>%
      dplyr::select(x,y) %>%
      as.data.frame()
    
    # applying the manual layout to the graph objects, one for each group
    layout1 <- create_layout(param_graph1, layout = layout_man)}else{
    layout1 <- create_layout(param_graph1, layout=layout)}
  
  p1_title <- title[1]
  

  # Plot
  p1 <- ggraph(layout1) +
    geom_edge_arc(aes(color=as.factor(sign), 
                      width = abs(val), 
                      label=hlab,
                      #linetype = as.factor(sign)
                      ), 
                  strength = 0.1,
                  angle_calc = "along", 
                  vjust = -.75,
                  # family = 'Times',
                  # fontface = "bold",
                  check_overlap = FALSE,
                  arrow = arrow(25, length = unit(0.3, "inches"), type = "open"),
                  label_colour = "grey20",
                  end_cap = circle(0.5, "inches"),
                  start_cap = circle(0.5, "inches")
    )+
    geom_node_text(aes(label = metric),fontface = "bold",#family = 'Times',  size = 10, #node names
                   nudge_y = 0.05) +
    scale_edge_color_manual(name = "Direction", 
                            values = c("#E41A1C", "#377EB8", "grey80"))+
    scale_edge_width(guide = "none", range = c(.5,2)) +
    theme_graph(fg_text_colour = 'white',
                base_family = 'Arial'#,base_family = 'Times'
                )+
    # ggtitle(p1_title) +
    scale_x_continuous(expand = c(0.095, 0.095))+
    # scale_y_continuous(expand=c(0.01, 0.01))+
    theme(#plot.title = element_text(size = 30),
          legend.position = "none",
          plot.background = element_rect(color="black", fill="transparent")
          )

  return(p1)
}


get_nodes <- function(fit){
  lavaan::standardizedsolution(fit) %>% 
    filter(lhs == rhs) %>% 
    transmute(metric = lhs, e = est.std)
}

random_layout <- function(fit){
  get_nodes(fit) %>%
    mutate(x=runif(nrow(.), min=-1, max=1),
           y=runif(nrow(.), min=-1, max=1)) %>%
    dplyr::select(-e) %>%
    as_tibble()
}
