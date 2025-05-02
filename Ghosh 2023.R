
Dragonfly=c(Dragonfly_n,Dragonfly_s,Dragonfly_d)
Damselfly=c(Damselfly_n,Damselfly_s,Damselfly_d)
Homoptera=c(Homoptera_n,Homoptera_s,Homoptera_d)
Bee=c(Bee_n,Bee_s,Bee_d)
Wasp=c(Wasp_n,Wasp_s,Wasp_d)
Ant=c(Ant_n,Ant_s,Ant_d)
Orthoptera=c(Orthoptera_n,Orthoptera_s,Orthoptera_d)
nest=c(nest_n,nest_s,nest_d)
nesting=c(rep("Noachhar",100),rep("Sadhugunj",100),rep("Dinhata",100))
######### histogram
n_r=lm(nesting~Dragonfly+Damselfly+Homoptera+Bee+Wasp+Ant+Orthoptera)
a_r=anova(n_r)
a_r
df1=cbind.data.frame(nesting,Dragonfly,Damselfly,Homoptera,Bee,Wasp,Ant,Orthoptera)


boxplot(nest~Dragonfly+Damselfly+Homoptera+Bee+Wasp+Ant+Orthoptera,
        data = df1)

colnames(df1)=c("Nesting","Dragonfly","Damselfly","Homoptera","Bee","Wasp","Ant","Orthoptera")
head(df1)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)


df2<- df1 %>% 
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0)) %>%
  filter(text %in% names(df1))



df2 %>%
  mutate(text = fct_reorder(text, value)) %>% # Reorder data
  ggplot( aes(x=text, y=value, fill=text, color=text)) +
  geom_violin(width=2.1, size=0.2) +
  theme_ipsum() +
  theme(
    legend.position="none"
  ) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Assigned Probability (%)")

p

library(ggstatsplot)
library(palmerpenguins)
library(tidyverse)
data("penguins", package = "palmerpenguins")
penguins <- drop_na(penguins)
head(penguins)

plt_Dragonfly<- ggbetweenstats(
  data = df1,
  x = Nesting,
  y = Dragonfly
)

plt_Damselfly<- ggbetweenstats(
  data = df1,
  x = Nesting,
  y = Damselfly
)

plt_Homoptera<- ggbetweenstats(
  data = df1,
  x = Nesting,
  y = Homoptera
)

plt_Bee<- ggbetweenstats(
  data = df1,
  x = Nesting,
  y = Bee
)

plt_Wasp<- ggbetweenstats(
  data = df1,
  x = Nesting,
  y = Wasp
)

plt_Ant<- ggbetweenstats(
  data = df1,
  x = Nesting,
  y =Ant
)

plt_Orthoptera<- ggbetweenstats(
  data = df1,
  x = Nesting,
  y = Orthoptera
)




p_Dragonfly <- plt_Dragonfly + 
  # Add labels and title
  labs(
    x = "Nesting sites",
    y = "Dragonfly",
    title = "Distribution of Dragonfly in pellets"
  ) + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 20, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto", 
      size = 20, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 20, color = "black"),
    axis.title = element_text(size = 20)
  )
p_Dragonfly

p_Damselfly <- plt_Damselfly + 
  # Add labels and title
  labs(
    x = "Nesting sites",
    y = "Damselfly",
    title = "Distribution of Damselfly in pellets"
  ) + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 20, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto", 
      size = 20, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 20, color = "black"),
    axis.title = element_text(size = 20)
  )
p_Damselfly



p_Homoptera <- plt_Homoptera + 
  # Add labels and title
  labs(
    x = "Nesting sites",
    y = "Homoptera",
    title = "Distribution of Homoptera in pellets"
  ) + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 20, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto", 
      size = 20, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 20, color = "black"),
    axis.title = element_text(size = 20)
  )
p_Homoptera

p_Bee <- plt_Bee + 
  # Add labels and title
  labs(
    x = "Nesting sites",
    y = "Bee",
    title = "Distribution of Bee in pellets"
  ) + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 20, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto", 
      size = 20, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 20, color = "black"),
    axis.title = element_text(size = 20)
  )
p_Bee


p_Wasp <- plt_Wasp + 
  # Add labels and title
  labs(
    x = "Nesting sites",
    y = "Wasp",
    title = "Distribution of Wasp in pellets"
  ) + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 20, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto", 
      size = 20, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 20, color = "black"),
    axis.title = element_text(size = 20)
  )
p_Wasp


p_Ant <- plt_Ant + 
  # Add labels and title
  labs(
    x = "Nesting sites",
    y = "Ant",
    title = "Distribution of Ant in pellets"
  ) + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 20, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto", 
      size = 20, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 20, color = "black"),
    axis.title = element_text(size = 20)
  )
p_Ant

p_Orthoptera <- plt_Orthoptera + 
  # Add labels and title
  labs(
    x = "Nesting sites",
    y = "Orthoptera",
    title = "Distribution of Orthoptera in pellets"
  ) + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 20, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto", 
      size = 20, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 20, color = "black"),
    axis.title = element_text(size = 20)
  )
p_Orthoptera
