###ggplot2
library(ggplot2)
library(ggrepel)
library(grid)
library(ggthemes)
#ggplot2()
data("midwest")
str(midwest)
midwest_sub = midwest[midwest$poptotal > 40000,]
midwest_sub$large_country = ifelse(midwest_sub$poptotal > 40000, midwest_sub$county, '')
####  choose the dataset  choose the aes
my_grob = grid.text(
  'This text is at x=0.7,and y=0.9!',
  x = 0.7,
  y = 0.9,
  gp = gpar(
    col = 'firebrick',
    fontsize = 10,
    fontface = 'bold'
  )
)
g = ggplot(midwest, aes(x = popamerindian, y = popasian)) +
  geom_point(aes(col = state, size = popdensity, shape = category)) +
  geom_text(aes(label = large_country), size = 2, data = midwest_sub) +
  geom_smooth(method = 'lm', col = 'firebrick') +
  
  xlim(c(0, 5000)) +
  ylim(c(0, 4000)) +
  scale_color_brewer(palette = 2) +
  labs(
    title = "this is the title ",
    y = 'Poplulation',
    x = "Area",
    subtitle = 'this is the subtitle',
    caption = 'this is the caption'
  ) + theme_gdocs() + annotation_custom(my_grob)+geom_label_repel(aes(label=large_country,data=midwest_sub))

plot(g)
#
#
# ggplot(mtcars, aes(x = cyl, y = gear)) +
#   geom_point(aes(col = gear)) +
#   geom_smooth(method = 'lm')

cars
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_text(label = cars$speed) +
  geom_point(aes(size = speed, col = 'firebrick')) +
  labs(
    title = "this is the title",
    subtitle = 'thisi is the subtitle',
    caption = 'this is the caption',
    x = 'this is the x',
    y = 'this is the y'
  ) + annotation_custom(grid.text(
    'This is one added annotation',
    x = 0.7,
    y = 0.9,
    gp = gpar(
      col = 'blue',
      fontsize = 14,
      fontface = 'bold'
    )
  ))+
theme_gdocs()

#####
###geom_jitter used to handle overlap
### or use the geom_count to count how many data in each point
str(mpg)
data(mpg)
mpg$class=as.factor(mpg$class)
ggplot(mpg,aes(x=cty,y=hwy))+
  geom_smooth(method='lm')+
  theme_gdocs()+geom_count(col='steelblue')
ggplot(mpg,aes(manufacturer))+geom_bar(width=.5,fill='steelblue')+facet_grid(~class)
ggplot(mpg,aes(manufacturer))+geom_bar(width=.5,aes(fill=class))+theme_bw()
###get the percentage of them
ggplot(mpg,aes(manufacturer))+geom_bar(width=.5,aes(fill=class),position = "fill")+theme_bw()
data("economics")
ggplot(economics,aes(x=date))+
  geom_line(aes(y=psavert,col='psavert'))+
  geom_line(aes(y=uempmed,col='uempmed'))+
  theme_gdocs()














