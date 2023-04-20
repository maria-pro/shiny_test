test<-employ%>%
  filter(var=="Sheep and Beef cattle")%>%
  pivot_wider(
    names_from=state,
    values_from = value
  )%>%
  mutate(
    offset=base-zero
  )


test%>%
  plot_ly(
    x=var,
    y=~offset,
    frame=~year,
    #y=~value,
    #color=~state,
    #colors = colors,
    #hoverinfo = "text",
    type = 'bar',
    width = 0.5,
    textposition = "none",
    showlegend = FALSE)%>%
  layout(yaxis = list(title = 'Business as usual$ - Net zero$'),
              xaxis= list(title = ~var)
  )


employ_group<-employ%>%
  filter(industry_group=="Sector 1 - Agriculture, forestry and fishing (A)")

employ_group%>%
  plot_ly(
    x=~year,
    y=~value,
    frame=~year,
    shape=~industry_group,
    #y=~value,
    color=~var,
    size=~value,
    #colors = colors,
    #hoverinfo = "text",
  #  type = 'marker',
    #  width = 0.5,
    #  textposition = "none",
    showlegend = FALSE)%>%
  layout(yaxis = list(title = 'Effect in $'),
         xaxis= list(title = "Year")
  )
