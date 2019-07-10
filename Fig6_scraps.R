####################### 
# Code below deciphers location of points on graph for labels and is not necessary to run. 
# There is also some draft ggplots in here. 

which(Ecosci_text$Country1 == "Turkmenistan")
which(Ecosci_text$Country1 == "St. Lucia")

which(Ecosci_text3$Country1 == "United States")
# Ecosci_text3[135,]
# annotate(geom="text", x = 13.21, y=	3.59, label = "United States")
which(Ecosci_text3$Country1 == "China")
# Ecosci_text3[28,]
# annotate(geom="text", x = 12.85, y=	2.62, label = "China")
which(Ecosci_text3$Country1 == "Japan")
# Ecosci_text3[64,]
# annotate(geom="text", x = 12.74, y=	2.41, label = "Japan")
which(Ecosci_text3$Country1 == "Germany")
# Ecosci_text3[47,]
# annotate(geom="text", x = 12.57, y=	2.90, label = "Germany")
which(Ecosci_text3$Country1 == "Canada")
# Ecosci_text3[25,]
# annotate(geom="text", x = 12.21, y=	3.05, label = "Canada")
which(Ecosci_text3$Country1 == "Australia")
# Ecosci_text3[7,]
# annotate(geom="text", x = 12.07, y=	3.19, label = "Australia")
which(Ecosci_text3$Country1 == "United Kingdom")
# Ecosci_text3[133,]
# annotate(geom="text", x = 12.44, y=	2.81, label = "United Kingdom")
which(Ecosci_text3$Country1 == "Brazil")
# Ecosci_text3[19,]
# annotate(geom="text", x = 12.30, y=	2.72, label = "Brazil")
which(Ecosci_text3$Country1 == "India")
# Ecosci_text3[57,]
# annotate(geom="text", x = 12.21, y=	2.48, label = "India")
which(Ecosci_text3$Country1 == "Russia")
# Ecosci_text3[108,]
# annotate(geom="text", x = 12.21, y=	2.09, label = "Russian Fed'n")
which(Ecosci_text3$Country1 == "Palau")
# Ecosci_text3[99,]
# annotate(geom="text", x = 8.30, y=	1, label = "Palau")
which(Ecosci_text3$Country1 == "Maldives")
# Ecosci_text3[78,]
# annotate(geom="text", x = 9.33, y=	0.30, label = "Maldives")
which(Ecosci_text3$Country1 == "New Zealand")
# Ecosci_text3[92,]
# annotate(geom="text", x = 11.175, y=	2.66, label = "New Zealand")
which(Ecosci_text3$Country1 == "Costa Rica")
# Ecosci_text3[31,]
# annotate(geom="text", x = 10.57, y=	2.43, label = "Costa Rica")
which(Ecosci_text3$Country1 == "Sierra Leone")
# Ecosci_text3[115,]
# annotate(geom="text", x = 9.52, y=	2.276, label = "Sierra Leone")
which(Ecosci_text3$Country1 == "Guinea")
# Ecosci_text3[51,]
# annotate(geom="text", x = 9.667, y=	2.096, label = "Guinea")
which(Ecosci_text3$Country1 == "Armenia")
# Ecosci_text3[6,]
# annotate(geom="text", x = 10.02, y=	0.60, label = "Armenia")
which(Ecosci_text3$Country1 == "Albania")
# Ecosci_text3[2,]
# annotate(geom="text", x = 10.09, y=	0.48, label = "Albania")

which(Ecosci_text3$Country1 == "Zimbabwe")
# Ecosci_text3[140,]
# annotate(geom="text", x = 10.00, y=	1.785, label = "Zimbabwe")
which(Ecosci_text3$Country1 == "Uganda")
# Ecosci_text3[130,]
# annotate(geom="text", x = 10.31, y=	1.81, label = "Uganda")
which(Ecosci_text3$Country1 == "Nauru")
# Ecosci_text3[89,]
# annotate(geom="text", x = 8.02, y=	0, label = "Nauru")
which(Ecosci_text3$Country1 == "Dominican Republic")
# Ecosci_text3[36,]
# annotate(geom="text", x = 10.73, y=	0, label = "Dominican Republic")

which(Ecosci_text3$Country1 == "Uzbekistan")
# Ecosci_text3[137,]
# annotate(geom="text", x = 10.66, y=	031, label = "Uzbekistan")

which(Ecosci_text$Country1 == "St. Lucia")
# Ecosci_text[186,]
# annotate(geom="text", x = 9.09, y=	2.15, label = "St. Lucia")

which(Ecosci_text$Country1 == "Turkmenistan")
# Ecosci_text[224,]
# annotate(geom="text", x = 10.41, y=	0.3, label = "Turkmenistan")

ggplot(data = Ecosci_text3) +
  geom_point(mapping = aes(x = L10AveGNI0715, y = L10TotalCnt, color = 5)) + 
  geom_point(mapping = aes(x = LogGNP8795, y = L10pub8795, color = 7))

ggplot(data = Ecosci_text3) +
  geom_point(mapping = aes(x = L10AveGNI0715, y = L10TotalCnt), color = "blue") + 
  geom_point(mapping = aes(x = LogGNP8795, y = L10pub8795), color = "red")

gg <- ggplot(data = Ecosci_text3, aes(x = L10AveGNI0715, y = L10TotalCnt)) +
  geom_point(aes(color = L10AveGNI0715)) +
  geom_smooth(method="lm", col="firebrick", size=2)  
gg


#Example ggplot
# gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
#     geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
#     geom_smooth(method="lm", col="firebrick", size=2) + 
#     coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
#     labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

#   gg <- gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01))

#   gg + annotate(geom="text", x=3, y=30, label="Scatter plot",
#                  color="red")

