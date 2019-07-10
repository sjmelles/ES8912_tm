# code for figure 6
library(Rcmdr) # needed for readXL; there are probably other excel readers available.
library(ggplot2)

Ecosci_text <- readXL("C:/Steph_2019/in_prep/Ecoscience/Ecoscience_drafts/Figures/Final/Manuscript_Fig_Oct_2018/Submitted_Fig_datas/FINAL_8795_0715_combo.xlsx", rownames=FALSE, header=TRUE, na="", sheet="FINAL_8795_0715_combo", stringsAsFactors=TRUE)

# first get GNP ave back on original scale
Ecosci_text$GNP0715_Orig <- (10^(Ecosci_text$L10GNP0715_scaled)*1000000)
# transform back to log10 scale
Ecosci_text$L10GNP0715_Orig <- log10(Ecosci_text$GNP0715_Orig)

Ecosci_text$L10Pop2015 <- log10(Ecosci_text$Population.2015)

MLR <- lm(Ecosci_text$PapsL10_0715 ~ Ecosci_text$L10Pop2015 + Ecosci_text$L10GNP0715_Orig)
summary(MLR)
which(Ecosci_text$Country1 == "Turkmenistan")
which(Ecosci_text$Country1 == "St. Lucia")
Ecosci_text2 <- Ecosci_text[-225,]
Ecosci_text2 <- Ecosci_text2[-187,]
which(Ecosci_text2$Country1 == "Turkmenistan")
which(Ecosci_text2$Country1 == "St. Lucia")

MLR2 <- lm(Ecosci_text2$PapsL10_0715 ~ Ecosci_text2$L10Pop2015 + Ecosci_text2$L10GNP0715_Orig)
summary(MLR2)

# residuals 
Ecosci_text3 <- readXL("C:/Steph_2019/in_prep/Ecoscience/Ecoscience_drafts/Figures/Final/Manuscript_Fig_Oct_2018/Submitted_Fig_datas/FINAL_8795_0715_combo.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Pop", stringsAsFactors=TRUE)

Step1 <- lm(Ecosci_text3$PapsL10_0715 ~ Ecosci_text3$L10Pop2015)
summary(Step1)

Step2 <- lm(Step1$residuals ~ Ecosci_text3$L10GNP0715_Orig)
summary(Step2)

ggplot(data = Ecosci_text) +
     geom_point(mapping = aes(x = L10GNP0715_Orig, y = PapsL10_0715, color = 5)) + 
     geom_point(mapping = aes(x = LogGNP8795, y = L10pub8795, color = 7))

ggplot(data = Ecosci_text) +
  geom_point(mapping = aes(x = L10GNP0715_Orig, y = PapsL10_0715), color = "blue") + 
  geom_point(mapping = aes(x = LogGNP8795, y = L10pub8795), color = "red")

gg <- ggplot(data = Ecosci_text, aes(x = L10GNP0715_Orig, y = PapsL10_0715)) +
  geom_point(aes(color = L10GNP0715_Orig)) +
  geom_smooth(method="lm", col="firebrick", size=2)  
gg


gg <- ggplot(data = Ecosci_text) +
  geom_point(aes(x = L10GNP0715_Orig, y = PapsL10_0715), color = "blue") +
  geom_smooth(aes(x = L10GNP0715_Orig, y = PapsL10_0715), method="lm", col="blue", size=2) + 
  geom_point(mapping = aes(x = LogGNP8795, y = L10pub8795), color = "firebrick") +
  geom_smooth(aes(x = LogGNP8795, y = L10pub8795), method="lm", col="firebrick", size=2) + 
  labs(y="Log10 Publication output", x="Log10 Gross Domestic Product (red) and Gross National Income (blue)") +  
  annotate(geom="text", x = 6.75, y=	3, label = "United States") +
  annotate(geom="text", x = 12.9, y=	3.55, label = "United States") +
  annotate(geom="text", x = 13.3, y=	2.64, label = "China") +
  annotate(geom="text", x = 13.2, y=	2.4, label = "Japan") +
  annotate(geom="text", x = 13, y=	3, label = "Germany") +
  annotate(geom="text", x = 12.6, y=	3.1, label = "Canada") +
  annotate(geom="text", x = 12.6, y=	3.25, label = "Australia") +
  annotate(geom="text", x = 12.8, y=	2.85, label = "UK") +
  annotate(geom="text", x = 8.45, y=	2.2, label = "St. Lucia") +
  annotate(geom="text", x = 11.5, y=	0, label = "Turkmenistan") +
  annotate(geom="text", x = 7.9, y=	1, label = "Palau") +
  annotate(geom="text", x = 11.35, y=	0.5, label = "Lybia") +
  annotate(geom="text", x = 11.5, y=	0.3, label = "Uzbekistan") +
  annotate(geom="text", x = 7.5, y=	0, label = "Nauru") + 
  annotate(geom="text", x = 8.4, y=	-0.1, label = "Kiribati") +
  annotate(geom="text", x = 9.2, y=	0.1, label = "Maldives") +
  annotate(geom="text", x = 12.8, y=	2, label = "Russian Fed'n") +
  annotate(geom="text", x = 12.4, y=	1.4, label = "Turkey") +
  annotate(geom="text", x = 12.68, y=	1.1, label = "United Arab Em's") +
  annotate(geom="text", x = 12.6, y=	1.0, label = "Saudi Arabia") +
  annotate(geom="text", x = 11.5, y=	0.8, label = "Iraq") +
  annotate(geom="text", x = 10.0, y=	2.5, label = "Costa Rica") +
  annotate(geom="text", x = 10.3, y=	2.7, label = "New Zealand") +
  annotate(geom="text", x = 6.35, y=	2.8, label = "UK") +
  annotate(geom="text", x = 6.15, y=	2.3, label = "Australia") +
  annotate(geom="text", x = 6.38, y=	2, label = "Canada") +
  annotate(geom="text", x = 6.85, y=	1.35, label = "Germany") + 
  annotate(geom="text", x = 7, y=	1, label = "Japan") +
  annotate(geom="text", x = 6.1, y=	0.85, label = "China") +
  annotate(geom="text", x = 6.2, y=	0.5, label = "USSR") +
  annotate(geom="text", x = 3.7, y=	1.75, label = "New Zealand") +
  annotate(geom="text", x = 4, y=	1.15, label = "Kenya") +
  annotate(geom="text", x = 3, y=	0.8, label = "Tanzania") +
  annotate(geom="text", x = 4.1, y=	2, label = "South Africa") +
  annotate(geom="text", x = 9.2, y=	0.1, label = "") +
  annotate(geom="text", x = 9.2, y=	0.1, label = "") 
gg

####################### 
# Code below deciphers which points were which and is not necessary to run

# which(Ecosci_text$Country1 == "United States")
# which(Ecosci_text$Country1 == "China")
#Ecosci_text[30,] 2.624282, 12.84744
# annotate(geom="text", x = 13.2, y=	2.64, label = "China")

# which(Ecosci_text$Country1 == "Japan")
#Ecosci_text[89,] 2.369216, 12.73991
# annotate(geom="text", x = 13.1, y=	2.4, label = "Japan")

# which(Ecosci_text$Country1 == "Germany")
#Ecosci_text[72,] 2.91698, 12.56675
# annotate(geom="text", x = 13, y=	3, label = "Germany")

# which(Ecosci_text$Country1 == "Canada")
# annotate(geom="text", x = 12.20947, y=	3.049993, label = "Canada")

# which(Ecosci_text$Country1 == "France")
# annotate(geom="text", x = 12.44664, y=	2.822168, label = "France")

# which(Ecosci_text$Country1 == "United Kingdom")
# annotate(geom="text", x = 12.44093, y=	2.833784, label = "United Kingdom")

# which(Ecosci_text$Country1 == "Brazil")
# which(Ecosci_text$Country1 == "India")
# which(Ecosci_text$Country1 == "Russian Federation")
# which(Ecosci_text$Country1 == "St. Lucia")
# which(Ecosci_text$Country1 == "Palau")
# which(Ecosci_text$Country1 == "Turkmenistan")
# which(Ecosci_text$Country1 == "Tonga")
# which(Ecosci_text$Country1 == "Tunisia")
# which(Ecosci_text$Country1 == "Turkey")
# which(Ecosci_text$Country1 == "Tanzania")
# which(Ecosci_text$Country1 == "Uganda")
# which(Ecosci_text$Country1 == "Ukraine")
# which(Ecosci_text$Country1 == "Uruguay")
# which(Ecosci_text$Country1 == "Uzbekistan")
# which(Ecosci_text$Country1 == "Vietnam")
# which(Ecosci_text$Country1 == "Vietnam")
# which(Ecosci_text$Country1 == "Vanuatu")
# which(Ecosci_text$Country1 == "Samoa")
# which(Ecosci_text$Country1 == "Zambia")
# which(Ecosci_text$Country1 == "Zimbabwe")

LM0715 <- lm(PapsL10_0715 ~ L10GNP0715_Orig, data = Ecosci_text)

LM8795 <- lm(L10pub8795 ~ LogGNP8795, data = Ecosci_text)    



#Example ggplot
# gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
#     geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
#     geom_smooth(method="lm", col="firebrick", size=2) + 
#     coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
#     labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")
  
#   gg <- gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01))
  
#   gg + annotate(geom="text", x=3, y=30, label="Scatter plot",
#                  color="red")
  
