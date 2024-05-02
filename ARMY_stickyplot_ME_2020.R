#==========================================
####STICKY PLOTS####
#The plots resemble violin and pirate plots but with an added feature: arrows that illustrate the changes in two data points 
#For example data points from time 1 to time 2.
#========================================== ME. April2020


#Install required packages
install.packages <- c("cowplot", "readr", "ggplot2", "dplyr", "lavaan", "smooth", "Hmisc", "tidyverse", "reshape2", "rio", "Rmisc", "purrr", "magrittr", "yarrr","tidyr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}


library(tidyverse)
library(dplyr)
library(rio)
library(reshape2)
library(ggplot2)
library(Rmisc)
library(purrr)
library(magrittr)
library(cowplot)
library(readr)
library(lavaan)
library(smooth)
library(Hmisc)
library(tidyr)
library(yarrr)


#Set your working directory and the path to your data file.
setwd("~/OneDrive - University of Florida/one drive/eye_tracker_Mina/excel files R/Dprime")
dprime <- read.csv("dprime_time_train.csv")


#Separate training (subordinate & Basic) and time (Pre & Post)
dprime_pre<-filter(dprime,time=="apre") 
dprime_post<-filter(dprime,time=="post") 
dprime_basic<-filter(dprime,training=="basic") 
dprime_sub<-filter(dprime,training=="sub") 


###For BASIC condition###
sticky_plot = function(dprime_basic){ #Create function, data = data of interest as determined by the create_data function(dprime_basic is my data name)
  jitter = runif(dim(dprime_basic)[1], -0.02, 0.02)} #Add custom jitter values
ggplot(aes(x=time,y=dprime),data=dprime_basic)+ #Create ggplot #here Time refers to Pre and Post tests
  geom_violin(aes(x=time,fill=as.factor(time)),alpha=.5,color='#9da0a3')+ #Create violin distributions
  geom_rect(mapping=aes(xmin=-0,xmax=0,ymin=min(dprime),ymax=max(dprime)),fill='white')+ #Cut distributions in half
  geom_line(aes(group=id),color='#747678',alpha=.8)+ #Create lines between points for each subject
  geom_point(color=c(rep('#f29838',1,dim(dprime_basic)[1]/2),rep('#3889f2',1,dim(dprime_basic)[1]/2)),alpha=.8)+ #Insert each participants data points
  stat_summary(aes(x=time,y=dprime),fun=mean, geom='line',color='black')+ #Create grand averaged effects line
  stat_summary(aes(x=time),fun=mean, geom='point',color='black')+ #Insert points for each conditions mean
  stat_summary(aes(x=time),fun.data=mean_cl_normal, geom='errorbar',width=.1,color='black')+ #Add errorbars to the means
  scale_fill_manual(values=c("#f29838", "#3889f2"),labels = c("Pre", "Post"))+ #Determine condition colours and names
  ylim(0, 1.6) +
  labs(x='',y='d')+ #Determine y axis label, remove x axis label
  theme_linedraw()+ #Remove grid background
  theme(legend.title = element_blank(), #Remove legend title
        text = element_text(size=12), #Determine font size
        axis.ticks = element_blank(), #Remove axis tick marks
        axis.line = element_line()) #Remove axis lines


###For SUBORDINATE condition###
sticky_plot = function(dprime_sub){ #Create function, data = data of interest as determined by the create_data function
  jitter = runif(dim(dprime_sub)[1], -0.02, 0.02)} #Add custom jitter values
ggplot(aes(x=time,y=dprime),data=dprime_sub)+ #Create ggplot
  geom_violin(aes(x=time,fill=as.factor(time)),alpha=.5,color='#9da0a3')+ #Create violin distributions
  geom_rect(mapping=aes(xmin=0,xmax=0,ymin=min(dprime),ymax=max(dprime)),fill='white')+ #Cut distributions in half
  geom_line(aes(group=id),color='#747678',alpha=.8)+ #Create lines between points for each subject
  geom_point(color=c(rep('#f29838',1,dim(dprime_sub)[1]/2),rep('#3889f2',1,dim(dprime_sub)[1]/2)),alpha=.8)+ #Insert each participants data points
  stat_summary(aes(x=time,y=dprime),fun=mean, geom='line',color='black')+ #Create grand averaged effects line
  stat_summary(aes(x=time),fun=mean, geom='point',color='black')+ #Insert points for each conditions mean
  stat_summary(aes(x=time),fun.data=mean_cl_normal, geom='errorbar',width=.1,color='black')+ #Add errorbars to the means
  scale_fill_manual(values=c("#f29838", "#3889f2"),labels = c("Pre", "Post"))+ #Determine condition colours and names
  ylim(0, 1.6) +
  theme_linedraw()+ #Remove grid background
  theme(legend.title = element_blank(), #Remove legend title
        text = element_text(size=12), #Determine font size
        axis.ticks = element_blank(), #Remove axis tick marks
        axis.line = element_line()) #Remove axis lines


###For training type in SUBORDINATE cordition###
#First, subset the data per training type 
dprime_trained_sub<-filter(dprime,condition_sub_main=="trained") 
dprime_untrained_sub<-filter(dprime,condition_sub_main=="untrained") 
dprime_new_sub<-filter(dprime,condition_sub_main=="znew") 

#Trained
sticky_plot = function(dprime_trained_sub){ #Create function, data = data of interest as determined by the create_data function
  jitter = runif(dim(dprime_trained_sub)[1], -0.02, 0.02)} #Add custom jitter values
ggplot(aes(x=time,y=dprime_sub_main),data=dprime_trained_sub)+ #Create ggplot
  geom_violin(aes(x=time,fill=as.factor(time)),alpha=.5,color='#9da0a3')+ #Create violin distributions
  geom_rect(mapping=aes(xmin=0,xmax=0,ymin=min(),ymax=max()),fill='white')+ #Cut distributions in half
  geom_line(aes(group=id),color='#747678',alpha=.8)+ #Create lines between points for each subject
  geom_point(color=c(rep('#f29838',1,dim(dprime_trained_sub)[1]/2),rep('#3889f2',1,dim(dprime_trained_sub)[1]/2)),alpha=.8)+ #Insert each participants data points
  stat_summary(aes(x=time,y=dprime_sub_main),fun=mean, geom='line',color='black')+ #Create grand averaged effects line
  stat_summary(aes(x=time),fun=mean, geom='point',color='black')+ #Insert points for each conditions mean
  stat_summary(aes(x=time),fun.data=mean_cl_normal, geom='errorbar',width=.1,color='black')+ #Add errorbars to the means
  scale_fill_manual(values=c("#f29838", "#3889f2"),labels = c("Pre", "Post"))+ #Determine condition colours and names
  ylim(0, 1.9) +
  labs(x='',y='d')+ #Determine y axis label, remove x axis label
  theme_linedraw()+ #Remove grid background
  theme(legend.title = element_blank(), #Remove legend title
        text = element_text(size=12), #Determine font size
        axis.ticks = element_blank(), #Remove axis tick marks
        axis.line = element_line()) #Remove axis lines

#Untrained
sticky_plot = function(dprime_untrained_sub){ #Create function, data = data of interest as determined by the create_data function
  jitter = runif(dim(dprime_untrained_sub)[1], -0.02, 0.02)} #Add custom jitter values
ggplot(aes(x=time,y=dprime_sub_main),data=dprime_untrained_sub)+ #Create ggplot
  geom_violin(aes(x=time,fill=as.factor(time)),alpha=.5,color='#9da0a3')+ #Create violin distributions
  geom_rect(mapping=aes(xmin=0,xmax=0,ymin=min(),ymax=max()),fill='white')+ #Cut distributions in half
  geom_line(aes(group=id),color='#747678',alpha=.8)+ #Create lines between points for each subject
  geom_point(color=c(rep('#f29838',1,dim(dprime_untrained_sub)[1]/2),rep('#3889f2',1,dim(dprime_untrained_sub)[1]/2)),alpha=.8)+ #Insert each participants data points
  stat_summary(aes(x=time,y=dprime_sub_main),fun=mean, geom='line',color='black')+ #Create grand averaged effects line
  stat_summary(aes(x=time),fun=mean, geom='point',color='black')+ #Insert points for each conditions mean
  stat_summary(aes(x=time),fun.data=mean_cl_normal, geom='errorbar',width=.1,color='black')+ #Add errorbars to the means
  scale_fill_manual(values=c("#f29838", "#3889f2"),labels = c("Pre", "Post"))+ #Determine condition colours and names
  ylim(0, 1.9) +
  labs(x='',y='d')+ #Determine y axis label, remove x axis label
  theme_linedraw()+ #Remove grid background
  theme(legend.title = element_blank(), #Remove legend title
        text = element_text(size=12), #Determine font size
        axis.ticks = element_blank(), #Remove axis tick marks
        axis.line = element_line()) #Remove axis lines

#New
sticky_plot = function(dprime_new_sub){ #Create function, data = data of interest as determined by the create_data function
  jitter = runif(dim(dprime_new_sub)[1], -0.02, 0.02)} #Add custom jitter values
ggplot(aes(x=time,y=dprime_sub_main),data=dprime_new_sub)+ #Create ggplot
  geom_violin(aes(x=time,fill=as.factor(time)),alpha=.5,color='#9da0a3')+ #Create violin distributions
  geom_rect(mapping=aes(xmin=0,xmax=0,ymin=min(),ymax=max()),fill='white')+ #Cut distributions in half
  geom_line(aes(group=id),color='#747678',alpha=.8)+ #Create lines between points for each subject
  geom_point(color=c(rep('#f29838',1,dim(dprime_new_sub)[1]/2),rep('#3889f2',1,dim(dprime_new_sub)[1]/2)),alpha=.8)+ #Insert each participants data points
  stat_summary(aes(x=time,y=dprime_sub_main),fun=mean, geom='line',color='black')+ #Create grand averaged effects line
  stat_summary(aes(x=time),fun=mean, geom='point',color='black')+ #Insert points for each conditions mean
  stat_summary(aes(x=time),fun.data=mean_cl_normal, geom='errorbar',width=.1,color='black')+ #Add errorbars to the means
  scale_fill_manual(values=c("#f29838", "#3889f2"),labels = c("Pre", "Post"))+ #Determine condition colours and names
  ylim(0, 1.9) +
  labs(x='',y='d')+ #Determine y axis label, remove x axis label
  theme_linedraw()+ #Remove grid background
  theme(legend.title = element_blank(), #Remove legend title
        text = element_text(size=12), #Determine font size
        axis.ticks = element_blank(), #Remove axis tick marks
        axis.line = element_line()) #Remove axis lines



###For training type in BASIC cordition###
#First, subset the data per training type 
dprime_trained_basic<-filter(dprime,condition_basic_main=="trained") 
dprime_untrained_basic<-filter(dprime,condition_basic_main=="untrained") 
dprime_new_basic<-filter(dprime,condition_basic_main=="znew") 

#Trained
sticky_plot = function(dprime_trained_basic){ #Create function, data = data of interest as determined by the create_data function
  jitter = runif(dim(dprime_trained_basic)[1], -0.02, 0.02)} #Add custom jitter values
ggplot(aes(x=time,y=basic_main),data=dprime_trained_basic)+ #Create ggplot
  geom_violin(aes(x=time,fill=as.factor(time)),alpha=.5,color='#9da0a3')+ #Create violin distributions
  geom_rect(mapping=aes(xmin=0,xmax=0,ymin=min(),ymax=max()),fill='white')+ #Cut distributions in half
  geom_line(aes(group=id),color='#747678',alpha=.8)+ #Create lines between points for each basicject
  geom_point(color=c(rep('#f29838',1,dim(dprime_trained_basic)[1]/2),rep('#3889f2',1,dim(dprime_trained_basic)[1]/2)),alpha=.8)+ #Insert each participants data points
  stat_summary(aes(x=time,y=basic_main),fun=mean, geom='line',color='black')+ #Create grand averaged effects line
  stat_summary(aes(x=time),fun=mean, geom='point',color='black')+ #Insert points for each conditions mean
  stat_summary(aes(x=time),fun.data=mean_cl_normal, geom='errorbar',width=.1,color='black')+ #Add errorbars to the means
  scale_fill_manual(values=c("#f29838", "#3889f2"),labels = c("Pre", "Post"))+ #Determine condition colours and names
  ylim(0, 1.9) +
  labs(x='',y='d')+ #Determine y axis label, remove x axis label
  theme_linedraw()+ #Remove grid background
  theme(legend.title = element_blank(), #Remove legend title
        text = element_text(size=12), #Determine font size
        axis.ticks = element_blank(), #Remove axis tick marks
        axis.line = element_line()) #Remove axis lines

#Untrained
sticky_plot = function(dprime_untrained_basic){ #Create function, data = data of interest as determined by the create_data function
  jitter = runif(dim(dprime_untrained_basic)[1], -0.02, 0.02)} #Add custom jitter values
ggplot(aes(x=time,y=basic_main),data=dprime_untrained_basic)+ #Create ggplot
  geom_violin(aes(x=time,fill=as.factor(time)),alpha=.5,color='#9da0a3')+ #Create violin distributions
  geom_rect(mapping=aes(xmin=0,xmax=0,ymin=min(),ymax=max()),fill='white')+ #Cut distributions in half
  geom_line(aes(group=id),color='#747678',alpha=.8)+ #Create lines between points for each basicject
  geom_point(color=c(rep('#f29838',1,dim(dprime_untrained_basic)[1]/2),rep('#3889f2',1,dim(dprime_untrained_basic)[1]/2)),alpha=.8)+ #Insert each participants data points
  stat_summary(aes(x=time,y=basic_main),fun=mean, geom='line',color='black')+ #Create grand averaged effects line
  stat_summary(aes(x=time),fun=mean, geom='point',color='black')+ #Insert points for each conditions mean
  stat_summary(aes(x=time),fun.data=mean_cl_normal, geom='errorbar',width=.1,color='black')+ #Add errorbars to the means
  scale_fill_manual(values=c("#f29838", "#3889f2"),labels = c("Pre", "Post"))+ #Determine condition colours and names
  ylim(0, 1.9) +
  labs(x='',y='d')+ #Determine y axis label, remove x axis label
  theme_linedraw()+ #Remove grid background
  theme(legend.title = element_blank(), #Remove legend title
        text = element_text(size=12), #Determine font size
        axis.ticks = element_blank(), #Remove axis tick marks
        axis.line = element_line()) #Remove axis lines

#New
sticky_plot = function(dprime_new_basic){ #Create function, data = data of interest as determined by the create_data function
  jitter = runif(dim(dprime_new_basic)[1], -0.02, 0.02)} #Add custom jitter values
ggplot(aes(x=time,y=basic_main),data=dprime_new_basic)+ #Create ggplot
  geom_violin(aes(x=time,fill=as.factor(time)),alpha=.5,color='#9da0a3')+ #Create violin distributions
  geom_rect(mapping=aes(xmin=0,xmax=0,ymin=min(),ymax=max()),fill='white')+ #Cut distributions in half
  geom_line(aes(group=id),color='#747678',alpha=.8)+ #Create lines between points for each basicject
  geom_point(color=c(rep('#f29838',1,dim(dprime_new_basic)[1]/2),rep('#3889f2',1,dim(dprime_new_basic)[1]/2)),alpha=.8)+ #Insert each participants data points
  stat_summary(aes(x=time,y=basic_main),fun=mean, geom='line',color='black')+ #Create grand averaged effects line
  stat_summary(aes(x=time),fun=mean, geom='point',color='black')+ #Insert points for each conditions mean
  stat_summary(aes(x=time),fun.data=mean_cl_normal, geom='errorbar',width=.1,color='black')+ #Add errorbars to the means
  scale_fill_manual(values=c("#f29838", "#3889f2"),labels = c("Pre", "Post"))+ #Determine condition colours and names
  ylim(0, 1.9) +
  labs(x='',y='d')+ #Determine y axis label, remove x axis label
  theme_linedraw()+ #Remove grid background
  theme(legend.title = element_blank(), #Remove legend title
        text = element_text(size=12), #Determine font size
        axis.ticks = element_blank(), #Remove axis tick marks
        axis.line = element_line()) #Remove axis lines


geom_point(color=c(rep('#767C81',1,dim(dprime_sub)[1]/2),rep('#8C9196',1,dim(dprime_sub)[1]/2)),alpha=.8)+ #Insert each participants data points
  stat_summary(aes(x=time,y=dprime),fun=mean, geom='line',color='black')+ #Create grand averaged effects line
  stat_summary(aes(x=time),fun=mean, geom='point',color='black')+ #Insert points for each conditions mean
  stat_summary(aes(x=time),fun.data=mean_cl_normal, geom='errorbar',width=.1,color='black')+ #Add errorbars to the means
  scale_fill_manual(values=c("#767C81", "#C4C7CA"),labels = c("Pre", "Post"))+ #Determine condition colours and names 
  
  
  



