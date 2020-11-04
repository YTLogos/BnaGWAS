# -*- coding: utf-8 -*-
"""
Created on Wed Nov  4 16:54:21 2020

@author: taoyan
"""
import sys
input=open(sys.argv[1],"r")
output=open(sys.argv[2],"w")
for line in input:
    if line[0]=="S":
        output.write(line)
    if line[0]!="S":
        l=line.strip("\n").split("\t")
        if l[1]=="1":
            l[1]="A1"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="2":
            l[1]="A2"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="3":
            l[1]="A3"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="4":
            l[1]="A4"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="5":
            l[1]="A5"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="6":
            l[1]="A6"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="7":
            l[1]="A7"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="8":
            l[1]="A8"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="9":
            l[1]="A9"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="10":
            l[1]="A10"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="11":
            l[1]="C1"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="12":
            l[1]="C2"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="13":
            l[1]="C3"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="14":
            l[1]="C4"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="15":
            l[1]="C5"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="16":
            l[1]="C6"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="17":
            l[1]="C7"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="18":
            l[1]="C8"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="19":
            l[1]="C9"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="20":
            l[1]="An"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
        if l[1]=="21":
            l[1]="Cn"
            l[0]=l[1]+"_"+l[2]
            a=' '.join(l)
            output.write(a+"\n")
    
input.close()
output.close()

