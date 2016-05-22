#!/usr/bin/env python

import sys
#print sys.argv

f = open('/home/martin/Downloads/'+sys.argv[1],'r')

s = f.readline()  
#print(s)

t = s.replace('.', '0')
#print (t)


news = ""
for c in t:
    news = news + c + " "

news = news[:-1] # remove last character
#print(news)
final= ''
for i in range(9):
    
    final = final + news[i*18:i*18 +18] + '\n' 
    
#print(final)

g = open('/home/martin/Downloads/sud.txt','w')
g.write(final)
g.close()
f.close()
