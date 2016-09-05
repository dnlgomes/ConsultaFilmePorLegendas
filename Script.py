from os import listdir
from os.path import isfile, join


mypath = "Documents/Rec Inf/Dois/Legendas/"
onlyfiles = [f for f in listdir(mypath) if isfile(join(mypath, f))]

for file in onlyfiles:
    saida = open("Documents/Rec Inf/Dois/LegendasTexto" + file, 'w')
    f = open(mypath + file, "r")
    count = 2
    for line in f:
        if line.isdigit():
            count = 1
            continue

        if "<font color=" in line:
            continue

        if count < 1:
            saida.write(line)
        count -= 1



