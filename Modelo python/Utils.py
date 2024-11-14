import os
path = os.path.join(os.getcwd(),"Modelo python")
path1 = os.path.join(path,"change.txt")
path2 = os.path.join(path,"change_complete.txt")

with open(path1,'r') as file:
    list = file.readlines()
    file.close()

for item in list:
    item = item.replace("                                       ","")
    item=item.replace("(","{")
    item=item.replace(")","},")
    item=item.replace(" ",",")
    with open(path2,'a') as file:
        file.write(item)


# Chasing cars 
# snow algo