import numpy as np 

class graph:
    def __init__(self,matrix=np.zeros((1,1)),routes=[[1]]):
        self.clients=[]
        self.bridges=[]
        self.routes=[]
        self.weight=matrix
        self.route_cost={}
        self.c={}
        self.S={}
        self.K={}
        self.L={}

        self.__add_route__(routes)
        self.__calc_constant__()

    def print_graph(self):
        for i in self.bridges:
            print(str(i[0]) + " -> " + str(i[1]) )
        print("Weight: " +str(sum(self.route_cost.values())))

    def __add_route__(self,routes:list[list[int]]):
        for route in routes:
            cost=0
            path=[]
            client=0
            for item in route:
                if item not in self.clients: self.clients.append(item)
                path.append((client,item))
                cost+=self.weight[client,item]
                self.bridges.append((client,item))
                client=item
            path.append((client,0))
            cost+=self.weight[client,0]
            self.bridges.append((client,0))
            self.route_cost[(len(self.routes))]=cost
            self.routes.append(path)

    def __calc_constant__(self):

        for route in range(len(self.routes)):
            for i,j in enumerate(self.routes[route]):
                self.c[(route,i)]=self.weight[j[0],j[1]]

        for route in range(len(self.routes)):
            for j in range(len(self.routes[route])):
                for i in range(len(self.routes[route])):
                    if j-i <=1:break
                    sum=0
                    for k in range(len(self.routes[route])):
                        if k>=j: break
                        elif k<=i: continue
                        else:
                            bridge=self.routes[route][k]
                            sum+=self.weight[bridge[0],bridge[1]]
                    self.S[(route,i,j)]=sum

        for route1 in range(len(self.routes)):
            for i in range(len(self.routes[route1])):
                for route2 in range(len(self.routes)):
                    for j in range(len(self.routes[route2])):
                        bridge1=self.routes[route1][i]
                        bridge2=self.routes[route2][j]
                        self.K[route1,i,route2,j]=self.weight[bridge2[0],bridge1[1]]

        for route1 in range(len(self.routes)):
            for i in range(len(self.routes[route1])):
                for route2 in range(len(self.routes)):
                    for j in range(len(self.routes[route2])):
                        bridge1=self.routes[route1][i]
                        bridge2=self.routes[route2][j]
                        self.L[route1,i,route2,j]=self.weight[bridge1[0],bridge2[1]]




















matrix_cost=np.matrix([[0,5,4,5],
                       [5,0,6,5],
                       [4,6,0,1],
                       [5,5,1,0]])

list=[[1,2,3]]

grraph=graph(matrix_cost,list)
grraph.print_graph()

# Esta 0->1->2->3->0

# Solucion 0->2->3->1->0 
# X_00103

matrix_cost=np.matrix([[0,5,4,5,1,4],
                       [5,0,6,5,2,1],
                       [4,6,0,1,3,5],
                       [5,5,1,0,6,7],
                       [1,2,3,6,0,8],
                       [4,1,5,7,8,0]])

list=[[2,3,1],
      [4,5]]

#grraph=graph(matrix_cost,list)


grraph.print_graph()

# 0->1->2->0
# 0->4->5->3->0

# 0->1->4->5->2->0
# 0->3->0
# 52854 24 
# Solucion 
# 0->1->2->3->0
# 0->4->5->0
# X(00102)


