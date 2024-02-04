

class Snake:
    def __init__(self):
        self.body = [(0, 0)]
        self.direction = 'd'


    def get_head(self):
        return self.body[-1]


    def move(self, new_head):
        self.body.append(new_head)

    
    def remove_tail(self):
        self.body.pop(0)

    