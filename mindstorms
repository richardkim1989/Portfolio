import turtle

window = turtle.Screen()
window.bgcolor("black")

#draw squares
square = turtle.Turtle()
square.shape("classic")
square.color("white")
square.speed(15)

def one_square(move):
    for i in range(4):
        move.forward(100)
        move.right(90)

def more_squares():
    for i in range(36):
        one_square(square)
        square.right(10)
more_squares()

#draw circles
circle = turtle.Turtle()
circle.shape("classic")
circle.color("blue")
circle.speed(15)

def one_circle(move):
    circle.circle(100)

def more_circles():
    for i in range(36):
        one_circle(circle)
        circle.right(10)
more_circles()

#draw triangles
triangle = turtle.Turtle()
triangle.shape("classic")
triangle.color("red")
triangle.speed(15)

for i in range(10):
    triangle.left(180)
    triangle.forward(100)
    triangle.left(90)
    triangle.forward(100)
    triangle.left(135)
    triangle.forward(140)
    triangle.right(10)

window.exitonclick()
