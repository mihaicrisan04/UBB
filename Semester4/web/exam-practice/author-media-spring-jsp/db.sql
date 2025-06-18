create DATABASE store_web_exam;
use store_web_exam;

create TABLE User (
    id int AUTO_INCREMENT PRIMARY KEY,
    username VARCHAR(255) not null
)

create TABLE Product (
    id int AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255),
    price int not null
)

create table Orderr (
    id int AUTO_INCREMENT PRIMARY KEY,
    userId int not null,
    totalPrice int not null
)

create table OrderItem (
    id int AUTO_INCREMENT PRIMARY KEY,
    orderId int not null,
    productId int not null
)

INSERT into User (username) VALUES (
    ('mihai'),
    ('ana')
);


insert into Product (name, price) value
('BOOK-Math', 200),
    ('TOY-car', 100);

insert into Product (name, price) values
('BOOK-book2', 250),
('BOOK-book3', 300),
('TOY-car2', 150),
('TOY-car3', 200),
('TOY-car4', 250),

