

-- Create the hosts table
create table hosts (
    id INT primary key,
    name varchar(255) not null,
    email varchar(255) not null
);

-- Create the listings table
create table listings (
    id INT primary key,
    host_id INT not null,
    title varchar(255) not null,
    description text,
    price DECIMAL(10, 2) not null,
    foreign key (host_id) references hosts(id)
);

-- Insert sample data into the hosts table
insert into hosts (id, name, email) values
(1, 'John Doe', 'john.doe@example.com'),
(2, 'Jane Smith', 'jane.smith@example.com');

-- Insert sample data into the listings table
insert into listings (id, host_id, title, description, price) values
(1, 1, 'Cozy Cottage', 'A cozy cottage in the countryside', 100.00),
(2, 1, 'Modern Apartment', 'A modern apartment in the city center', 150.00),
(3, 2, 'Beach House', 'A beautiful beach house with ocean views', 200.00);

select * from hosts;

select
    listings.id as listing_id,
    listings.title,
    listings.description,
    listings.price,
    hosts.id as host_id,
    hosts.name,
    hosts.email
from
    listings
join
    hosts on listings.host_id = hosts.id
where
    hosts.id = 1;


-- End of file


