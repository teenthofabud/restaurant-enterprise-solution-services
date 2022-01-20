create table if not exists category (
    id identity not null,
    name varchar(50) not null,
    description varchar(200),
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_inventory_category primary key (id),
    constraint uq_inventory_category_name unique (name)
);
create index if not exists idx_inventory_category_name on category(name);
alter table category alter column id restart with 1;

create table if not exists product (
    id identity not null,
    inventory_category_id int not null,
    name varchar(50) not null,
    description varchar(200),
    image_url varchar(255),
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_inventory_product primary key (id),
    constraint fk_inventory_product_inventory_category foreign key (inventory_category_id) references category(id),
    constraint uq_inventory_product_inventory_category_id_name unique (inventory_category_id, name)
);
create index if not exists idx_inventory_product_name on product(name);
alter table product alter column id restart with 1;

create table if not exists quantity (
    id identity not null,
    inventory_product_id int not null,
    weight_id varchar(50) not null,
    amount double not null,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_inventory_quantity primary key (id),
    constraint fk_inventory_quantity_inventory_product foreign key (inventory_product_id) references product(id)
);
create index if not exists idx_inventory_quantity_weight_id on quantity(weight_id);
alter table quantity alter column id restart with 1;