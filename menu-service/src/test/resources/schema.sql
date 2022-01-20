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
    constraint pk_menu_category primary key (id),
    constraint uq_menu_category_name unique (name)
);
create index if not exists idx_menu_category_name on category(name);
alter table category alter column id restart with 1;

create table if not exists item (
    id identity not null,
    menu_category_id int not null,
    name varchar(50) not null,
    description varchar(200),
    is_vegeterian bigint not null,
    image_url varchar(255),
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_menu_item primary key (id),
    constraint fk_menu_item_menu_category foreign key (menu_category_id) references category(id),
    constraint uq_menu_item_menu_category_id_name unique (menu_category_id, name)
);
create index if not exists idx_menu_item_name on item(name);
alter table item alter column id restart with 1;

create table if not exists price (
    id identity not null,
    menu_item_id int not null,
    currency_id varchar(50) not null,
    amount double not null,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_menu_price primary key (id),
    constraint fk_menu_price_menu_item foreign key (menu_item_id) references item(id)
);
create index if not exists idx_menu_price_currency_id on price(currency_id);
alter table price alter column id restart with 1;