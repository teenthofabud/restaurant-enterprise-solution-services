create table if not exists menu_category (
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
create index if not exists idx_menu_category_name on menu_category(name);

create table if not exists menu_item (
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
    constraint fk_menu_item_menu_category foreign key (menu_category_id) references menu_category(id),
    constraint uq_menu_item_menu_category_id_name unique (menu_category_id, name)
);
create index if not exists idx_menu_item_name on menu_item(name);