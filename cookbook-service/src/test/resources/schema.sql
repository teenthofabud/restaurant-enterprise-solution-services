create table if not exists cuisine (
    id identity not null,
    name varchar(50) not null,
    description varchar(50) not null,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_cookbook_cuisine primary key (id),
    constraint uq_cookbook_cuisine_name unique (name)
);
create index if not exists idx_cookbook_cuisine_name on cuisine(name);
alter table cuisine alter column id restart with 1;

create table if not exists recipe (
    id identity not null,
    cookbook_cuisine_id int not null,
    menu_item_id varchar(100) not null,
    name varchar(50) not null,
    description varchar(200),
    preparation_time_duration double not null,
    preparation_time_unit_id varchar(50) not null,
    cooking_time_duration double not null,
    cooking_time_unit_id varchar(50) not null,
    instructions varchar(255) not null,
    cooking_method varchar(150) not null,
    number_of_servings int not null,
    portion_size_amount double not null,
    portion_size_unit_id varchar(50) not null,
    created_on timestamp default current_timestamp,
    created_by int not null,
    modified_on timestamp default current_timestamp,
    modified_by int not null,
    active_sw bigint default 1,
    version int default 0,
    constraint pk_cookbook_recipe primary key (id),
    constraint fk_cookbook_recipe_cookbook_cuisine foreign key (cookbook_cuisine_id) references cuisine(id),
    constraint uq_cookbook_recipe_name_cookbook_cuisine_id_menu_item_id unique (name, cookbook_cuisine_id, menu_item_id)
);
create index if not exists idx_cookbook_recipe_menu_item_id on recipe(menu_item_id);
create index if not exists idx_cookbook_recipe_name on recipe(name);
create index if not exists idx_cookbook_recipe_number_of_servings on recipe(number_of_servings);
create index if not exists idx_cookbook_recipe_cooking_method on recipe(cooking_method);
alter table recipe alter column id restart with 1;

create table if not exists ingredient (
    id int AUTO_INCREMENT,
    cookbook_recipe_id int not null,
    inventory_product_id int not null,
    name varchar(50) not null,
    description varchar(200),
    quantity_amount double not null,
    quantity_unit_id varchar(50) not null,
    created_on timestamp default current_timestamp,
    created_by int not null,
    modified_on timestamp default current_timestamp,
    modified_by int not null,
    active_sw bigint default 1,
    version int default 0,
    constraint pk_cookbook_ingredient primary key (id),
    constraint fk_cookbook_ingredient_cookbook_recipe foreign key (cookbook_recipe_id) references recipe(id),
    constraint uq_cookbook_ingredient_name_cookbook_recipe_id_inventory_product_id unique (name, cookbook_recipe_id, inventory_product_id)
);
create index if not exists idx_cookbook_ingredient_inventory_product_id on ingredient(inventory_product_id);
create index if not exists idx_cookbook_ingredient_name on ingredient(name);
alter table ingredient alter column id restart with 1;
