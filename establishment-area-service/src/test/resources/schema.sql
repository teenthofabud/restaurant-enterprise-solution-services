create table if not exists area_floor (
    id identity not null,
    name varchar(50) not null,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw bigint default true,
    version int default 0,
    constraint pk_area_floor primary key (id),
    constraint uq_area_floor_name unique (name)
);
create index if not exists idx_area_floor_name on area_floor(name);
alter table area_floor alter column id restart with 1;

create table if not exists area_kitchen (
    id identity not null,
    name varchar(50) not null,
    description varchar(200),
	floor_id int not null,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw bigint default true,
    version int default 0,
    constraint pk_area_kitchen primary key (id),
    constraint fk_area_kitchen_floor_id foreign key (floor_id) REFERENCES area_floor(id),
    constraint uq_area_kitchen_name_floor_id unique (name, floor_id)
);
create index if not exists idx_area_kitchen_name on area_kitchen(name);
alter table area_kitchen alter column id restart with 1;

create table if not exists area_table (
    id identity not null,
    name varchar(50) not null,
    description varchar(200),
	floor_id int not null,
	capacity int default 0,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw bigint default true,
    version int default 0,
    constraint pk_area_table primary key (id),
    constraint fk_area_table_floor_id foreign key (floor_id) REFERENCES area_floor(id),
    constraint uq_area_table_name_floor_id unique (name, floor_id)
    );
create index if not exists idx_area_table_name on area_table(name);
alter table area_table alter column id restart with 1;