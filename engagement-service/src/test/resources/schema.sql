create table if not exists check_in (
    id identity not null,
    customer_account_id varchar(100) not null,
    sequence varchar(200) not null,
    type varchar(100) not null,
    number_of_persons int not null,
    notes varchar(200),
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_engagement_check_in primary key (id),
    constraint uq_engagement_check_in_customer_account_id_sequence_created_on unique (customer_account_id, sequence, type, created_on)
);
create index if not exists idx_engagement_check_in_customer_account_id on check_in(customer_account_id);
create index if not exists idx_engagement_check_in_sequence on check_in(sequence);
create index if not exists idx_engagement_check_in_type on check_in(type);
alter table check_in alter column id restart with 1;

create table if not exists walk_in (
    id identity not null,
    engagement_check_in_id int not null,
    name varchar(100) not null,
    phone_number varchar(50),
    email_id varchar(100),
    constraint pk_engagement_walk_in primary key (id),
    constraint fk_engagement_walk_in_check_in foreign key (engagement_check_in_id) references check_in(id)
);
create index if not exists idx_engagement_walk_in_name on walk_in(name);
alter table walk_in alter column id restart with 1;

create table if not exists reservation (
    id identity not null,
    engagement_check_in_id int not null,
    date date not null,
    time time not null,
    constraint pk_engagement_reservation primary key (id),
    constraint fk_engagement_reservation_check_in foreign key (engagement_check_in_id) references check_in(id)
);
create index if not exists idx_engagement_reservation_date_time on reservation(date, time);
alter table reservation alter column id restart with 1;

create table if not exists table_allocation (
    id identity not null,
    engagement_check_in_id int not null,
    establishmentarea_table_id varchar(100) not null,
    notes varchar(200),
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_engagement_table_allocation primary key (id),
    constraint fk_engagement_table_allocation_check_in foreign key (engagement_check_in_id) references check_in(id),
    constraint uq_engagement_table_allocation_table_id_sequence_created_on unique (engagement_check_in_id, establishmentarea_table_id, created_on)
);
create index if not exists idx_engagement_table_allocation_establishmentarea_table_id on table_allocation(establishmentarea_table_id);
alter table check_in alter column id restart with 1;
