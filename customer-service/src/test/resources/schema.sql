create table if not exists account (
    id identity not null,
    first_name varchar(50) not null,
    last_name varchar(50) not null,
    gender_id varchar(100),
    date_of_birth date,
    country_code varchar(10) not null,
    phone_number varchar(50) not null,
    email_id varchar(150),
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_customer_account primary key (id),
    constraint uq_customer_account_phone_number unique (phone_number)
);
create index if not exists idx_customer_account_name on account(first_name, last_name);
create index if not exists idx_customer_account_gender_id on account(gender_id);
create index if not exists idx_customer_account_phone_number on account(phone_number);
create index if not exists idx_customer_account_country_code on account(country_code);
create index if not exists idx_customer_account_email_id on account(email_id);
alter table account alter column id restart with 1;

create table if not exists address (
    id identity not null,
    name varchar(50) default 'default',
    customer_account_id int not null,
    address_line_1 varchar(100) not null,
    address_line_2 varchar(100),
    city_id  varchar(10) not null,
    state_id varchar(10) not null,
    pincode varchar(50) not null,
    country_id varchar(10) not null,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_customer_address primary key (id),
    constraint fk_customer_address_customer_account_id foreign key (customer_account_id) references account(id),
    constraint uq_customer_address_name_customer_account_id unique (name, customer_account_id)
);
create index if not exists idx_customer_address_pincode on address(pincode);
create index if not exists idx_customer_address_city_id on address(city_id);
create index if not exists idx_customer_address_state_id on address(state_id);
create index if not exists idx_customer_address_country_id on address(country_id);
create index if not exists idx_customer_address_name on address(name);
alter table address alter column id restart with 1;
