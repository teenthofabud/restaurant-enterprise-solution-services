create table if not exists customer_account (
    id identity not null,
    first_name varchar2(50) not null,
    last_name varchar2(50) not null,
    gender_id int,
    date_of_birth date,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_customer_account primary key (id)
);
create index if not exists idx_customer_account_name on customer_account(first_name, last_name);
create index if not exists idx_customer_gender_id on customer_account(gender_id);

create table if not exists customer_contact (
    id identity not null,
    name varchar2(50) default 'default',
    customer_account_id int not null,
    phone_number varchar2(50) not null,
    country_code varchar2(10) not null,
    email_id varchar2(150),
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_customer_contact primary key (id),
    constraint fk_customer_contact_customer_account_id foreign key (customer_account_id) references customer_account(id),
    constraint uq_customer_contact_phone_number unique (phone_number)
);
create index if not exists idx_customer_contact_phone_number on customer_contact(phone_number);
create index if not exists idx_customer_contact_country_code on customer_contact(country_code);
create index if not exists idx_customer_contact_email_id on customer_contact(email_id);

create table if not exists customer_address (
    id identity not null,
    name varchar2(50) default 'default',
    customer_account_id int not null,
    address_line_1 varchar2(100) not null,
    address_line_2 varchar2(100),
    city varchar2(50) not null,
    state varchar2(50) not null,
    pincode varchar2(50) not null,
    country_id int not null,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_customer_address primary key (id),
    constraint fk_customer_address_customer_account_id foreign key (customer_account_id) references customer_account(id)
);
create index if not exists idx_customer_address_pincode on customer_address(pincode);
create index if not exists idx_customer_address_city on customer_address(city);
create index if not exists idx_customer_address_country_id on customer_address(country_id);
