create table if not exists meeting (
    id identity not null,
    account_id varchar(100) not null,
    sequence varchar(200) not null,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_encounter_meeting primary key (id),
    constraint uq_encounter_meeting_account_id_sequence unique (account_id, sequence)
);
create index if not exists idx_encounter_meeting_account_id on meeting(account_id);
create index if not exists idx_encounter_meeting_sequence on meeting(sequence);
alter table meeting alter column id restart with 1;

create table if not exists pick_up (
    id identity not null,
    encounter_meeting_id int not null,
    name varchar(100) not null,
    phone_number varchar(200) not null,
    constraint pk_encounter_pick_up primary key (id)
);
create index if not exists idx_encounter_pick_up_name on pick_up(name);
create index if not exists idx_engagement_pick_up_phone_number on pick_up(phone_number);
alter table pick_up alter column id restart with 1;

create table if not exists delivery (
    id identity not null,
    encounter_meeting_id int not null,
    order_id varchar(100) not null,
    constraint pk_encounter_delivery primary key (id)
);
create index if not exists idx_encounter_delivery_order_id on delivery(order_id);
alter table delivery alter column id restart with 1;