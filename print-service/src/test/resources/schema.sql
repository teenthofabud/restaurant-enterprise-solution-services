create table if not exists print_template (
    id identity not null,
    name varchar(50) not null,
    description varchar(200),
    template_type_id varchar(128) not null,
    content varchar(255) not null,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_print_template primary key (id),
    constraint uq_print_template_name_template_type_id unique (name, template_type_id)
);
create index if not exists idx_print_template_name on print_template(name);
create index if not exists idx_print_template_template_type_id on print_template(template_type_id);