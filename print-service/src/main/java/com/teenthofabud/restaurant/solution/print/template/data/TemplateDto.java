package com.teenthofabud.restaurant.solution.print.template.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class TemplateDto {

    @ToString.Include
    private Optional<String> name;
    @ToString.Include
    private Optional<String> description;
    @ToString.Include
    private Optional<String> templateTypeId;
    @ToString.Include
    private Optional<String> content;
    @ToString.Include
    private Optional<String> active;

    public TemplateDto() {
        this.name = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
        this.description = Optional.ofNullable(null);
        this.templateTypeId = Optional.ofNullable(null);
        this.content = Optional.ofNullable(null);
    }

}
