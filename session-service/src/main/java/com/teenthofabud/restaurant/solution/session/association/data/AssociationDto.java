package com.teenthofabud.restaurant.solution.session.association.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class AssociationDto {

    @ToString.Include
    private Optional<String> experienceId;
    @ToString.Include
    private Optional<String> tableId;
    @ToString.Include
    private Optional<String> accountId;
    /*@ToString.Include
    private Optional<String> endedOn;*/
    @ToString.Include
    private Optional<String> active;

    public AssociationDto() {
        this.experienceId = Optional.ofNullable(null);
        this.tableId = Optional.ofNullable(null);
        this.accountId = Optional.ofNullable(null);
        //this.endedOn = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

}
