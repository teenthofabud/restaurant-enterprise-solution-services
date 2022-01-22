package com.teenthofabud.restaurant.solution.session.association.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class AssociationForm {

    @ToString.Include
    private String experienceId;
    @ToString.Include
    private String tableId;
    @ToString.Include
    private String accountId;
    /*@ToString.Include
    private String endedOn;*/

}
