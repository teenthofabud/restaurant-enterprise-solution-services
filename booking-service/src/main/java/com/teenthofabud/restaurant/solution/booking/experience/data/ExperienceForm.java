package com.teenthofabud.restaurant.solution.booking.experience.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;


@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ExperienceForm {

    @ToString.Include
    private String name;
    @ToString.Include
    private String description;

}
