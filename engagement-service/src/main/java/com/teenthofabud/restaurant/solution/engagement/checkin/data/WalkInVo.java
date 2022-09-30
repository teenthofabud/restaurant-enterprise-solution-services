package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class WalkInVo implements CheckInVoParameters {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String name;
    @EqualsAndHashCode.Include
    @ToString.Include
    private String phoneNumber;
    @EqualsAndHashCode.Include
    @ToString.Include
    private String emailId;

}
