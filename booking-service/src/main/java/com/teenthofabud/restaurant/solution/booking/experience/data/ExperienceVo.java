package com.teenthofabud.restaurant.solution.booking.experience.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class ExperienceVo extends TOABBaseVo implements Comparable<ExperienceVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;

    @Override
    public int compareTo(ExperienceVo o) {
        return this.getName().compareTo(o.getName());
    }
}
