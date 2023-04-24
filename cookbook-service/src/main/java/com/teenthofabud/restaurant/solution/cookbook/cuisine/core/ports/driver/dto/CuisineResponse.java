package com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driver.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import lombok.*;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class CuisineResponse extends TOABBaseVo implements Comparable<CuisineResponse> {

    @ToString.Include
    private String id;
    @EqualsAndHashCode.Include
    @ToString.Include
    private String name;
    @EqualsAndHashCode.Include
    @ToString.Include
    private String description;
    @Override
    public int compareTo(CuisineResponse o) {
        return Integer.compare(this.getName().compareTo(o.getName()), this.getDescription().compareTo(o.getDescription()));
    }
}
