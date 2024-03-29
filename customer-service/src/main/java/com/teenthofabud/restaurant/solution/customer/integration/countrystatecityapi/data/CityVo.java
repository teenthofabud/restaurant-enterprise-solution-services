package com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public class CityVo implements Comparable<CityVo> {

    private Long id;
    @ToString.Include
    private String name;

    @Override
    public int compareTo(CityVo o) {
        return this.id.compareTo(o.getId());
    }
}
