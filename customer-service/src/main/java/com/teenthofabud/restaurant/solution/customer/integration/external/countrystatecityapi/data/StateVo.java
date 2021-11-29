package com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.data;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public class StateVo implements Comparable<StateVo> {

    private String id;
    @ToString.Include
    private String name;
    @ToString.Include
    private String iso2;

    @Override
    public int compareTo(StateVo o) {
        return this.iso2.compareTo(o.getIso2());
    }
}
