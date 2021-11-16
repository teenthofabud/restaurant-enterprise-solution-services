package com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.NON_NULL)
public class StateVo implements Comparable<StateVo> {

    private String id;
    @ToString.Include
    private String name;
    @ToString.Include
    @JsonProperty("country_id")
    private String countryId;
    @ToString.Include
    private String iso2;
    @ToString.Include
    @JsonProperty("country_code")
    private String countryCode;

    @Override
    public int compareTo(StateVo o) {
        return this.iso2.compareTo(o.getIso2());
    }
}
