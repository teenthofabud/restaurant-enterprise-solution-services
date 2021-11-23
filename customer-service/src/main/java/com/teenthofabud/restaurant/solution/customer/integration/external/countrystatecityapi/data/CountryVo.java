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
public class CountryVo implements Comparable<CountryVo> {

    private Long id;
    @ToString.Include
    private String name;
    @ToString.Include
    private String iso3;
    @ToString.Include
    private String iso2;
    @ToString.Include
    @JsonProperty("phonecode")
    private String phoneCode;
    @ToString.Include
    private String currency;

    @Override
    public int compareTo(CountryVo o) {
        return this.iso2.compareTo(o.getIso2());
    }
}
