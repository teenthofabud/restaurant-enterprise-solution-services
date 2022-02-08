package com.teenthofabud.restaurant.solution.customer.address.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.CityVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.CountryVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.StateVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class AddressVo extends TOABBaseVo implements Comparable<AddressVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String name;
    @ToString.Include
    private String addressLine1;
    @ToString.Include
    private String addressLine2;
    @ToString.Include
    private String cityId;
    @ToString.Include
    private CityVo city;
    @ToString.Include
    private String stateId;
    @ToString.Include
    private StateVo state;
    @ToString.Include
    private String pincode;
    @ToString.Include
    private String countryId;
    @ToString.Include
    private CountryVo country;
    @ToString.Include
    private String accountId;
    @ToString.Include
    private AccountVo account;

    @Override
    public int compareTo(AddressVo o) {
        return Integer.compare(this.getAccount() != null ? this.getAccount().compareTo(o.getAccount()) : this.getAccountId().compareTo(o.getAccountId()),
                this.getId().compareTo(o.getId()));
    }
}
