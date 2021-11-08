package com.teenthofabud.restaurant.solution.customer.integration.metadata.gender.data;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class GenderVo implements Comparable<GenderVo> {

    private String id;
    @ToString.Include
    private String name;
    @ToString.Include
    private Boolean active;

    @Override
    public int compareTo(GenderVo o) {
        return this.id.compareTo(o.getId());
    }
}
