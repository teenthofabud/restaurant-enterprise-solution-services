package com.teenthofabud.restaurant.solution.inventory.quantity.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TypeModelVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class UnitVo extends TypeModelVo {

    private String symbol;

    public UnitVo(String name) {
        super.setName(name);
    }

}
