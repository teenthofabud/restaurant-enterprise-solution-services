package com.teenthofabud.restaurant.solution.inventory.quantity.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductVo;
import lombok.*;

import javax.measure.Unit;
import javax.measure.quantity.Mass;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class QuantityVo extends TOABBaseVo implements Comparable<QuantityVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private Double amount;
    @ToString.Include
    private String weightId;
    @ToString.Include
    private String productId;
    @ToString.Include
    private ProductVo product;
    @ToString.Include
    private UnitVo weight;

    @Override
    public int compareTo(QuantityVo o) {
        return Integer.compare(this.getProductId().compareTo(o.getProductId()), this.getWeightId().compareTo(o.getWeightId()));
    }
}
