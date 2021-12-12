package com.teenthofabud.restaurant.solution.menu.price.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemVo;
import lombok.*;

import java.util.Currency;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class PriceVo extends TOABBaseVo implements Comparable<PriceVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private Double amount;
    @ToString.Include
    private String currencyId;
    @ToString.Include
    private String itemId;
    @ToString.Include
    private ItemVo item;
    @ToString.Include
    private Currency currency;

    @Override
    public int compareTo(PriceVo o) {
        return Integer.compare(this.getItemId().compareTo(o.getItemId()), this.getCurrencyId().compareTo(o.getCurrencyId()));
    }
}
