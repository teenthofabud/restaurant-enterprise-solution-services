package com.teenthofabud.restaurant.solution.menu.price.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryEntity;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemEntity;
import lombok.*;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Entity
@Table(name = "menu_item_price")
@EntityListeners(AuditingEntityListener.class)
public class PriceEntity extends TOABBaseEntity implements Comparable<PriceEntity> {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    @Column(name = "currency_id")
    private String currencyId;
    private Double amount;
    @ManyToOne(targetEntity = ItemEntity.class, fetch = FetchType.LAZY, cascade = CascadeType.MERGE)
    @JoinColumn(name = "menu_item_id")
    private ItemEntity item;

    @Override
    public int compareTo(PriceEntity o) {
        return this.getId().compareTo(o.getId());
    }
}
