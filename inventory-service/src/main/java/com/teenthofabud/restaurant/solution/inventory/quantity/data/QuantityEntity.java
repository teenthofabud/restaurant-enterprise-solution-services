package com.teenthofabud.restaurant.solution.inventory.quantity.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductEntity;
import lombok.*;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Entity
@Table(name = "quantity")
@EntityListeners(AuditingEntityListener.class)
public class QuantityEntity extends TOABBaseEntity implements Comparable<QuantityEntity> {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    @Column(name = "weight_id")
    private String weightId;
    private Double amount;
    @ManyToOne(targetEntity = ProductEntity.class, fetch = FetchType.LAZY, cascade = CascadeType.MERGE)
    @JoinColumn(name = "inventory_product_id")
    private ProductEntity product;

    @Override
    public int compareTo(QuantityEntity o) {
        return this.getId().compareTo(o.getId());
    }
}
