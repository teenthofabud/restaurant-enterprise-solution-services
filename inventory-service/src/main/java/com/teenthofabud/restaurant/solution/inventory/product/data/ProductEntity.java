package com.teenthofabud.restaurant.solution.inventory.product.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryEntity;
import lombok.*;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Entity
@Table(name = "product")
@EntityListeners(AuditingEntityListener.class)
public class ProductEntity extends TOABBaseEntity implements Comparable<ProductEntity> {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private String name;
    private String description;
    @Column(name = "image_url")
    private String imageUrl;
    @ManyToOne(targetEntity = CategoryEntity.class, fetch = FetchType.LAZY, cascade = CascadeType.MERGE)
    @JoinColumn(name = "inventory_category_id")
    private CategoryEntity category;

    @Override
    public int compareTo(ProductEntity o) {
        return this.getId().compareTo(o.getId());
    }
}
