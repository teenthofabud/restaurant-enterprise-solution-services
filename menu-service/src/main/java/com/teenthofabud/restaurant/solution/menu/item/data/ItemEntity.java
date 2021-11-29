package com.teenthofabud.restaurant.solution.menu.item.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryEntity;
import lombok.*;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Entity
@Table(name = "menu_item")
@EntityListeners(AuditingEntityListener.class)
public class ItemEntity extends TOABBaseEntity implements Comparable<ItemEntity> {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private String name;
    private String description;
    @Column(name = "is_vegeterian")
    private Boolean isVegeterian;
    @Column(name = "image_url")
    private String imageUrl;
    @ManyToOne(targetEntity = CategoryEntity.class, fetch = FetchType.LAZY, cascade = CascadeType.MERGE)
    @JoinColumn(name = "menu_category_id")
    private CategoryEntity category;

    @Override
    public int compareTo(ItemEntity o) {
        return this.getId().compareTo(o.getId());
    }
}
