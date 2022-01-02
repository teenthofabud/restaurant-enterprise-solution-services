package com.teenthofabud.restaurant.solution.inventory.category.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductEntity;
import lombok.*;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Entity
@Table(name = "category")
@EntityListeners(AuditingEntityListener.class)
public class CategoryEntity extends TOABBaseEntity implements Comparable<CategoryEntity> {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private String name;
    private String description;
    @OneToMany(mappedBy = "category", fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    private List<ProductEntity> products;

    @Override
    public int compareTo(CategoryEntity o) {
        return this.getId().compareTo(o.getId());
    }
}
