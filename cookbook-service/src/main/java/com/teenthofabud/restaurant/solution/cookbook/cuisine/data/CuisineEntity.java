package com.teenthofabud.restaurant.solution.cookbook.cuisine.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
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
@Table(name = "cuisine")
@EntityListeners(AuditingEntityListener.class)
public class CuisineEntity extends TOABBaseEntity implements Comparable<CuisineEntity> {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private String name;
    private String description;
    @OneToMany(mappedBy = "cuisine", fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    private List<RecipeEntity> recipes;

    @Override
    public int compareTo(CuisineEntity o) {
        return this.getId().compareTo(o.getId());
    }
}
