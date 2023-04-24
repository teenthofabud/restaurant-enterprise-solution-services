package com.teenthofabud.restaurant.solution.cookbook.recipe.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineEntity;
import lombok.*;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Entity
@Table(name = "recipe")
@EntityListeners(AuditingEntityListener.class)
public class RecipeEntity extends TOABBaseEntity implements Comparable<RecipeEntity> {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private String name;
    private String description;
    private String instructions;
    @Column(name = "number_of_servings")
    private Integer numberOfServings;
    @Column(name = "menu_item_id")
    private String itemId;
    @Column(name = "preparation_time_duration")
    private Double preparationTimeDuration;
    @Column(name = "preparation_time_unit_id")
    private String preparationTimeUnitId;
    @Column(name = "cooking_time_duration")
    private Double cookingTimeDuration;
    @Column(name = "cooking_time_unit_id")
    private String cookingTimeUnitId;
    @Column(name = "portion_size_amount")
    private Double portionSizeAmount;
    @Column(name = "portion_size_unit_id")
    private String portionSizeUnitId;
    @Column(name = "cooking_method")
    private String cookingMethod;
    @ManyToOne(fetch = FetchType.LAZY,cascade = CascadeType.MERGE)
    @JoinColumn(name = "cookbook_cuisine_id")
    private CuisineEntity cuisine;

    @Override
    public int compareTo(RecipeEntity o) {
        return this.getId().compareTo(o.getId());
    }
}
