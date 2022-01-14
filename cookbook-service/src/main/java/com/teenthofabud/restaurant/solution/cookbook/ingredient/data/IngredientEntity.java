package com.teenthofabud.restaurant.solution.cookbook.ingredient.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Entity
@Table(name = "ingredient")
@EntityListeners(AuditingEntityListener.class)
public class IngredientEntity extends TOABBaseEntity implements Comparable<IngredientEntity> {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private String name;
    private String description;
    @Column(name = "inventory_product_id")
    private String productId;
    @Column(name = "quantity_amount")
    private Double quantityAmount;
    @Column(name = "quantity_unit_id")
    private String quantityUnitId;
    @ManyToOne(fetch = FetchType.LAZY,cascade = CascadeType.MERGE)
    @JoinColumn(name = "cookbook_recipe_id")
    private RecipeEntity recipe;

    @Override
    public int compareTo(IngredientEntity o) {
        return this.getId().compareTo(o.getId());
    }
}
