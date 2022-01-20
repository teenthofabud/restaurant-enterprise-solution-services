package com.teenthofabud.restaurant.solution.cookbook.ingredient.repository;

import com.teenthofabud.core.common.repository.TOABSimpleEntityBaseRepository;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;

@Repository
public interface IngredientRepository extends TOABSimpleEntityBaseRepository<IngredientEntity> {

    @Lock(LockModeType.PESSIMISTIC_READ)
    List<IngredientEntity> findByRecipeId(Long recipeId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public IngredientEntity save(IngredientEntity entity);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Boolean existsByNameAndRecipeIdAndProductId(String name, Long recipeId, String productId);
}
