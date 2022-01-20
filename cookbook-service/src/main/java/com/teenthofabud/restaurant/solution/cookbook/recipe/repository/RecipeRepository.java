package com.teenthofabud.restaurant.solution.cookbook.recipe.repository;

import com.teenthofabud.core.common.repository.TOABSimpleEntityBaseRepository;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;

@Repository
public interface RecipeRepository extends TOABSimpleEntityBaseRepository<RecipeEntity> {

    @Lock(LockModeType.PESSIMISTIC_READ)
    List<RecipeEntity> findByCuisineId(Long cuisineId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public RecipeEntity save(RecipeEntity entity);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Boolean existsByNameAndCuisineIdAndItemId(String name, Long cuisineId, String itemId);
}
