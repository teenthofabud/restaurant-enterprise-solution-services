package com.teenthofabud.restaurant.solution.menu.category.repository;

import com.teenthofabud.core.common.repository.TOABSimpleEntityBaseRepository;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;

@Repository
public interface CategoryRepository extends TOABSimpleEntityBaseRepository<CategoryEntity> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public CategoryEntity save(CategoryEntity entity);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Boolean existsByName(String name);
}
