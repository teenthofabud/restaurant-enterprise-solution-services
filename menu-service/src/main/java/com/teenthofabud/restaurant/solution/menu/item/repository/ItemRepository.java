package com.teenthofabud.restaurant.solution.menu.item.repository;

import com.teenthofabud.core.common.repository.TOABSimpleEntityBaseRepository;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;

@Repository
public interface ItemRepository extends TOABSimpleEntityBaseRepository<ItemEntity> {

    @Lock(LockModeType.PESSIMISTIC_READ)
    List<ItemEntity> findByCategoryId(Long categoryId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public ItemEntity save(ItemEntity entity);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Boolean existsByNameAndCategoryId(String name, Long categoryId);
}
