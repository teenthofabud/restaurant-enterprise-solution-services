package com.teenthofabud.restaurant.solution.cookbook.cuisine.repository;

import com.teenthofabud.core.common.repository.TOABSimpleEntityBaseRepository;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;

@Repository
public interface CuisineRepository extends TOABSimpleEntityBaseRepository<CuisineEntity> {

    @Lock(LockModeType.PESSIMISTIC_READ)
    List<CuisineEntity> findByName(String name);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public CuisineEntity save(CuisineEntity entity);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Boolean existsByName(String name);
}
