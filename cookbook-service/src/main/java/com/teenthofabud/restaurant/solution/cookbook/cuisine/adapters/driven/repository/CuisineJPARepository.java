package com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.repository;

import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;

@Repository
public interface CuisineJPARepository extends JpaRepository<CuisineEntity, Long> {

    @Lock(LockModeType.PESSIMISTIC_READ)
    List<CuisineEntity> findByName(String name);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public CuisineEntity save(CuisineEntity entity);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Boolean existsByName(String name);
}
