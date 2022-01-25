package com.teenthofabud.restaurant.solution.establishmentarea.floor.repository;

import com.teenthofabud.core.common.repository.TOABSimpleEntityBaseRepository;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;

@Repository
public interface FloorRepository extends TOABSimpleEntityBaseRepository<FloorEntity> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public FloorEntity save(FloorEntity entity);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Boolean existsByFlrName(String name);
}
