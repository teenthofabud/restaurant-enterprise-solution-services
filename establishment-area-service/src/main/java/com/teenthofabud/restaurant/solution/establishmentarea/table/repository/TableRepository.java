package com.teenthofabud.restaurant.solution.establishmentarea.table.repository;

import com.teenthofabud.core.common.repository.TOABSimpleEntityBaseRepository;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;

@Repository
public interface TableRepository extends TOABSimpleEntityBaseRepository<TableEntity> {

    @Lock(LockModeType.PESSIMISTIC_READ)
    public List<TableEntity> findByFloorFlrId(long floorId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public TableEntity save(TableEntity entity);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Boolean existsByTableNameAndFloorFlrId(String name, long floorId);
}
